"""
Houdini to lucille RIB exporter.

  Written by Syoyo Fujita(syoyo@lucillerender.org)
"""

import os, sys
import re
import math

ribnodelist = {}

def vnormalize(v):

    d2 = v[0] * v[0] + v[1] * v[1] + v[2] * v[2]
    d = math.sqrt(d2)

    if (d > 1.0e-12):
        invd = 1.0 / d
        v[0] *= invd
        v[1] *= invd
        v[2] *= invd

def vcross(a, b):

    v = [0.0, 0.0, 0.0]
    
    v[0] = a[1] * b[2] - b[1] * a[2]
    v[1] = a[2] * b[0] - b[2] * a[0]
    v[2] = a[0] * b[1] - b[0] * a[1]

    return v

def ortho(n):

    w = [0.0, 0.0, 0.0]

    if (n[0] < 0.6 and n[0] > -0.6):
        w[0] = 1.0
    elif (n[1] < 0.6 and n[1] > -0.6):
        w[1] = 1.0
    elif (n[2] < 0.6 and n[2] > -0.6):
        w[2] = 1.0
    else:
        w[0] = 1.0

    u = vcross(w, n)
    vnormalize(u)

    v = vcross(n, u)
    vnormalize(v)

    return (u, v, n)
    

def matmul(m, x, y, z):

    w = [0.0, 0.0, 0.0]

    w[0] = m[0][0] * x + m[1][0] * y + m[2][0] * z 
    w[1] = m[0][1] * x + m[1][1] * y + m[2][1] * z 
    w[2] = m[0][2] * x + m[1][2] * y + m[2][2] * z 

    return w
    
#
# Create a tube geometry from control points.
#
def emit_fur_as_tube_polygon(prim):

    ndiv = 8
    n = prim.numVertices()

    if prim.type() != hou.primType.Polygon:
        return None

    if n < 2:
        return None

    s = ""

    s += "# seg = %d\n" % n
    s += "PointsPolygons "

    # Create ndiv * (n-1) faces(quads)
    # build face(quad polygon)
    s += " [ "
    for j in range(ndiv*(n-1)):
        s += "4 "
    s += " ] "

    tubes = []

    for i in range(n-1):

        v1    = prim.vertex(i+1).point().position()
        v0    = prim.vertex(i).point().position()
        width = 2.0 * prim.vertex(i).attribValue("width")

        axis = [ v1[0] - v0[0]
               , v1[1] - v0[1]
               , v1[2] - v0[2]
               ]

        vnormalize(axis)

        basis = ortho(axis)

        topPts    = []
        bottomPts = []

        for j in range(ndiv):

            x = width * math.cos( 2.0 * math.pi * (j / float(ndiv)) )
            y = width * math.sin( 2.0 * math.pi * (j / float(ndiv)) )
            z = 0.0     

            p = matmul(basis, x, y, z)
            bp = [ p[0] + v0[0]
                 , p[1] + v0[1]
                 , p[2] + v0[2]
                 ]
            bottomPts.append(bp)

            tp = [ p[0] + v1[0]
                 , p[1] + v1[1]
                 , p[2] + v1[2]
                 ]
            topPts.append(tp)

        tubes.append((bottomPts, topPts))
        

    # emit indices
    s += " [ "

    for i in range(n-1):
        offt = ndiv * i
        for j in range(ndiv-1):
            s += "%d %d %d %d " % (offt + j, offt + j + ndiv, offt + j + 1 + ndiv, offt + j + 1)
        s += "%d %d %d %d " % (offt + (ndiv-1), offt + (ndiv-1) + ndiv, offt + ndiv, offt)
        
    s += " ] "


    s += " \"P\" [ "

    # emit verts
    for tube in tubes:
        bp = tube[0]
        for p in bp:
            s += " %f %f %f " % (p[0], p[1], p[2])

        tp = tube[1]
        for p in tp:
            s += " %f %f %f " % (p[0], p[1], p[2])

    s += " ] "

    # print s

    return s

def emit_header():
    """
    Emits RIB header string
    """

    s  = ""
    s += "# RenderMan RIB-Structure 1.1\n"
    s += "# Exported by htol, Houdini to lucille RIB exporter by Syoyo Fujita\n"
    s += "Display \"untitled.hdr\" \"framebuffer\" \"rgb\"\n"

    return s


def emit_footer():
    """
    Emits RIB footer string
    """

    s  = ""
    s += "# RIB End\n"

    return s

def visit_camera(cam):

    print "[htol] ==> Exporining camera : ", cam.path()
    # print cam.path()

    """
    Camera matrix should be inverted, and should apply rh -> lh conversion.
    """

    xform = cam.parmTransform().inverted()
    #xform = cam.parmTransform()

    m = xform.asTuple()

    xm = []
    for i in range(16):
        xm.append(m[i])

    fov = 45.0  # FIXME: calculate fov from camera object.

    s  = ""
    s += "PixelSamples 2 2\n"
    s += "Shutter 0.0 1.0\n"
    s += "Projection \"perspective\" \"fov\" [" + str(fov) + "]\n"
    s += "Orientation \"rh\"\n"     # Houdini employs reft hand coord.
    s += "ConcatTransform ["

    for i in range(16):
        s += "%f " % xm[i]

    s += "]\n"

    # print s

    return s


def visit_fur(obj):

    #
    # Filter out non-geometry node.
    #
    if not hasattr(obj, "displayNode"):
        print "[htol]     Skipped object ", obj
        return

    if obj.displayNode() is None:
        print "[htol]     Skipped object ", obj
        return

    # obj node has parmTransform method?
    xform = None
    if hasattr(obj, "parmTransform"):
        xform = obj.parmTransform()
    else:
        print "[htol]     Skipped object ", obj
        return

    geo = obj.displayNode().geometry()
    assert geo is not None

    s = ""

    # emit xform
    m = xform.asTuple()
    s += "# Fur. prims = %d\n" % (len(geo.prims()))
    s += "AttributeBegin\n"
    s += "Transform [ "
    for i in range(16):
        s += "%f " % m[i]
    s += " ]\n"

    
    for (fid, prim) in enumerate(geo.prims()):

        if prim.type() == hou.primType.Mesh:
            continue    # skip

        if not isinstance(prim, hou.Polygon):
            print "[htol] Warn: [%s] is not a polygonal object, skipping export" % obj.path()
            return

        # Fur is represented as an control points + width
        ss = emit_fur_as_tube_polygon(prim) 

        if ss is not None:
            s += ss

    s += "\nAttributeEnd\n"
    return s

def visit_object(obj):

    print "[htol] ==> Exporing ", obj.path()

    #
    # Filter out non-geometry node.
    #
    if not hasattr(obj, "displayNode"):
        print "[htol]     Skipped object ", obj
        return None

    if obj.displayNode() is None:
        print "[htol]     Skipped object ", obj
        return None

    if not hasattr(obj.displayNode(), "geometry"):
        print "[htol]     Skipped object ", obj
        return None

    # obj node has parmTransform method?
    xform = None
    if hasattr(obj, "parmTransform"):
        xform = obj.parmTransform()
    else:
        print "[htol]     Skipped object ", obj
        return None

    geo = obj.displayNode().geometry()

    hasP = True     # This should be always true
    hasN = False    # Geometry has per vertex normal?

    if not hasattr(geo, "primAttribs"):
        print "[htol]     Skipped object ", obj
        return None

    #
    # Fur obj?
    #
    for primAttrib in geo.primAttribs():
        if primAttrib.name() == "furdensity":
            # Delegate the process to visit_fur()
            return visit_fur(obj)

    for attrib in geo.pointAttribs():
        # print "[DBG] ", (attrib.name(), attrib.dataType(), attrib.size())

        if attrib.name() == "N" and attrib.size() == 3:
            hasN = True


    s = ""

    # emit xform
    m = xform.asTuple()
    s += "AttributeBegin\n"
    s += "Transform [ "
    for i in range(16):
        s += "%f " % m[i]
    s += " ]\n"


    s += "PointsPolygons ["

    # emit face
    for (fid, prim) in enumerate(geo.prims()):

        if prim.type() == hou.primType.Mesh:
            continue    # skip

        if not isinstance(prim, hou.Polygon):
            print "[htol] Warn: [%s] is not a polygonal object, skipping export" % obj.path()
            return

        s += "%d " % prim.numVertices()

    s += " ] [ "

    # Extract polygonal primitives.
    polyPrims = [prim for prim in geo.prims() if prim.type() == hou.primType.Polygon]

    # emit face idx
    vsum = 0
    for (fid, prim) in enumerate(polyPrims):
        for vid in xrange(prim.numVertices()):    
            s += "%d " % (vsum + vid)
        vsum += prim.numVertices()

    s += "] \"P\" [ "

    # emit positions
    for (fid, prim) in enumerate(polyPrims):
        for vid in xrange(prim.numVertices()):    

            # get vertex 
            v = prim.vertex(vid)

            p = v.point().position()
            
            s += "%f %f %f " % (p[0], p[1], p[2])    # x,y and z

    s += "]"


    if hasN:
        # emit normals

        s += " \"N\" ["

        # emit positions
        for (fid, prim) in enumerate(polyPrims):
            for vid in xrange(prim.numVertices()):    

                # get vertex 
                v = prim.vertex(vid)

                n = v.point().attribValue("N")
                
                s += "%f %f %f " % (n[0], n[1], n[2])    # x,y and z

        s += "]"

    s += "\nAttributeEnd\n"

    # print s
    
    print "      Exported %d vertices." % vsum

    return s

    # for point in geo.points():
    #     print "[DBG] ", point.position()
    # print


def walk_tree(node, indent=0):
    """
    Recursively walk through houdini nodes and emit geometry data to RIB.
    """

    global ribnodelist

    cam_re = re.compile("cam\d")
    obj_re = re.compile("/obj/")

    s = ""

    for child in node.children():

        if obj_re.search(child.path()):

            # camera node?
            if (cam_re.match(child.name())):
                s = visit_camera(child)
                if s is not None:
                    ribnodelist["camera"] = s
            else:
                s = visit_object(child)
                if s is not None:
                    ribnodelist[child.name()] = s

        # print " " * indent + child.name() + " (" + str(type(child)) + ")"

        walk_tree(child, indent + 3)


def export(node, ribname):
    """
    Export geometry in the houdini world into RIB file.

      - node    : Root node to export the data. e.g. hou.node("/")
      - ribname : Name of the RIB file.
    """

    print "[htol] Exporing the scene..."

    f = open(ribname, "w")

    print >>f, emit_header()

    walk_tree(node)

    if not ribnodelist.has_key("camera"):
        print "[htol] No camera was found!"
    else:
        print >>f, ribnodelist["camera"]
        del ribnodelist["camera"]
    
    print >>f, "WorldBegin"

    for (k, v) in ribnodelist.items():
        print >>f, v

    print >>f, "WorldEnd"

    print >>f, emit_footer()

    f.close()

    print "[htol] Expored the scene"



# For testing the exporter...
if __name__ == '__main__':
    import hou
    
    ribfile = os.path.join(os.environ["HOME"], "untitled.rib")
    export(hou.node("/"), ribfile)

