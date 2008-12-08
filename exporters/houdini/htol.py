"""
Houdini to lucille RIB exporter.

  Written by Syoyo Fujita(syoyo@lucillerender.org)
"""

import os, sys
import re

ribnodelist = {}

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

    print "  ==> Exporining camera : ", cam.path()
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

def visit_object(object):

    print "[htol]  ==> Exporing ", object.path()

    if object.displayNode() is None:
        print "[htol] Skip object ", object
        return

    xform = object.parmTransform()
    geo = object.displayNode().geometry()
    assert geo is not None

    hasP = True     # This should be always true
    hasN = False    # Geometry has per vertex normal?


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
            print "[htol] Warn: [%s] is not a polygonal object, skipping export" % object.path()
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
                ribnodelist["camera"] = s
            else:
                s = visit_object(child)
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

