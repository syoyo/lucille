#!BPY

# $Id: pantalone.py 276 2007-04-07 12:01:44Z lucille $

"""
Name: 'lucille(.rib)...'
Blender: 236
Group: 'Export'
Tooltip: 'Save as a RenderMan RIB file'
"""

__author__="muda"
__url__="http://lucille.atso-net.jp/wiki/index.php?Pantalone"
__version__="0.01"
__bpydoc__="""\
This is an another Blender -> RenderMan(especially for lucille) exporter.
"""

import os, sys, time;
import re;		# regular expression
import Blender;
from Blender.BGL import *
from Blender.Draw import *
from Blender.Noise import *

# --- global variables -----------------------------------
gFullpath = Blender.sys.makename(ext=".rib");
gFilename = os.path.basename(gFullpath);
#gDirname = os.path.dirname(gFullpath);
gDirname = "/Users/syoyo/data";

gLshDirname = ""
gLightNum = 0; # --------------------------------------------------------

if os.name == "nt": # windows
	gLshDirname = "C:\lucille"
else:	# assume mac os x or linux
	gLshDirname = "/Users/syoyo/work/lucille/src/lsh"


# --------------------------------------------------------
# GUI component
# --------------------------------------------------------

##########################################################
#GUI Created using RipSting's Blender-Python GUI designer#
#Download at Http://oregonstate.edu/~dennisa/Blender/BPG/#
##########################################################

# 3
TextboxFileName = Create(gFilename) # filename textbox
renderSizeMenu = Create(1)
# 1
TextboxLshDirName = Create(gLshDirname)    # lsh path textbox
# 2
TextboxDirName = Create(gDirname)  # output dir textbox

gRenderSize = 256;

def draw():
	global exitButton, TextboxFileName, renderSizeMenu, expButton, expRenderButton, TextboxLshDirName, lshPathSetButton, TextboxDirName, expDirButton

	glClearColor(0.753, 0.753, 0.753, 0.0)
	glClear(GL_COLOR_BUFFER_BIT)


	glColor3f(0.000, 0.000, 0.000)
	glRasterPos2i(16, 330)
	Text('filename')
	glColor3f(0.239, 0.239, 0.239)
	glRasterPos2i(16, 106)
	Text('blender to lucille(RIB) exporter')
	glColor3f(0.000, 0.000, 0.000)
	glRasterPos2i(16, 130)
	Text('pantalone')
	glRasterPos2i(16, 434)
	Text('Rendering size')
	glRasterPos2i(16, 266)
	Text('Path to lucille(lsh and rockenfield). :')
	glRasterPos2i(16, 394)
	Text('export directory :')


	Button('exit', 1, 288, 94, 47, 23, '')
	Button('Export file', 2, 16, 166, 111, 47, '')
	Button('Export and Render', 3, 224, 166, 111, 47, '')
	Button('set', 4, 304, 230, 31, 23, '')
	Button('set', 5, 304, 358, 31, 23, '')

	TextboxFileName = String('', 6, 16, 294, 287, 23, TextboxFileName.val, 380, '')
	TextboxLshDirName = String('', 7, 16, 230, 287, 23, TextboxLshDirName.val, 380, '')
	TextboxDirName = String('', 8, 16, 358, 287, 23, TextboxDirName.val, 380, '')


	renderSizeMenu = Menu('Menu%t|256x256 %x1|512x512 %x2|1024x1024 %x3', 9, 232, 422, 103, 23, renderSizeMenu.val, '')


def selectDirname(fname):

	global gFullpath;

	print "selectDirname = " + fname;

	dirname = os.path.dirname(fname);
	if not os.path.exists(dirname):
		Blender.Draw.PupMenu("directory [ %s ] does not exist!\t|OK" % dirname);
	else:
		if (fname.find(".rib") == -1):
			fname += ".rib";

		TextboxFileName.val = os.path.basename(fname); 
		TextboxDirName.val = os.path.dirname(fname);

		gFullpath = fname;

		Blender.Redraw();
	
def selectLshDirname(fname):

	print "selectLshDirname = " + fname;

	dirname = os.path.dirname(fname);

	if not os.path.exists(dirname):
		Blender.Draw.PupMenu("directory [ %s ] does not exist!\t|OK" % dirname);
	else:

		TextboxLshDirName.val = os.path.dirname(fname); 
		Blender.Redraw();


def event(evt, val):
	if (evt== QKEY and not val): Exit()
def bevent(evt):
	global gFullpath, gRenderSize;

	if evt == 1: #exitButton
		Exit()

	elif evt == 9: #renderSizeMenu
		if renderSizeMenu.val == 1: #First Item
			print "256x256 selected"
			gRenderSize = 256;
			InsertCodeHere = 1
		elif renderSizeMenu.val == 2: #Second Item
			print "512x512 selected"
			gRenderSize = 512;
			InsertCodeHere = 1
		elif renderSizeMenu.val == 3: #Third Item
			print "1kx1k selected"
			gRenderSize = 1024;
			InsertCodeHere = 1

	elif evt == 2: #expButton
		InsertCodeHere = 1
		if not os.path.exists(TextboxDirName.val):
			msg = "Directory [ %s ] does not exit. Please select existing directory." % TextboxDirName.val;

			Blender.Draw.PupMenu(msg + "%t|OK");
		else:
			gFullpath = TextboxDirName.val + os.sep + TextboxFileName.val;
			print "[pantalone] Exporting scene to [ %s ] ..." % gFullpath;

			exportRIB(gFullpath);

			#msg = "[pantalone] Export [ %s ] OK!" % gFullpath;
			#Blender.Draw.PupMenu(msg + "%t|OK");

	elif evt == 3: #expRenderButton

		if not os.path.exists(TextboxDirName.val):
			msg = "Directory [ %s ] does not exit. Please select existing directory." % TextboxDirName.val;

			Blender.Draw.PupMenu(msg + "%t|OK");
		else:
			gFullpath = TextboxDirName.val + os.sep + TextboxFileName.val;
			print "exporting scene to [ %s ] ..." % gFullpath;

			exportRIB(gFullpath);
			execLsh(TextboxLshDirName.val, gFullpath);
		
		
		InsertCodeHere = 1

	elif evt == 4: #lshPathSetButton
		InsertCodeHere = 1

		if os.name == "nt": # windows
			basename = "lsh.exe";
		else:	# mac os x or linux
			basename = "lsh";

		path = TextboxLshDirName.val + os.sep + basename;

		Blender.Window.FileSelector(selectLshDirname, 'Select the directory where lsh exists', path);
		


	elif evt == 5: #expDirButton
		InsertCodeHere = 1
		print gFullpath;
		Blender.Window.FileSelector(selectDirname, 'Select output directory', gFullpath);


	Blender.Redraw()

# --------------------------------------------------------
# exporter component
# --------------------------------------------------------

def execLsh(path, fname):
	lshExecName = "lsh";
	if os.name == "nt":	# windows
		lshExecName += ".exe";

	print "execLsh: path = %s, fname = %s\n" % (path, fname);

	if os.name == "nt":
		line = path + os.sep + lshExecName + " \"" + fname + "\"";
	else:
		line = path + os.sep + lshExecName + " " + fname;

	print "executing command [ %s ]\n" % line;
	os.system(line);
 
def writeHeader(ribfile):
	ribfile.write("##RenderMan RIB-Structure 1.0\n");
	ribfile.write("version 3.03\n");
	ribfile.write("# Exported by pantalone, another Blender -> lucille exporter scripts\n");
	ribfile.write("#   $HeadURL: http://lucille.atso-net.jp/svn/proj/lucille/branches/RB-0.2/tools/blender/pantalone.py $\n");
	ribfile.write("#   $Revision: 276 $\n");
	ribfile.write("Option \"renderer\" \"nthreads\" [1]\n");
	ribfile.write("Option \"raytrace\" \"arealight_rays\" [16]\n");
	ribfile.write("PixelSamples 1 1\n");

# }

# export polygon
def expPoly(mesh, ribfile):

	if (len(mesh.faces) == 0):
		return;

	# write list of vertex index 
	ribfile.write("PointsPolygons [ ");

	for face in mesh.faces:

		if len(face.v) == 4:
			ribfile.write("3 3 ");
		else:
			ribfile.write("%d " % len(face.v));

	ribfile.write("] [ ");

	for face in mesh.faces:
		num = len(face.v);

		# export only triangle or quad polygon.
		if num == 4:
			for i in (0, 1, 2, 0, 2, 3):
				ribfile.write("%d " % face.v[i].index);
		elif num == 3:
			for vert in face.v:
				ribfile.write("%d " % vert.index);

	ribfile.write("]\n");
	ribfile.write('"P" [ ');

	# write list of vertex positon.
	for vert in mesh.verts:
		ribfile.write("%f %f %f " % (vert.co[0], vert.co[1], vert.co[2]));
	ribfile.write("] ");

	# write list of vertex normal
	if mesh.faces[0].smooth:

		ribfile.write(' "N" [ ');

		for vert in mesh.verts:
			ribfile.write("%f %f %f " % (vert.no[0], vert.no[1], vert.no[2]));
		ribfile.write("] ");

	# write list of vertex color
	if mesh.hasVertexColours() == 1:

		vtcol = range(len(mesh.verts));

		ribfile.write(' "Cs" [ ');
		for face in mesh.faces:
			num = len(face.v);

			# treat only triangle or quad polygon.
			if num == 3 or num == 4:
				for i in range(len(face.v)):
					vtcol[face.v[i].index] = face.col[i];

		# for each vertex.
		for vc in vtcol:
			if hasattr(vc, "r"):
				ribfile.write("%f %f %f " % (vc.r / 256.0, vc.g / 256.0, vc.b / 256.0));
			

		ribfile.write(' ] ');

	# write list of UV
	if len(mesh.faces[0].uv) > 0:

		ribfile.write(' "st" [ ');

		vtuv = [];

		for i in range(len(mesh.verts)):
			# initialize with zero;
			vtuv.append((0, 0));

		# map fave UVs into vertex UVs.
		for f in mesh.faces:
			for i in range(len(f.uv)):
				uv = f.uv[i];
				# swap y axis.
				uv = uv[0], 1.0 - uv[1];

				vtuv[f.v[i].index] = uv;

		# for each vertex
		for c in vtuv:
			if (c == 0):
				# No uv is assigned to this vertex?
				ribfile.write("0.0 0.0 ");
			else:
				# for each component
				for d in c:
					ribfile.write("%f " % d);
		
		ribfile.write(' ] ');

	ribfile.write("\n");
			
	
def expCamera(ribfile):
	scene = Blender.Scene.GetCurrent();
	cam = scene.getCurrentCamera();
	
	invmat = cam.getInverseMatrix();

	ribfile.write("# Camera transformation\n");
	ribfile.write("Transform [ " +
		"%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s ]\n" %
		(invmat[0][0], invmat[0][1], -invmat[0][2], invmat[0][3],
		 invmat[1][0], invmat[1][1], -invmat[1][2], invmat[2][3],
		 invmat[2][0], invmat[2][1], -invmat[2][2], invmat[2][3],
		 invmat[3][0], invmat[3][1], -invmat[3][2], invmat[3][3]));

	
def expShader(ribfile):
	ribfile.write("Surface \"constant\"\n");
	ribfile.write("Color [ 1.0 1.0 1.0 ]\n");

def expSetting(ribfile):

	global gRenderSize;
	
	ribfile.write("Perspective 40.0\n");
	ribfile.write("Format %d %d 1\n" % (gRenderSize, gRenderSize));
	ribfile.write("Display \"muda.hdr\" \"file\" \"rgb\"\n");
	
# write objects transformation matrix.
def expTrans(name, ribfile):

	obj = Blender.Object.Get(name);
	mat = obj.getMatrix();

	ribfile.write("Transform [ %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f ]\n" %
		(mat[0][0], mat[0][1], mat[0][2], mat[0][3],
		 mat[1][0], mat[1][1], mat[1][2], mat[1][3],
		 mat[2][0], mat[2][1], mat[2][2], mat[2][3],
		 mat[3][0], mat[3][1], mat[3][2], mat[3][3]));

# write object's material
def expMaterial(name, ribfile):
	mesh = Blender.NMesh.GetRawFromObject(name);

	try:
		material = Blender.Material.Get(mesh.materials[0].name);
	except:
		return;

	# get material textures
	mtextures = material.getTextures();

	for mtex in mtextures:
		# only consider image(bitmap) texture
		if mtex is not None and mtex.tex.type == Blender.Texture.Types.IMAGE:
			# escape '\' and ' '. this is only for win32 platform.
			escname = re.sub(r'\\', r'\\\\', mtex.tex.image.filename);
			escname = re.sub(r' ', r'\\ ', escname);
			# write absolute path to texture image.
			print "teximage =  " + escname;

			ribfile.write("Surface \"constant\" \"texture\" [ \"%s\" ]\n" % escname);

	
# Export area light source geometry.
def expAreaLight(obj, ribfile):
	global gLightNum;

	mesh = Blender.NMesh.GetRawFromObject(obj.getName());

	gLightNum = gLightNum + 1;

	ribfile.write("AreaLightSource \"arealight\" %d\n" % gLightNum);
	expTrans(obj.getName(), ribfile);
	expPoly(mesh, ribfile);

def expScene(ribfile):

	# get scene root
	objs = Blender.Object.Get();

	# export rendering settings
	expSetting(ribfile);

	# export camera;
	expCamera(ribfile);

	ribfile.write("\nWorldBegin\n\n");

	expShader(ribfile);

	# foreach mesh in the scene.
	for obj in objs:

		data = obj.getData();

		# Polygon mesh.
		if (type(data) == Blender.Types.NMeshType):
				
			mesh = Blender.NMesh.GetRawFromObject(obj.getName());
			ribfile.write("AttributeBegin\n");
			ribfile.write("Attribute \"objectname\" \"name\" [ \"%s\" ]\n" % obj.getName());

			# when "(arealight)" string appears in the object name,
			# pantalone treat this object as the geometry of a
			# area light.
			if (re.compile("\(arealight\)").search(obj.getName()) != None):
				print "[pantalone]  Exporting arealight: " + obj.getName();
				# export as area light.
				expAreaLight(obj, ribfile);
			else:
				print "[pantalone]  Exporting geometry: " + obj.getName();
				expMaterial(obj.getName(), ribfile);
				expTrans(obj.getName(), ribfile);
				expPoly(mesh, ribfile);

			ribfile.write("AttributeEnd\n");

	ribfile.write("\nWorldEnd\n");

def exportRIB(fname):

	startTime = time.time();

	f = open(fname, "w");
	
	# write RIB header
	writeHeader(f);

	expScene(f);

	f.flush();
	f.close();

	endTime = time.time();
	elapsedTime = endTime - startTime;

	print "[pantalone] Export OK (Processed in %f seconds)" % elapsedTime;

# }

# -------------
# main function
# -------------

print "==================================================================";
print "pantalone: blender to lucille exporter";
print "[pantalone] fullpath = " +gFullpath;
print "[pantalone] dirname = " + os.path.dirname(gFullpath);
print "[pantalone] absfullpath = " + os.path.abspath(gFullpath);


Register(draw, event, bevent)

#filename = Blender.sys.makename(ext=".rib");
#Blender.Window.FileSelector(exportRIB, 'Export RIB', filename);


"""
 o 0.3

   Support for AreaLight.

 o 0.2 or earlier

   I forgot many changes...

"""
