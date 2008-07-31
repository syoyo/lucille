# RenderMan export script by Christoffer Green and Goran Kocov
# version 0.1 Beta9.0
# the script needs Blender 2.28a and Python 2.2 installed
# to run the script press alt+p in the text window

import Blender
import string
import os
import sys
from os import P_NOWAIT
from Blender.Draw import *
from Blender.BGL import *
from math import pi
from math import atan

try:
	import totiff
except:
	print "Did not find the totiff module, will use ImageMagic instead\n"


# MAKE SURE YOU CHANGE THIS TO YOUR PYTHON FOLDER
if (os.name != "posix"):
	sys.path = ['c:\\python22']

scene = Blender.Scene.GetCurrent()
camobj = scene.getCurrentCamera()
camera = Blender.Camera.Get(camobj.getData().name)

# The bevent constants
exportevent = 			1 # export button pressed
exitevent = 			2 # exit button pressed
updateguievent =		3 # update the pressed
filetoggleevent = 		999 # file button pressed

# GUI variables
fs = 				Create('')
menuval	=			Create(4)
dof =				Create(0)
samples	=			Create(2)
stop = 				Create(2.0)
lenght = 			Create(1.0)
distance = 			Create(5.0)
srate = 			Create(3)
file = 				Create(0)
buffer = 			Create(1)
filename = 			Create("default.rib")
image = 			Create("default")
dirw = 				Create("default")
prmanrenderer = 		Create("prman")
dlrenderer =	 		Create("renderdl")
bmrtrenderer = 			Create("rendrib")
entropyrenderer = 		Create("entropy")
aqsisrenderer = 		Create("aqsis")
prmantexmaker = 		Create("txmake")
dltexmaker =	 		Create("tdlmake")
bmrttexmaker = 			Create("mkmip")
entropytexmaker = 		Create("mkmip")
aqsistexmaker = 		Create("teqser")
radiosity = 			Create(0)
radnum = 			Create(50)
gi = 				Create(0)
gisteps = 			Create(100)
geom = 				Create(1)
meshexport = 			Create(1)
pointwidth = 			Create(1.0)
anim = 				Create(0)
maxpixeldist = 			Create(20)
maxerror = 			Create(0.25)
rshadow = 			Create(1)
smooth = 			Create(0.00)
areasamples = 			Create(20)
buff256 = 			Create(0)
buff512 = 			Create(1)
buff1024 = 			Create(0)
buff2048 = 			Create(0)
motionblur = 			Create(0)
motionblurframe = 		Create(1)
rendert =			Create(1)
caustics = 			Create(0)
photon =			Create(4000)
uv = 				Create(0)
recurs = 			Create(4)
debug = 			Create(0)
hiddenlayer =			Create(0)
globalsettings = 		Create(1)
scenesettings = 		Create(0)
shadereditor = 			Create(0)
surfaceshader = 		Create(3)
surfaceshadername = 		Create("")
about = 			Create(0)
shadermenu = 			Create(1)
ambientshader = 		Create(1.00)
diffuseshader = 		Create(0.50)
specularshader = 		Create(0.50)
roughnessshader = 		Create(0.10)
coefshader = 			Create(1.50)
reflblurshader = 		Create(0.00)
refrblurshader = 		Create(0.00)
objecteditor = 			Create(0)
convertbm = 			Create(1)
objectshadermenu = 		Create(0)
arealightMESH = 		Create(0)
recievecaustics = 		Create(0)
transmittcaustics = 		Create(0)
transmittcausticsslider = 	Create(1.40)
reflsamples = 			Create(0)
refrsamples = 			Create(0)
kr =	 			Create(1.0)
rendershadow = 			Create(0)
autogs = 			Create(0)
reflectcaustics = 		Create(0)
progress = 			Create(1)
gain =				Create(1.0)
gamma =				Create(1.0)
lightx =			Create(1.0)
bias =				Create(0.25)
raytraceshadow = 		Create(1)
projection =			Create(0)
textureblurm =			Create(0.0)
norm = 				Create(0.01)
converttex = 			Create(1)
gisphere =			Create(1)
rvisibility =			Create(0)
finalgather =			Create(0)
rgba =				Create(1)
castshadow =			Create(1)
reconvert =			Create(0)
texturem = 			Create('                                                                                                                                    ')
texturem.val = ''
textshader = 			Create('Surface "plastic"                                                                                                                                                                                                                                            ')
textshader.val = 		'Surface "plastic"'
textshaderd = 			Create('Displacement "bumpy"                                                                                                                                                                                                                                            ')
textshaderd.val = 		'Displacement "bumpy"'
hdritex =			Create('                                                                                                                                       ')
hdritex.val =			''
hdri =				Create(0)
preworldrafile =		Create('                                                                                                                                       ')
preworldrafile.val =		''
preworldra =			Create(0)
postworldrafile =		Create('                                                                                                                                       ')
postworldrafile.val =		''
postworldra =			Create(0)
objectrafile =			Create('                                                                                                                                       ')
objectrafile.val =		''
objectra =			Create(0)
textnor = 			Create(0)
illuminationmenu =		Create(0)
areagi =			Create(0)
indirectgi = 			Create(1)
eblur = 			Create(0.00)
esamples =			Create(1)
gianim = 			Create(1)
amesh = 			Create(0)
patchsize = 			Create(4.0)
elemsize = 			Create(2.0)
minsize = 			Create(1.0)

# Other variables
renderer = 			"PRMan |BMRT |Entropy |3Delight |AQSIS "
shader = 			"Plastic (All)|Glass (BMRT, Entropy)|Shiny (BMRT, Entropy) |Custom (All) |BMS (All) "
pixelsamples = 			"1 Preview|2 Good|3 Recomended|4|5 To High|6|7 For DoF|8|9|10 Best and Crazy"
shadingrate = 			"0.25 Best|0.5|1 Good|2|4 Preview|6|8|12|16 Bad"
sratelist = 			[0.25, 0.5, 1, 2, 4, 6, 8, 12, 16]
meshex = 			"PointsPolygons |Polygons |SubdivSurface |Points "
illumination =			"Plastic |Matte |RoughMetal |ThinPlastic |ShinyPlastic |ShinyMetal |Clay |BrushedMetal |Ceramic |Glass "
projectionm =			"Plane |Cylinder |Sphere |UV-Map |Auto"
sshader = 			"ADD NEW"
sshader2 = 			""
shadernametemp = 		""
selectedname = 			""
selectedtype = 			"Type: "
tmp =				""
imagelist = 			""
imagelistmenu =			"No images loaded"
rgbatext =			"rgb |rgba |rgbz |z "
castshadowtext =		"Os |opaque |shader |none"
txdone = 			[]

# Global variables
increment = 			0
xbar = 				11
i = 				0
meshobjects =			0
maxblur =			10.0
meshnum =			0

# Create the BlenderMan directories on startup
if (os.name != "posix"):
	rootdir =			'/BlenderMan/'
	partitionname = 		Blender.sys.progname[0:2]
else:
	rootdir =			os.getenv("HOME") + '/BlenderMan/'
pdir = 				rootdir + dirw.val + '/'
globalpropertiesdir = 		pdir + '/GlobalProperties/'
surfaceshadersdir = 		pdir + '/SurfaceShaders/'
displacementshadersdir = 	pdir + '/DisplacementShaders/'
objectsettingsdir = 		pdir + '/ObjectSettings/'
lightsettingsdir = 		pdir + '/LightSettings/'
meshesdir = 			os.path.normpath(pdir + '/Meshes/')
imagesdir = 			pdir + '/images/'
shadowdir = 			pdir + '/shadows/'
texturesdir = 			pdir + '/textures/'
shadersdir = 			pdir + '/shaders/'
try: os.mkdir(rootdir)
except OSError:	pass
try: os.mkdir(pdir)
except OSError: pass
try: os.mkdir(surfaceshadersdir)
except OSError:	pass
try: os.mkdir(globalpropertiesdir)
except OSError: pass
try: os.mkdir(displacementshadersdir)
except OSError: pass
try: os.mkdir(meshesdir)
except OSError:	pass
try: os.mkdir(objectsettingsdir)
except OSError: pass
try: os.mkdir(lightsettingsdir)
except OSError: pass
try: os.mkdir(shadowdir)
except OSError:	pass
try: os.mkdir(texturesdir)
except OSError:	pass
try: os.mkdir(imagesdir)
except OSError: pass

def gui():
	global menuval, dof, samples, stop, lenght, distance, srate, file, buffer, dirw
	global filename, image, radiosity, radnum, gi, gisteps, geom, meshexport, pointwidth
	global anim, maxpixeldist, maxerror, smooth, areasamples, buff256, buff512, buff1024, buff2048
	global motionblur, motionblurframe, rendert, caustics, photon, uv, recurs, debug, globalsettings, scenesettings
	global shadereditor, sshader, surfaceshader, surfaceshadername, about, shadermenu, ambientshader
	global diffuseshader, specularshader, roughnessshader, coefshader, reflblurshader, refrblurshader
	global objecteditor, selectedname, convertbm, objectshadermenu, arealightMESH, recievecaustics
	global transmittcaustics, transmittcausticsslider, reflsamples, refrsamples, rendershadow
	global xbar, autogs, kr, progress, indirectgi, areagi, eblur, esamples, gianim, amesh, reflectcaustics
	global patchsize, elemsize, minsize, textshader, raytraceshadow, bias, textshaderd, gain, gamma
	global lightx, textureblurm, texturem, norm, planarm, converttex, imagelistmenu
	global textnor, illuminationmenu, projection, hiddenlayer, gisphere, rvisibility, finalgather
	global hdritex, hdri, rgba, castshadow, reconvert, maxblur, sshader2
	global prmanrenderer, dlrenderer, bmrtrenderer, entropyrenderer, aqsisrenderer
	global prmantexmaker, dltexmaker, bmrttexmaker, entropytexmaker, aqsistexmaker
	global preworldra, preworldrafile, postworldra, postworldrafile, objectra, objectrafile

	if (autogs.val == 1): #automaticly select objects
		Redraw(1)
		getSelectedObject()
		saveload(0,0,'object')

	# GUI settings
	glClearColor(0.4,0.48,0.57, 0.0) 	# blue background
	glClear(GL_COLOR_BUFFER_BIT)
	glColor3f(0, 0, 0) 			# main black back
	glRectf(2, 2, 630, 340)
	glColor3f(0.4, 0.48, 0.57) 		# main blue back
	glRectf(4, 4, 628, 300)
	glColor3f(0.27, 0.3, 0.35) 		# blue top
	glRectf(4, 302, 628, 338)
	glColor3f(1,1,1)

	# Buttons for the main BlenderMan panels
	globalsettings = Toggle("Global Settings", 						12,  75,  310, 100, 20, globalsettings.val, "Global project settings")
	scenesettings = Toggle("Scene Settings", 						557, 175, 310, 100, 20, scenesettings.val, "Global scene settings")
	shadereditor = Toggle("Shader Editor", 							13,  275, 310, 100, 20, shadereditor.val, "Surface and diplacement shader settings")
	objecteditor = Toggle("Object Editor", 							33,  375, 310, 100, 20, objecteditor.val, "Edit object settings that Blender doesn't support")
	about = Toggle("About", 								16,  475, 310, 100, 20, about.val, "Information about this script")

	# Global Settings
	if (globalsettings.val == 1):
		debug = Slider("Debug Level ",	 						updateguievent, 440, 270, 180, 20, debug.val, 0, 3, 0, "How much debug information the renderer should output")
		hiddenlayer = Number("Hidden Layer:", 						0, 10, 270, 120, 20, hiddenlayer.val, 0, 20, "Objects that are present on this layer won't be exported")
		progress = Toggle("Progressbar",   						updateguievent, 520, 30, 100, 20, progress.val, "Hint: It takes slightly less time to export a scene with the progressbar off")
		gain = Slider("Gain ",								0, 225, 270, 180, 20, gain.val, 0.0, 5.0, 0)
		gamma = Slider("Gamma ",							0, 225, 248, 180, 20, gamma.val, 0.0, 5.0, 0)
		lightx = Slider("Light* ",							0, 225, 226, 180, 20, lightx.val, 0.0, 15.0, 0, "Light intensity multiplier for lamps (pointlights) and spotlights")
		if (menuval.val != 5):
			recurs = Slider("RaySteps ",						0, 440, 248, 180, 20, recurs.val, 0, 15, 0, "Maximum number of steps for the raytracing recursion")
		reconvert = Toggle("Reconvert Textures",					updateguievent, 440, 226, 180, 20, reconvert.val, "Reconvert UV textures")
		rgba = Menu(rgbatext, 								updateguievent, 370, 150, 90, 20, rgba.val, "What channels to export (A = Alpha. Z = Depth)")
		menuval = Menu(renderer, 							updateguievent, 370, 125, 90, 20, menuval.val, "Which renderer should be taken into account when exporting the scene and displaying the GUI")
		samples = Menu(pixelsamples, 							updateguievent, 370, 75, 90, 20, samples.val, "Silhouette, motion blur and depth of field quality")
		srate = Menu(shadingrate, 							updateguievent, 370, 100, 90, 20, srate.val, "Shading quality")
		filename = String("Filename: ", 						0, 150, 75, 140, 19, filename.val, 200, "Name of the master RIB file")
		image = String("Image: ", 							0, 150, 100, 140, 19, image.val, 200, "Name of the rendered image(s)")
		dirw = String("Project: ", 							500, 150, 125, 140, 19, dirw.val, 200, "Name of the project")
		if menuval.val == 1:
			prmanrenderer = String("Renderer: ", 					0, 480, 125, 140, 19, prmanrenderer.val, 200, "Name of the renderer executable")
			prmantexmaker = String("Tex. Maker: ", 					0, 480, 100, 140, 19, prmantexmaker.val, 200, "Name of the mipmap texture making executable")
		elif menuval.val == 2:
			bmrtrenderer = String("Renderer: ", 					0, 480, 125, 140, 19, bmrtrenderer.val, 200, "Name of the renderer executable")
			bmrttexmaker = String("Tex. Maker: ", 					0, 480, 100, 140, 19, bmrttexmaker.val, 200, "Name of the mipmap texture making executable")
		elif menuval.val == 3:
			entropyrenderer = String("Renderer: ", 					0, 480, 125, 140, 19, entropyrenderer.val, 200, "Name of the renderer executable")
			entropytexmaker = String("Tex. Maker: ", 				0, 480, 100, 140, 19, entropytexmaker.val, 200, "Name of the mipmap texture making executable")
		elif menuval.val == 4:
			dlrenderer = String("Renderer: ", 					0, 480, 125, 140, 19, dlrenderer.val, 200, "Name of the renderer executable")
			dltexmaker = String("Tex. Maker: ", 					0, 480, 100, 140, 19, dltexmaker.val, 200, "Name of the mipmap texture making executable")
		elif menuval.val == 5:
			aqsisrenderer = String("Renderer: ", 					0, 480, 125, 140, 19, aqsisrenderer.val, 200, "Name of the renderer executable")
			aqsistexmaker = String("Tex. Maker: ", 					0, 480, 100, 140, 19, aqsistexmaker.val, 200, "Name of the mipmap texture making executable")
		anim = Toggle("Animation",   							updateguievent, 150, 30, 69, 19, anim.val, "Render single or multiple frames")
		rendert = Toggle("Render",   							updateguievent, 220, 30, 69, 19, rendert.val, "Start the renderer after the export is done")
		file = Toggle("File",   							filetoggleevent, 150, 50, 70, 20, file.val, "Render the image to a file")
		buffer = Toggle("Buffer", 							5, 220, 50, 70, 20, buffer.val, "Render the image to the framebuffer")
		Button("Exit",   								exitevent, 385, 30, 75, 20, "Exit the script")
		Button("Export", 								exportevent, 310, 30, 74, 20, "Start the export")
		Button("Load Sett", 								30, 310, 51, 74, 20, "Load global and scene settings")
		Button("Save Sett", 								31, 385, 51, 75, 20, "Save global and scene settings")
		glRasterPos2i(310, 80)
		Text("Samples")
		glRasterPos2i(310, 105)
		Text("ShRate")
		glRasterPos2i(310, 130)
		Text("Renderer")
		glRasterPos2i(310, 155)
		Text("Channels")
		if (progress.val == 1):
			# Progressbar text
			glRasterPos2i(10, 25)
			Text("Mesh: %s/%s"%(int(meshnum), int(meshobjects)))
##			if anim.val:
##				glRasterPos2i(10, 40)
##				display = Blender.Scene.GetCurrent()
##				Text("Frame: %s/%s"%(display.currentFrame(), display.endFrame() - display.startFrame() + 1))
			# Progressbar backplate
			glColor3f(0, 0, 0)
			glRectf(9, 10, 620, 21)
			glColor3f(0.78, 0.7, 0.8)
			glRectf(10, 9, 621, 20)
			glColor3f(0.4, 0.4, 0.4)
			glRectf(10, 10, 620, 20)
			# Progressbar
			glColor3f(0, 0, 0)
			glRectf(10, 10, xbar + 1, 20)
			glColor3f(0.78, 0.7, 0.8)
			glRectf(10, 11, xbar, 20)
			glColor3f(0.52, 0.25, 0.45)
			glRectf(11, 11, xbar, 19)
	
	# Scene settings
	if (scenesettings.val == 1):
		preworldra = Toggle("PreWorld ReadArchive", 					updateguievent, 10, 60, 180, 20, preworldra.val, "Add a ReadArchive to a user-defined file just before WorldBegin")
		if (preworldra.val == 1):
			preworldrafile = String("File: ", 					0,  190,  60, 430, 20, preworldrafile.val, 100)
		postworldra = Toggle("PostWorld ReadArchive", 					updateguievent, 10, 40, 180, 20, postworldra.val, "Add a ReadArchive to a user-defined file just after WorldBegin")
		if (postworldra.val == 1):
			postworldrafile = String("File: ", 					0,  190,  40, 430, 20, postworldrafile.val, 100)
		if (menuval.val != 2):
			dof = Toggle("Depth Of Field",    					updateguievent, 440, 270, 180, 20, dof.val, "Depth of field of the camera")
			if (dof.val == 1):
				stop = Slider("Stop ", 						0, 440, 250, 180, 20, stop.val, 0, 100, 0, "Focal Stop. The camera's aperture")
				lenght = Slider("Lenght ",  					0, 440, 230, 180, 20, lenght.val, 0, 10, 0, "Focal Lenght. Lenght of the camera's lens")
				distance = Slider("Distance ",					0, 440, 210, 180, 20, distance.val, 0, 20, 0, "Focal Distance. Distance at which the camera is focused")
			motionblur = Toggle("Motion Blur", 					updateguievent, 225, 170, 180, 20, motionblur.val, "Apply blur to objects in motion")
			if (motionblur.val == 1):
				motionblurframe = Slider("Frames ",  				0, 225, 150, 180, 20, motionblurframe.val, 1, 10, 0, "How much blur to apply based on how many frames to take into account")
		if (menuval.val == 3):
			hdri = Toggle("HDRI", 							updateguievent, 225, 120, 180, 20, hdri.val, "Sets up a sphere that emits light filtered by a HDRI image")
			if (hdri.val == 1):
				hdritex = String("HDRI Texture: ", 				0,  225,  100, 280, 20, hdritex.val, 100, "Flie name of the HDRI image")
		if ((menuval.val == 2) or (menuval.val == 3)):
			gi = Toggle("Global Illumination", 					6, 10, 270, 180, 20, gi.val, "Uses advanced algorithms for realstic lighting")
			if (gi.val == 1):
				indirectgi = Toggle("Indirect", 				475, 10, 250, 90, 20, indirectgi.val, "Calculate GI from light bouncing between objects")
				areagi = Toggle("Area", 					476, 100, 250, 90, 20, areagi.val, "Sets up a big sphere that generates skydome lighting")
				gisteps = Slider("Steps ",					0, 10, 230, 180, 20, gisteps.val, 0, 600, 0, "Controls the detail of the solution")
				if (indirectgi.val == 1):
					if (finalgather.val == 0):
						maxerror = Slider("MaxError ",			0, 10, 210, 180, 20, maxerror.val,     0.01, 0.50, 0, "Smaller numbers cause recomputation to happen more often")
						maxpixeldist = Slider("MPixelDis ",		0, 10, 190, 180, 20, maxpixeldist.val, 0.00, 40.00, 0, "Smaller numbers cause recomputation to happen more often")
					gianim = Toggle("Animate GI", 				updateguievent, 10, 170, 180, 20, gianim.val, "Recompute GI for every frame in the animation")
					gisphere = Toggle("GI Sphere", 				0, 10, 150, 180, 20, gisphere.val, "Sets up a big sphere that generates skydome lighting")					
					if (menuval.val == 3):
						finalgather = Toggle("Final Gather", 		updateguievent, 10, 130, 180, 20, finalgather.val, "More precise, but slower form of GI")					
			if (menuval.val == 2):
				radiosity = Toggle("Radiosity", 					7, 225, 270, 180, 20, radiosity.val, "Calculate GI from light bouncing between objects")
				if (radiosity.val == 1):
					radnum = Slider("Steps ",					0, 225, 250, 180, 20, radnum.val, 0, 1000, 0, "Number of stepes for the radiosity calculation")
					patchsize = Slider("Patchsize",					0, 225, 230, 180, 20, patchsize.val, 0.0, 10.0, 0, "Smaller numbers cause recomputation to happen more often")
					elemsize = Slider("Elemsize ",					0, 225, 210, 180, 20, elemsize.val, 0.0, 6.0, 0, "Smaller numbers cause recomputation to happen more often")
					minsize = Slider("Minsize ",					0, 225, 190, 180, 20, minsize.val, 0.0, 3, 0, "Smaller numbers cause recomputation to happen more often")
	# Shader editor
	if (shadereditor.val == 1):
		surfaceshader = Menu(sshader, 							14, 10,  270, 20,  20, surfaceshader.val, "Choose shader")
		if (surfaceshader.val != 0):
			surfaceshadername = String("SH: ", 					17, 30,  270, 140, 20, surfaceshadername.val, 200)
			Button("X",   								15, 171, 270, 20,  20, "Delete shader")
		if surfaceshadername.val:
			Button("Test Render Shader",   						19,  220, 230, 150,  20)
			Button("Set default values",   						340, 380, 230, 150,  20)
			shadermenu = Menu(shader, 						32,  10,  230, 200,  20, shadermenu.val)
			if (shadermenu.val != 4):
				ambientshader = Slider("Ambient (Ka) ",	  			32,  10,  200, 300, 20, ambientshader.val,	0.00, 1.00, 0)
				diffuseshader = Slider("Diffuse (Kd) ",	   			32,  10,  175, 300, 20, diffuseshader.val,	0.00, 1.00, 0)
				specularshader = Slider("Specular (Ks) ",			32,  10,  150, 300, 20, specularshader.val,	0.00, 1.00, 0)
				roughnessshader = Slider("Roughness ",		   		32,  10,  125, 300, 20, roughnessshader.val,	0.00, 1.00, 0)
			if (shadermenu.val == 2):
				coefshader = Slider("Coefficient (eta)",			32,  10,  100, 300, 20, coefshader.val,		0.00, 3.00, 0)
				reflblurshader = Slider("Reflect Blur (blur)", 			32,  10,  75, 300, 20, reflblurshader.val, 	0.00, 1.00, 0)
				refrblurshader = Slider("Refract Blur (refrblur)", 		32,  10,  50, 300, 20, refrblurshader.val, 	0.00, 1.00, 0)
				reflsamples = Slider("Reflect Samples ", 			32,  315,  200, 300, 20, reflsamples.val, 	0, 30, 0)
				refrsamples = Slider("Refract Samples ",			32,  315,  175, 300, 20, refrsamples.val, 	0, 30, 0)
				kr = Slider("Kr ",						32,  315,  150, 300, 20, kr.val, 	  	0.0, 2.0, 0)
			if (shadermenu.val == 3):
				reflblurshader = Slider("Reflect Blur (blur)", 			32,  10,  100, 300, 20, reflblurshader.val, 	0.00, 1.00, 0)
				reflsamples = Slider("Reflect Samples ", 			32,  10,  75, 300, 20, reflsamples.val, 	0, 30, 0)
			elif (shadermenu.val == 4):
				glRasterPos2i(10, 200)
				Text('Surface Shader')
				textshader = String("", 					32,  10,  175, 600, 20, textshader.val, 100)
				glRasterPos2i(10, 150)
				Text('Displacement Shader')
				textshaderd = String("", 					32,  10,  125, 600, 20, textshaderd.val, 100)
				norm = Slider("Disp. Bound ", 					32,  10,  90, 300, 20, norm.val, 0.0, 10.0, 0, "Sets the radius of a sphere bounding the object. Larger displacements need biger values")
			elif (shadermenu.val == 5):
				projection = Menu(projectionm,					32, 315,  200, 180,  20, projection.val)
				texturem = String("Texture: ", 					32,  315,  180, 280, 20, texturem.val, 100)
				converttex = Menu(imagelistmenu,				368, 595,  180, 20,  20, converttex.val)
				Button("Convert and Reload",					367, 495, 200, 120,  20)
				textureblurm = Slider("Texture Blur ", 				32,  315,  160, 300, 20, textureblurm.val, 	0.0, 1.0, 0)
				norm = Slider("Bump Height ", 					32,  315,  140, 300, 20, norm.val, 	        0.0, 10.0, 0)
				textnor = Toggle("Nor", 					32,  315, 120, 60, 20, textnor.val)
				illuminationmenu = Menu(illumination,				32, 375,  120, 120,  20, illuminationmenu.val)

	# Object editor
	if (objecteditor.val == 1):
		autogs = Toggle("Auto getSelected (drains cpu)",				updateguievent,  10, 10, 180,   20, autogs.val)
		if (autogs.val == 0):
			Button("Get Selected",   						34, 10, 30, 180,  20)
		glRasterPos2i(200, 35)
		Text('Name: ' + selectedname)
		glRasterPos2i(200, 15)
		Text(selectedtype)
		if (selectedtype == 'Type: Mesh'):
			objectra = Toggle("Object ReadArchive", 				18, 10, 60, 180, 20, objectra.val, "Add a ReadArchive to a user-defined file just after object's AttributeBegin")
			if (objectra.val == 1):
				objectrafile = String("File: ", 				18,  190,  60, 430, 20, objectrafile.val, 100)
			convertbm = Toggle("Use Shader",		 	  		18, 10, 220, 180, 20, convertbm.val)
			if (convertbm.val == 1):
				namelist = string.split(sshader, "|")
				namelist.remove('ADD NEW')
				namelist = map(str, namelist)
				sshader2 = string.join(namelist, "|")
				if (len(sshader2) == 0):
					glRasterPos2i(10, 205)
					Text("No shaders available.")
				else:
					objectshadermenu = Menu(sshader2, 			18,  10,  200, 180,  20, objectshadermenu.val)
			geom = Toggle("Export Geometry", 					18, 230, 270, 180, 20, geom.val, "Output the mesh geometry to a RIB file")
			if (geom.val == 1):
				meshexport = Menu(meshex, 					18, 230, 250, 180, 20, meshexport.val)
				amesh = Toggle("Animated Mesh", 				18, 230, 230, 120, 20, amesh.val, "Turn this on if the mesh deforms in time")
				if (meshexport.val != 4):
					uv = Toggle("ExportUV", 				18, 350, 230, 60, 20, uv.val, "Exports and converts the UV map attached to the object (if there is one)")
				else:
				    pointwidth = Slider("Width ", 				18, 230, 180, 180, 20, pointwidth.val, 0.0, 10.0, 0, "Radius of the Points primitive")
			if (menuval.val == 2) or (menuval.val == 3):
				rvisibility = Toggle("Hide Source",   					18, 230, 200, 180, 20, rvisibility.val, "Hides the object from primary rays")
			if ((menuval.val == 2) or (menuval.val == 3)):
				arealightMESH = Toggle("AreaLight",   				18, 10, 270, 180, 20, arealightMESH.val, "Use the object as an area light")
				if (arealightMESH.val == 1):
					areasamples = Slider("AreaSamp ",	    		18, 10, 250, 180, 20, areasamples.val, 0, 400, 0, "Controls the graininess of the area light")
				recievecaustics = Toggle("Recieve Caustics",   			18, 440, 270, 180, 20, recievecaustics.val)
				reflectcaustics = Toggle("Reflect Caustics",	   		18, 440, 250, 180, 20, reflectcaustics.val)
				transmittcaustics = Toggle("Transmitt Caustics",	   	18, 440, 230, 180, 20, transmittcaustics.val)
				if (transmittcaustics.val == 1):
					transmittcausticsslider = Slider("IOR ",		18, 440, 210, 180, 20, transmittcausticsslider.val, 0.00, 2.20, 0, "Index of refraction")
			if (menuval.val != 5):
				glRasterPos2i(10, 165)
				Text('Raytraced Shadows')
				castshadow = Menu(castshadowtext, 				18,  10,  140, 180,  20, castshadow.val, "Os uses the object's opacity value for the shadow opacity, shader uses the shader opacity calculations, opaque produces black shadows and none turns shadows off")
		if (selectedtype == 'Type: Lamp'):
			if ((menuval.val == 2) or (menuval.val == 3)):
				caustics = Toggle("Caustics", 					18, 230, 270, 180, 20, caustics.val, "Turn on photon emission used for caustics calculations")
			if ((caustics.val == 1) & ((menuval.val == 2) or (menuval.val == 3))):
				photon = Slider("Photons ",					18, 230, 250, 180, 20, photon.val, 4000, 80000, 0, "Number of photons emitted")
			lampobj = Blender.Object.Get(selectedname)
			lamp = Blender.Lamp.Get(lampobj.getData().name)
			if (lamp.getMode() & lamp.Modes['Shadows']) and (lamp.type != 3):
				if (menuval.val != 5):
					raytraceshadow = Toggle("Ray Shadow",			245, 100,  270, 90, 20, raytraceshadow.val, "Use raytraced shadows")
				if lamp.type == 2:
					rendershadow = Toggle("Ren Shadow",			244, 10,  270, 90, 20, rendershadow.val, "Use shadowmaps")
				prmanshadowbuttonsposition = 270
				if (rendershadow.val == 1):
					bias = Slider("Bias ",					18, 10, 210, 180, 20, bias.val, 0.0, 1.0, 0, "Shadowmap bias")
				if (rendershadow.val == 1):
					buff256 = Toggle("256", 				8,  10,  190, 42, 20, buff256.val, "Shadowmap resolution")
					buff512 = Toggle("512", 				9,  52,  190, 42, 20, buff512.val, "Shadowmap resolution")
					buff1024 = Toggle("1024", 				10, 95,  190, 48, 20, buff1024.val, "Shadowmap resolution")
					buff2048 = Toggle("2048", 				11, 143, 190, 48, 20, buff2048.val, "Shadowmap resolution")
			if (lamp.getMode() & lamp.Modes['Shadows']) and (lamp.type != 3):
				if ((((menuval.val == 1) or (menuval.val == 3)) and (raytraceshadow.val == 1)) or (rendershadow.val == 1)):
					eblur = Slider("Blur ",					18, 10, 250, 180, 20, eblur.val, 0.0, maxblur, 0, "Shadow blur")
					esamples = Slider("Samples ",				18, 10, 230, 180, 20, esamples.val, 0, 256, 0, "Blur quality")

	# About dialog
	if (about.val == 1):
		glColor3f(0,0,0)
		glRasterPos2i(10, 279)
		Text("BlenderMan 0.1 Beta 9.0")
		glRasterPos2i(10, 259)
		Text("by Christoffer Green and Goran Kocov")
		glColor3f(1,1,1)
		glRasterPos2i(9, 280)
		Text("BlenderMan 0.1 Beta 9.0")
		glRasterPos2i(9, 260)
		Text("by Christoffer Green and Goran Kocov")
		glRasterPos2i(10, 230)
		Text("_______________________________________________________________________________________")
		glRasterPos2i(10, 200)
		Text("Thanks to sgefant for the name suggestion")
		glRasterPos2i(10, 180)
		Text("Thanks to Phillipe Crassous for helping me fix the spotlight rotation bug and other code improvements")
		glRasterPos2i(10, 160)
		Text("Thanks to desaster and #python for syntax help")
		glRasterPos2i(10, 140)
		Text("Thanks to K-Rich, Macke, Kib_Tph, sgefant and #Blender3d for pre-beta testing")
		glRasterPos2i(10, 120)
		Text("Thanks to Jan Walter for the basic exporting code")
		glRasterPos2i(10, 100)
		Text("Thanks to svo for the pointspolygon code")
		glRasterPos2i(10, 80)
		Text("Thanks to nishin for alpha/beta-testing")
		glRasterPos2i(10, 60)
		Text("Thanks to eeshlo for the totiff module")
		glRasterPos2i(10, 40)
		Text("Thanks to DetectiveThorn for the help with the light intensity conversion")

def event(evt, val):
	if (evt == ESCKEY and not val): 
		Exit()
	if (evt == ACCENTGRAVEKEY and not val): 
		export()

def bevent(evt):
	global sshader, increment, pdir, maxblur, imagelist, imagelistmenu, imagelisttif, shadernametemp, selectedname
	if (evt == 500):					# Change project
		newdirs()
		surfaceshadername.val = ""
		sshader = "ADD NEW"
		try: 
			f = open(surfaceshadersdir + 'ShaderNames','r')
			saveShaderName(0)
		except:
			saveShaderName(1)
		resetObject()
		saveload(0, 0, 'object')
		saveload(0, 0, 'global')
		Register (gui, event, bevent)
	if (evt == exportevent):				# Export button
		export()
	if (evt == exitevent): 					# Exit button
		Exit()
	if (evt == updateguievent):				# Update GUI
		Register (gui, event, bevent)
	if (evt == filetoggleevent):				# File toggle
		file.val = 1
		buffer.val = 0
		Register (gui, event, bevent)
	if (evt == 5):						# Buffer toggle
		file.val = 0
		buffer.val = 1
		Register (gui, event, bevent)
	if (evt == 6):						# Global Illumination toggle
		radiosity.val = 0
		Register (gui, event, bevent)
	if (evt == 7):						# Radiosity toggle
		gi.val = 0
		Register (gui, event, bevent)
	if (evt == 8):						# Shadow buffer buttons
		if (buff256.val == 1):
			buff512.val = 0
			buff1024.val = 0
			buff2048.val = 0
		saveload(1,0,'object')
		Register (gui, event, bevent)
	if (evt == 9):						# Shadow buffer buttons
		if (buff512.val == 1):
			buff256.val = 0
			buff1024.val = 0
			buff2048.val = 0
		saveload(1,0,'object')
		Register (gui, event, bevent)
	if (evt == 10):						# Shadow buffer buttons
		if (buff1024.val == 1):
			buff512.val = 0
			buff256.val = 0
			buff2048.val = 0
		saveload(1,0,'object')
		Register (gui, event, bevent)
	if (evt == 11):						# Shadow buffer buttons
		if (buff2048.val == 1):
			buff512.val = 0
			buff1024.val = 0
			buff256.val = 0
		saveload(1,0,'object')
		Register (gui, event, bevent)
	if (evt == 12):
		globalsettings.val = 1
		scenesettings.val = 0
		shadereditor.val = 0
		objecteditor.val = 0
		about.val = 0
		Register (gui, event, bevent)
	if (evt == 557):
		globalsettings.val = 0
		scenesettings.val = 1
		shadereditor.val = 0
		objecteditor.val = 0
		about.val = 0
		Register (gui, event, bevent)
	if (evt == 13):
		globalsettings.val = 0
		scenesettings.val = 0
		shadereditor.val = 1
		objecteditor.val = 0
		about.val = 0
		Register (gui, event, bevent)
	if (evt == 16):
		globalsettings.val = 0
		scenesettings.val = 0
		shadereditor.val = 0
		objecteditor.val = 0
		about.val = 1
		Register (gui, event, bevent)
	if (evt == 33):
		globalsettings.val = 0
		scenesettings.val = 0
		shadereditor.val = 0
		objecteditor.val = 1
		about.val = 0
		Register (gui, event, bevent)
	if (evt == 14):						# Add shader menu
		namelist = string.split(sshader, "|") # the names of the shaders gets hacked up into a list
		surfaceshadername.val = namelist[surfaceshader.val - 1] # change the label of the shader in the shadereditor to the one selected
		listlenght = len(namelist) # length of the list
		if (surfaceshader.val == listlenght): # if the length of the list of shader names is the same as the surfaceshader menu add another item to the menu
			increment = increment +1
			name = 0
			numbername = "%03d" % (int(increment))
			while (name == 0):
				name = 1
				for x in namelist[:]:
					if ('Surface.%s'%numbername == x):
						increment = increment +1
						name = 0
						numbername = "%03d" % (int(increment))
			namelist.insert(listlenght - 1 , 'Surface.%s'%numbername)
			sshader = string.join(namelist, "|")
			surfaceshadername.val = namelist[surfaceshader.val - 1]
			saveShaderName(1)
			resetShaderVal()
			shadermenu.val = 1
		else:
			saveload(0,0,'shader')
		saveShaderName(1)
		shadernametemp = surfaceshadername.val
		Register (gui, event, bevent)
	if (evt == 340):
		resetShaderVal()
		saveload(1,0,'shader')
		Register (gui, event, bevent)
	if ((evt == 15) & (surfaceshadername.val != "")):				# Surface shader delete
		deleteshader = surfaceshader.val
		tempselected = selectedname
		namelist = string.split(sshader, "|")
		namelist.remove(surfaceshadername.val)
		surfaceshadername.val = namelist[surfaceshader.val - 1]
		namelist = map(str, namelist)
		sshader = string.join(namelist, "|")
		listlenght = len(namelist)
		if (surfaceshader.val == listlenght):
			surfaceshadername.val = namelist[surfaceshader.val - 2]
		surfaceshadername.val = ""
		surfaceshader.val = surfaceshader.val - 1
		saveShaderName(1)
		saveload(0,0,'shader')
		for objects in Blender.Object.Get():
			if (objects.getType() == "Mesh"):
				resetObject()
				selectedname = objects.name
				saveload(0,0,'object')
				if deleteshader == objectshadermenu.val:
					convertbm.val = 0
				elif deleteshader < objectshadermenu.val:
					objectshadermenu.val = objectshadermenu.val - 1
				saveload(1,0,'object')
		selectedname = tempselected
		saveload(0,0,'object')
		Register (gui, event, bevent)
	if (evt == 17):									# Surface shader name edit
		if (len(surfaceshadername.val) != 0): 					# this if thing fixes a bug that crashes the app when renaming a shader without a name. ("" = "")
			namelist = string.split(sshader, "|")
			namelist[surfaceshader.val - 1]	= surfaceshadername.val
			sshader = string.join(namelist, "|")
			saveload(1,0,'shader')
			saveShaderName(1)
			Register (gui, event, bevent)	
	if (evt == 18):
		saveload(1,0,'object')
		Register (gui, event, bevent)
	if (evt == 19):
		testrender = open('%s/testrender.rib'%rootdir, 'w')
		testrender.write('Projection "perspective" "fov" 40\n')
		testrender.write('Format 320 240 1\n')
		if (rgba.val == 1):
			channels = "rgb"
		elif (rgba.val == 2):
			channels = "rgba"
		elif (rgba.val == 3):
			channels = "rgbz"
		elif (rgba.val == 4):
			channels = "z"
		testrender.write('Display "Test Render" "framebuffer" "%s"\n'%channels)
		testrender.write('Option "searchpath" "texture" ["%s"]\n'%texturesdir)
		testrender.write('Exposure 1 1.2\n')
		testrender.write('PixelSamples 3 3\n')
		testrender.write('Translate 0 0 5\n')
		testrender.write('Rotate -120 1 0 0\n')
		testrender.write('Rotate 25 0 0 1\n')
		testrender.write('WorldBegin\n')
		if (menuval.val == 1) or (menuval.val == 4):
			testrender.write('\tAttribute "visibility" "integer trace" [1]\n')
		if (menuval.val == 3):
			testrender.write('\tAttribute "visibility" "reflection" [1]\n')
			testrender.write('\tAttribute "visibility" "shadow" [1]\n')
		if (menuval.val == 1) or (menuval.val == 4):
			testrender.write('\tAttribute "visibility" "string transmission" ["shader"]\n')
		elif (menuval.val == 2) or (menuval.val == 3):
			testrender.write('\tAttribute "render" "string casts_shadows" ["shade"]\n')
		if (menuval.val == 2) or (menuval.val == 4) or (menuval.val == 5):
			if (menuval.val != 5):
				testrender.write('\tAttribute "light" "shadows" ["on"]\n')
			testrender.write('\tLightSource "spotlight" 1 "intensity" 20 "from" [0 3 4] "to" [0 0 0]\n')
			testrender.write('\tLightSource "spotlight" 2 "intensity" 20 "from" [0 -3 4] "to" [0 0 0]\n')
		elif (menuval.val == 3):
			testrender.write('\tLightSource "spotlight" 1 "intensity" 10 "from" [0 3 4] "to" [0 0 0] "string shadowname" ["shadow"] "float shadowsamples" [1]\n')
			testrender.write('\tLightSource "spotlight" 2 "intensity" 10 "from" [0 -3 4] "to" [0 0 0] "string shadowname" ["shadow"] "float shadowsamples" [1]\n')
		elif (menuval.val == 1):
			testrender.write('\tLightSource "shadowspot" 1 "intensity" 10 "from" [0 3 4] "to" [0 0 0] "string shadowname" ["shadow"] "float samples" [1]\n')
			testrender.write('\tLightSource "shadowspot" 2 "intensity" 10 "from" [0 -3 4] "to" [0 0 0] "string shadowname" ["shadow"] "float samples" [1]\n')
		testrender.write('\tAttributeBegin\n')
		name = 'foobar'
		meshobj = 10
		testrender.write('\t\tAttribute "displacementbound" "string coordinatesystem" ["object"] "float sphere" [1]\n')
		writeShader(testrender, name, meshobj, 0)
		testrender.write('\t\tSphere 1 -1 1 360\n')
		testrender.write('\tAttributeEnd\n')
		testrender.write('\tAttributeBegin\n')
		if ((menuval.val == 2) or (menuval.val == 3)):
			testrender.write('\tSurface "oakplank"\n')
		elif (menuval.val == 4) or (menuval.val == 1):
			testrender.write('\tSurface "rmarble"\n')
		elif (menuval.val == 5):
			testrender.write('\tSurface "matte"\n')
		testrender.write('\tPointsPolygons [4 ]  [0 3 2 1 ]\n')
		testrender.write('\"P" [10.0 10.0 -1.0 10.0 -10.0 -1.0 -10.0 -10.0 -1.0 -10.0 10.0 -1.0 ]\n')
		testrender.write('\tAttributeEnd\n')
		testrender.write('WorldEnd\n')
		testrender.close()
		render('%s/testrender.rib'%rootdir)
	if (evt == 20):							# Load settings
		loadSettings()
		Register (gui, event, bevent)
	if (evt == 21):							# Save settings
		saveSettings()
		Register (gui, event, bevent)
	if (evt == 30):
		saveload(0, 0, 'global')
		Register (gui, event, bevent)
	if (evt == 31):
		saveload(1, 0, 'global')
		Register (gui, event, bevent)
	if (evt == 32):
		saveload(1,0,'shader')
		Register (gui, event, bevent)
	if (evt == 34):
		worked = getSelectedObject()
		if (worked == 1):
			resetObject()
			Register (gui, event, bevent)
			saveload(0,0,'object')
		Register (gui, event, bevent)
	if (evt == 244):
		rendershadow.val = 1
		raytraceshadow.val = 0
		saveload(1,0,'object')
		if (eblur.val > 0.2):
			eblur.val = 0.2
		maxblur = 0.2
		Register (gui, event, bevent)
	if (evt == 245):
		rendershadow.val = 0
		raytraceshadow.val = 1
		saveload(1,0,'object')
		maxblur = 10.0
		Register (gui, event, bevent)
	if (evt == 475):
		indirectgi.val = 1
		areagi.val = 0
		Register (gui, event, bevent)
	if (evt == 476):
		indirectgi.val = 0
		areagi.val = 1
		Register (gui, event, bevent)
	if (evt == 367): 						# Convert textures
		imagelist = os.listdir(texturesdir) 
		if (imagelist != []):
			for image in imagelist:
				rawimage = '%s/%s'%(texturesdir, image)
				totiff.SetOutDir(texturesdir)
				ok = totiff.ConvertShort(rawimage)
				if not ok[0]: # error occurred
					print ok[1] # print error message
			imagelist = os.listdir(texturesdir) 
			imagelisttif = []
			for image in imagelist:
				if (image[-3:] == 'tif'):
					imagelisttif = imagelisttif + [image]
			imagelistmenu = string.join(imagelisttif, "|")
		Register (gui, event, bevent)
	if (evt == 368):
		imagelist = os.listdir(texturesdir) 
		if (imagelist != []):
			imagelisttif = []
			for image in imagelist:
				if (image[-3:] == 'tif'):
					imagelisttif = imagelisttif + [image]
			imagelistmenu = string.join(imagelisttif, "|")
			texturem.val = str(imagelisttif[converttex.val - 1])
			saveload(1,0,'shader')
		Register (gui, event, bevent)
Register (gui, event, bevent)

def getSelectedObject():
	global selectedname, selectedtype, namelist, sshader2, sshader
	try:
		selected = Blender.Object.GetSelected()[0]
	except:
		return 0
	selectedname = selected.name
	if (selected.getType() == "Mesh"):
		selectedtype = 'Type: Mesh'
	elif (selected.getType() == "Lamp"):
		selectedtype = 'Type: Lamp'
	else:
		selectedtype = 'Type: Other'
	namelist = string.split(sshader, "|")
	namelist.remove('ADD NEW')
	namelist = map(str, namelist)
	sshader2 = string.join(namelist, "|")
	return 1
	
def saveload(save, exportS, module):
	if (module == 'shader'):
		Variables = [shadermenu.val, ambientshader.val, diffuseshader.val, specularshader.val, roughnessshader.val, coefshader.val, 
			reflblurshader.val, refrblurshader.val, reflsamples.val, refrsamples.val, kr.val, textshader.val, textshaderd.val, texturem.val,
			textureblurm.val, norm.val, textnor.val, illuminationmenu.val, projection.val]
		if exportS:
			namelist = string.split(sshader, "|")
			name = namelist[objectshadermenu.val - 1]
		else: 
			name = surfaceshadername.val
		if (name == ''):
			return
		fname = surfaceshadersdir + name
	if (module == 'object'):
		Variables = [convertbm.val, objectshadermenu.val, geom.val, meshexport.val, uv.val, arealightMESH.val, 
			areasamples.val, recievecaustics.val, transmittcaustics.val, transmittcausticsslider.val, caustics.val, photon.val, 
			rendershadow.val, buff256.val, buff512.val, buff1024.val, buff2048.val, eblur.val, esamples.val, amesh.val, 
			reflectcaustics.val, raytraceshadow.val, bias.val, rvisibility.val, castshadow.val, pointwidth.val, 
			objectra.val, objectrafile.val]
		fname = objectsettingsdir + selectedname
		if (selectedname == ''): 
			return
	if (module == 'global'):
		Variables = [gi.val, gisteps.val, radiosity.val, radnum.val, dirw.val, image.val, filename.val, file.val, buffer.val, 
			anim.val, rendert.val, menuval.val, samples.val, srate.val, debug.val, maxerror.val, maxpixeldist.val, geom.val, 
			motionblur.val, motionblurframe.val, dof.val, stop.val, lenght.val, distance.val, autogs.val, progress.val, 
			indirectgi.val, areagi.val, gianim.val, patchsize.val, elemsize.val, minsize.val, recurs.val, gain.val, gamma.val, 
			lightx.val, hiddenlayer.val, gisphere.val, finalgather.val, hdri.val, hdritex.val, rgba.val, reconvert.val, 
			prmanrenderer.val, bmrtrenderer.val, entropyrenderer.val, dlrenderer.val, aqsisrenderer.val,
			prmantexmaker.val, bmrttexmaker.val, entropytexmaker.val, dltexmaker.val, aqsistexmaker.val, 
			preworldra.val, preworldrafile.val, postworldra.val, postworldrafile.val]
		fname = globalpropertiesdir + 'GlobalProperties'
	# Save variables
	if (save == 1):
		f = open(fname,'w')
		writeln(f,'BlenderMan0.1Beta9.0')
		for i in Variables:
			writeln(f,type(i).__name__)
			if (i == '\n'):
				writeln(f,'')
				continue
			writeln(f,i)

	# Load variables
	if (save == 0):
		try:
			f = open(fname,'r')
		except:	
			return
		index = 0
		version = str(f.readline())
		if (version != 'BlenderMan0.1Beta9.0\n'):
			return
		for i in Variables:
			varType = str(f.readline())
			if (varType == "str\n"):
				Variables[index] = readstr(f)
			elif (varType == "float\n"):
				Variables[index] = float(f.readline())
			elif (varType == "int\n"):
				Variables[index] = int(f.readline())
			index += 1
	f.close()
	if (module == 'shader'):
		shadermenu.val 			= Variables[0]
		ambientshader.val 		= Variables[1]
		diffuseshader.val 		= Variables[2]
		specularshader.val 		= Variables[3]
		roughnessshader.val 		= Variables[4]
		coefshader.val 			= Variables[5]
		reflblurshader.val 		= Variables[6]
		refrblurshader.val 		= Variables[7]
		reflsamples.val	 		= Variables[8]
		refrsamples.val 		= Variables[9]
		kr.val 				= Variables[10]
		textshader.val 			= Variables[11]
		textshaderd.val 		= Variables[12]
		texturem.val 			= Variables[13]
		textureblurm.val 		= Variables[14]
		norm.val 			= Variables[15]
		textnor.val 			= Variables[16]
		illuminationmenu.val 		= Variables[17]
		projection.val 			= Variables[18]
	elif (module == 'object'):
		convertbm.val 			= Variables[0]
		objectshadermenu.val 		= Variables[1]
		geom.val 			= Variables[2]
		meshexport.val 			= Variables[3]
		uv.val 				= Variables[4]
		arealightMESH.val	 	= Variables[5]
		areasamples.val 		= Variables[6]
		recievecaustics.val	 	= Variables[7]
		transmittcaustics.val 		= Variables[8]
		transmittcausticsslider.val 	= Variables[9]
		caustics.val 			= Variables[10]
		photon.val 			= Variables[11]
		rendershadow.val 		= Variables[12]
		buff256.val 			= Variables[13]
		buff512.val 			= Variables[14]
		buff1024.val 			= Variables[15]
		buff2048.val 			= Variables[16]
		eblur.val 			= Variables[17]
		esamples.val 			= Variables[18]
		amesh.val 			= Variables[19]
		reflectcaustics.val 		= Variables[20]
		raytraceshadow.val  		= Variables[21]
		bias.val 			= Variables[22]
		rvisibility.val 		= Variables[23]
		castshadow.val			= Variables[24]
		pointwidth.val			= Variables[25]
		objectra.val			= Variables[26]
		objectrafile.val		= Variables[27]
	elif (module == 'global'):
		gi.val 				= Variables[0]
		gisteps.val 			= Variables[1]
		radiosity.val 			= Variables[2]
		radnum.val 			= Variables[3]
		dirw.val 			= Variables[4]
		image.val 			= Variables[5]
		filename.val 			= Variables[6]
		file.val 			= Variables[7]
		buffer.val 			= Variables[8]
		anim.val 			= Variables[9]
		rendert.val 			= Variables[10]
		menuval.val 			= Variables[11]
		samples.val 			= Variables[12]
		srate.val 			= Variables[13]
		debug.val 			= Variables[14]
		maxerror.val 			= Variables[15]
		maxpixeldist.val 		= Variables[16]
		geom.val 			= Variables[17]
		motionblur.val 			= Variables[18]
		motionblurframe.val 		= Variables[19]
		dof.val 			= Variables[20]
		stop.val 			= Variables[21]
		lenght.val 			= Variables[22]
		distance.val 			= Variables[23]
		autogs.val 			= Variables[24]
		progress.val 			= Variables[25]
		indirectgi.val 			= Variables[26]
		areagi.val 			= Variables[27]
		gianim.val 			= Variables[28]
		patchsize.val 			= Variables[29]
		elemsize.val 			= Variables[30]
		minsize.val 			= Variables[31]
		recurs.val 			= Variables[32]
		gain.val 			= Variables[33]
		gamma.val 			= Variables[34]
		lightx.val 			= Variables[35]
		hiddenlayer.val 		= Variables[36]
		gisphere.val 			= Variables[37]
		finalgather.val 		= Variables[38]
		hdri.val			= Variables[39]
		hdritex.val			= Variables[40]
		rgba.val 			= Variables[41]
		reconvert.val			= Variables[42]
		prmanrenderer.val		= Variables[43]
		bmrtrenderer.val		= Variables[44]
		entropyrenderer.val		= Variables[45]
		dlrenderer.val			= Variables[46]
		aqsisrenderer.val		= Variables[47]
		prmantexmaker.val		= Variables[48]
		bmrttexmaker.val		= Variables[49]
		entropytexmaker.val		= Variables[50]
		dltexmaker.val			= Variables[51]
		aqsistexmaker.val		= Variables[52]
		preworldra.val			= Variables[53]
		preworldrafile.val		= Variables[54]		
		postworldra.val			= Variables[55]
		postworldrafile.val		= Variables[56]		
		
def writeln(f,x):
	try:
		if (type(x).__name__ == 'string'): # this thing is to combat a bug that saves strings with a \n, (we dont want that)
			nn = string.find(x, '\n')
			if (nn > 0):
				x = x[:nn] + x[nn + 1:]
		f.write(str(x))
		f.write('\n')
	except:
		pass

def readint(f):
	try:
		return int(f.readline())
	except:
		pass
def readfloat(f):
	try:
		return float(f.readline())
	except:
		pass
def readstr(f):
	try:
		s = (f.readline())
		nn = string.find(s, '\n')
		if (nn > 0):
			s = s[:nn] + s[nn + 1:]
		return s
	except:
		print s

def saveShaderName(save):
	global sshader
	if save: f = open(surfaceshadersdir + 'ShaderNames','w')
	else: f = open(surfaceshadersdir + 'ShaderNames','r')
	if save: writeln(f,sshader)
	else:	sshader = readstr(f)
	f.close()
try: 
	f = open(surfaceshadersdir + 'ShaderNames','r')
except:
	saveShaderName(1)
	Register (gui, event, bevent)

saveShaderName(0)
Register (gui, event, bevent)
sshader2 = sshader


def newdirs():
	global pdir, globalpropertiesdir, surfaceshadersdir, displacementshadersdir, objectsettingsdir
	global lightsettingsdir, lightsettingsdir, meshesdir, imagesdir, shadowdir, texturesdir, shadersdir
	if (os.name != "posix"):
		rootdir = '/BlenderMan/'
	else:
		rootdir = os.getenv("HOME") + '/BlenderMan/'
	pdir = rootdir + dirw.val + '/'
	globalpropertiesdir = pdir + '/GlobalProperties/'
	surfaceshadersdir = pdir + '/SurfaceShaders/'
	displacementshadersdir = pdir + '/DisplacementShaders/'
	objectsettingsdir = pdir + '/ObjectSettings/'
	lightsettingsdir = pdir + '/LightSettings/'
	meshesdir = pdir + '/Meshes/'
	imagesdir = pdir + '/images/'
	shadowdir = pdir + '/shadows/'
	texturesdir = pdir + '/textures/'
	shadersdir = pdir + '/shaders/'
	try:
		os.mkdir(pdir)
		os.mkdir(globalpropertiesdir)
		os.mkdir(surfaceshadersdir)
		os.mkdir(displacementshadersdir)
		os.mkdir(meshesdir)
		os.mkdir(objectsettingsdir)
		os.mkdir(lightsettingsdir)
		os.mkdir(shadowdir)
		os.mkdir(texturesdir)
		os.mkdir(imagesdir)
	except OSError:	
		pass

def writeHeader(ribfile, frames, frames2):
	ribfile.write('Option "searchpath" "texture" ["%s"]\n'%(texturesdir + ":" + shadowdir))
	display = Blender.Scene.GetCurrent()
	resolution = display.getWinSize()
	yResolution = resolution[1]
	xResolution = resolution[0]
	if xResolution >= yResolution:
		factor = yResolution / float(xResolution)
	else:
		factor = xResolution / float(yResolution)
	if Blender.World.Get() != []:
		world = Blender.World.Get()[0]
		if world.hor != [0, 0, 0]:
			ribfile.write('Imager "background" "color bgcolor" [%s %s %s]\n'%(world.hor[0], world.hor[1], world.hor[2]))
	scene = Blender.Scene.GetCurrent()
	camobj = scene.getCurrentCamera()
        camera = Blender.Camera.Get(camobj.getData().name)
        ribfile.write('Projection "perspective" "fov" [%s]\n'%(360.0 * atan(factor * 16.0 / camera.lens) /pi))
	ribfile.write('Format %s %s 1\n' % (xResolution, yResolution))
	ribfile.write("Clipping %s %s\n" % (camera.clipStart, camera.clipEnd))
	ribfile.write('PixelSamples %s %s\n'%(samples.val, samples.val))
	try:
		ribfile.write('Exposure %s %s\n'%(gain.val, gamma.val))
	except: 
		pass
	if (menuval.val != 2):
		if (dof.val == 1): 
			ribfile.write('DepthOfField %s %s %s\n'%(stop.val, lenght.val, distance.val))
	ribfile.write('ShadingRate %s\n'%sratelist[srate.val - 1])
	if ((radiosity.val == 1) & (menuval.val == 2)):
		ribfile.write('Option "radiosity" "steps" [%s]\n'%radnum.val)
		ribfile.write('Attribute "indirect" "maxerror" [%s]\n'%maxerror.val)
		ribfile.write('Attribute "indirect" "float maxpixeldist" [%s]\n'%maxpixeldist.val)
		ribfile.write('Attribute "radiosity" "float patchsize" [%s]'%patchsize.val)
		ribfile.write('Attribute "radiosity" "float elemsize" [%s]'%elemsize.val)
		ribfile.write('Attribute "radiosity" "float minsize" [%s]'%minsize.val)
	if (motionblur.val == 1):
		ribfile.write('Shutter 0 1\n')
	if (menuval.val == 1) or (menuval.val == 4):
		ribfile.write('Option "trace" "integer maxdepth" [%s]\n'%recurs.val)
	elif (menuval.val == 2) or (menuval.val == 3):
		ribfile.write('Option "render" "max_raylevel" [%s]\n'%recurs.val)
	ribfile.write('Option "statistics" "integer endofframe" [%s]\n'%debug.val)
	if ((indirectgi.val == 1) & (gi.val == 1)):
		if ((gianim.val == 0) & (frames2 == 0)):
			ribfile.write('Option "indirect" "string savefile" ["%s/irr.dat"]\n'%pdir)
		elif ((frames2 > 0) or (gianim.val == 1)):
			if (anim.val == 1):
				ribfile.write('Option "indirect" "string seedfile" ["%s/irr.dat"]\n'%pdir)
		if (menuval.val == 3):
			ribfile.write('Option "indirect" "integer maxbounce" [3]\n')


def writeTransform(ribfile, lampobj):
	global cframe
	Blender.Get('curframe')
	Blender.Window.RedrawAll()
	if (lampobj == 0):
		camobj = scene.getCurrentCamera()
	else:
		camobj = lampobj
	caminvmatrix = camobj.getInverseMatrix()
	ribfile.write("Transform [" +
			"%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s]\n" %
			(caminvmatrix[0][0],
			caminvmatrix[0][1],
			-caminvmatrix[0][2],
			caminvmatrix[0][3],
			caminvmatrix[1][0],
			caminvmatrix[1][1],
			-caminvmatrix[1][2],
			caminvmatrix[1][3],
			caminvmatrix[2][0],
			caminvmatrix[2][1],
			-caminvmatrix[2][2],
			caminvmatrix[2][3],
			caminvmatrix[3][0],
			caminvmatrix[3][1],
			-caminvmatrix[3][2],
			caminvmatrix[3][3]))

def ambientLight(ribfile):
	if Blender.World.Get() != []:
		world = Blender.World.Get()[0]
		ribfile.write('\tLightSource "ambientlight" 998 "float intensity" [1] "color lightcolor" [%s %s %s]\n\n'%(world.amb[0], world.amb[1], world.amb[2]))

def checkLamps(ribfile):
	global selectedname
	lampnum = 0
	for objects in Blender.Object.Get():
		name = objects.getName()
		if (objects.getType() == "Lamp"):
			lampnum += 1
			resetObject()
			selectedname = name
			Register (gui, event, bevent)
			saveload(0,0,'object')
			Register (gui, event, bevent)
			writeLamps(ribfile, name, lampnum)
			ribfile.write("\n")

def renderShadows(ribfile, frames2):
	global selectedname, partitionname
	display = Blender.Scene.GetCurrent()
	cframe = Blender.Get('curframe')
	for objects in Blender.Object.Get():
		name = objects.getName()
		if (objects.getType() == "Lamp"):
			resetObject()
			Register (gui, event, bevent)
			selectedname = name
			saveload(0,0,'object')
			Register (gui, event, bevent)
			if (rendershadow.val == 1):
				if (anim.val == 1):
					shadowframe = Blender.Get('curframe')
				else:
					shadowframe = ''
				ribfiledir = os.path.normpath(shadowdir + name + str(shadowframe))
				shadowrib = open(ribfiledir, 'w')
				if ((menuval.val == 4) & (os.name != "posix")):
					ribfile.write('ReadArchive "%s%s"\n'%(os.path.splitdrive( os.path.normcase( os.path.normpath(partitionname + shadowdir + name + str(shadowframe))))))
				else:
					ribfile.write('ReadArchive "%s"\n'%(shadowdir + name + str(shadowframe)))
				shadowrib.write('FrameBegin 0\n')
				lampobj = Blender.Object.Get(name)
				lamp = Blender.Lamp.Get(lampobj.getData().name)
				if ((lamp.getMode() & lamp.Modes['Shadows']) & (lamp.type == 2)): # if it is set to render shadow and is a spotlight
					if (buff256.val == 1):
						buffer = 256
					elif (buff512.val == 1):
						buffer = 512
					elif (buff1024.val == 1):
						buffer = 1024
					elif (buff2048.val == 1):
						buffer = 2048
					shadowrib.write('Projection "perspective" "fov" %s\n' %(lamp.spotSize))
					shadowrib.write('PixelSamples 1 1\n')
					if (menuval.val != 2) and (menuval.val != 3):
						shadowrib.write('Hider "hidden" "depthfilter" "midpoint"\n')
					if (menuval.val == 3):
						shadowrib.write('PixelFilter "min" 1 1\n')
					else:
						shadowrib.write('PixelFilter "box" 1 1\n')
					shadowrib.write('Hider "hidden" "jitter" [0]\n')
					writeTransform(shadowrib, lampobj)
					shadowrib.write('Format %s %s 1\n'%(buffer, buffer))
					shadowrib.write('Display "%s/%s%s.pic" "zfile" "z"\n'%(shadowdir, name, shadowframe))
					if menuval.val == 5:
						shadowrib.write('ShadingRate 1\n')
					else:
						shadowrib.write('ShadingRate 4\n')
					shadowrib.write('WorldBegin \n')
					shadows = 1
					createObjects(shadowrib, cframe, shadows, frames2)
					shadowrib.write('WorldEnd \n')
					shadowrib.write('MakeShadow "%s/%s%s.pic" "%s/%s%s.tx"\n'%(shadowdir, name, shadowframe, shadowdir, name, shadowframe))
				shadowrib.write('FrameEnd\n')

def writeLamps(ribfile, name, lampnum):
	global selectedname
	lampobj = Blender.Object.Get(name)
	lamp = Blender.Lamp.Get(lampobj.getData().name)
	x = lampobj.matrix[3][0] / lampobj.matrix[3][3]
	y = lampobj.matrix[3][1] / lampobj.matrix[3][3]
	z = lampobj.matrix[3][2] / lampobj.matrix[3][3]
	tox = -lampobj.matrix[2][0] + lampobj.matrix[3][0]
	toy = -lampobj.matrix[2][1] + lampobj.matrix[3][1]
	toz = -lampobj.matrix[2][2] + lampobj.matrix[3][2]
	if lamp.getMode() & lamp.Modes['Negative']:
		negative = -1
	else:
		negative = 1
	selectedname = name
	Register (gui, event, bevent)
	saveload(0,0,'object')
	if (caustics.val):
		ribfile.write('\tLightSource "caustic" 2\n')
		ribfile.write('\tIlluminate 2 0\n\n')
		ribfile.write('\tTransformBegin\n')
		ribfile.write('\tAttribute "light" "integer nphotons" [%s]\n'%photon.val)
	if lamp.getMode() & lamp.Modes['Shadows']:
		shadow = 'on'
	else:
		shadow = 'off'
	if (menuval.val == 2) or (menuval.val == 4):
		if (raytraceshadow.val == 1):
			ribfile.write('\tAttribute "light" "shadows" ["%s"]\n' % (shadow))
		else:
			ribfile.write('\tAttribute "light" "shadows" ["off"]\n')
	if (lamp.type == 2):
		if (anim.val == 1):
			shadowframe = '%s'%(Blender.Get('curframe'))
		else:
			shadowframe = ''
		energratio = lamp.dist * negative
		if ((shadow == 'on') and (raytraceshadow.val == 1)):
			if (menuval.val == 3):
				shadowname = 'shadow'
			elif (menuval.val == 1):
				shadowname = 'raytrace'
			else:
				shadowname = ''
		else:
			if (shadow == 'on') and (rendershadow.val == 1):
				shadowname = name + shadowframe + '.tx'
			else:
				shadowname = ''
		ribfile.write('\tLightSource "bml" %d "float shadowbias" %s "float blur" %s "float samples" %s "coneangle" %s "conedeltaangle" %s "from" [%s %s %s] "to" [%s %s %s] "intensity" %s "lightcolor" [%s %s %s] "string shadowname" ["%s"]\n' \
			 %(lampnum, bias.val, eblur.val, esamples.val, (lamp.spotSize * pi / 360), (lamp.spotBlend * (lamp.spotSize * pi / 360)), x, y, z, tox, toy, toz, (energratio * lamp.energy) * lightx.val, lamp.R, lamp.G, lamp.B, shadowname))
	elif (lamp.type == 1):
		energratio = negative
		if (shadow == "off") or (raytraceshadow.val != 1) or (menuval.val == 2) or (menuval.val == 4) or (menuval.val == 5):
			ribfile.write('\tLightSource "distantlight" %d "from" [%s %s %s] "to" [%s %s %s] "intensity" %s "lightcolor" [%s %s %s]\n' % (lampnum, x, y, z, tox, toy, toz, energratio * lamp.energy, lamp.R, lamp.G, lamp.B))
		else:
			if (menuval.val == 3):
				ribfile.write('\tLightSource "distantlight" %d "from" [%s %s %s] "to" [%s %s %s] "intensity" %s "lightcolor" [%s %s %s] "string shadowname" ["shadow"] "float shadowsamples" [%s] "float shadowblur" [%s]\n' % (lampnum, x, y, z, tox, toy, toz, energratio * lamp.energy, lamp.R, lamp.G, lamp.B, esamples.val, eblur.val))
			elif (menuval.val == 1):
				ribfile.write('\tLightSource "shadowdistant" %d "from" [%s %s %s] "to" [%s %s %s] "intensity" %s "lightcolor" [%s %s %s] "string shadowname" ["raytrace"] "float samples" [%s] "float width" [%s]\n' % (lampnum, x, y, z, tox, toy, toz, energratio * lamp.energy, lamp.R, lamp.G, lamp.B, esamples.val, eblur.val * 100 + 1))
	elif (lamp.type == 0):
		energratio = lamp.dist * negative
		if (shadow == "off") or (raytraceshadow.val != 1) or (menuval.val == 2) or (menuval.val == 4) or (menuval.val == 5):
			ribfile.write('\tLightSource "pointlight" %d "from" [%s %s %s] "intensity" %s "lightcolor" [%s %s %s]\n'%(lampnum, x, y, z, (energratio * lamp.energy) * lightx.val, lamp.R, lamp.G, lamp.B))
		else:
			if (menuval.val  == 3):		
				ribfile.write('\tLightSource "pointlight" %d "from" [%s %s %s] "intensity" %s "lightcolor" [%s %s %s] "float shadowblur" [%s] "float shadowsamples" [%s] "string shadowname" ["shadow"]\n'%(lampnum, x, y, z, (energratio * lamp.energy) * lightx.val, lamp.R, lamp.G, lamp.B, eblur.val, esamples.val))
			elif (menuval.val  == 1):		
				ribfile.write('\tLightSource "shadowpoint" %d "from" [%s %s %s] "intensity" %s "lightcolor" [%s %s %s] "float width" [%s] "float samples" [%s] "string sfpx" ["raytrace"] "string sfnx" ["raytrace"] "string sfpy" ["raytrace"] "string sfny" ["raytrace"] "string sfpz" ["raytrace"] "string sfnz" ["raytrace"]\n'%(lampnum, x, y, z, (energratio * lamp.energy) * lightx.val, lamp.R, lamp.G, lamp.B, eblur.val * 100 + 1, esamples.val))
	elif (lamp.type == 3):
		energratio = negative
		ribfile.write('\tLightSource "hemilight" %d "from" [%s %s %s] "to" [%s %s %s] "intensity" %s "lightcolor" [%s %s %s] "float falloff" [0]\n' % (lampnum, x, y, z, tox, toy, toz, energratio * lamp.energy, lamp.R, lamp.G, lamp.B))
	if (caustics.val == 1):
		ribfile.write('\tTransformEnd\n')

def writeMatrix(ribfile, name):
	Blender.Get('curframe')
	Blender.Window.RedrawAll()
	matrix = Blender.Object.Get(name).matrix
	ribfile.write("\t\tTransform [" +
		"%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s]\n" %
		(matrix[0][0], matrix[0][1],
		matrix[0][2], matrix[0][3],
		matrix[1][0], matrix[1][1],
		matrix[1][2], matrix[1][3],
		matrix[2][0], matrix[2][1],
		matrix[2][2], matrix[2][3],
		matrix[3][0], matrix[3][1],
		matrix[3][2], matrix[3][3]))

def resetObject():
	convertbm.val = 0
	objectshadermenu.val = 0
	geom.val = 1
	meshexport.val = 1
	pointwidth.val = 1.0
	uv.val = 0
	arealightMESH.val = 0
	areasamples.val = 20
	recievecaustics.val = 0
	transmittcaustics.val = 0
	transmittcausticsslider.val = 1.4
	caustics.val = 0
	photon.val = 4000
	buff256.val = 0
	buff512.val = 1
	buff1024.val = 0
	buff2048.val = 0
	eblur.val = 0.00
	esamples.val = 1
	amesh.val = 0
	reflectcaustics.val = 0
	raytraceshadow.val = 0
	rendershadow.val = 0
	bias.val = 0.25
	rvisibility.val = 0
	castshadow.val = 1
	objectra.val = 0

def resetShaderVal():
	if (shadermenu.val == 1):
		ambientshader.val = 1
		diffuseshader.val = 0.5
		specularshader.val = 0.5
		roughnessshader.val = 0.1
	if (shadermenu.val == 2):
		ambientshader.val = 0.2
		diffuseshader.val = 0.0
		specularshader.val = 0.5
		roughnessshader.val = 0.05
		coefshader = 1.5
		reflblurshader = 0
		refrblurshader.val = 0
		reflsamples.val = 0
		refrsamples.val = 0
	if (shadermenu.val == 3):
		ambientshader.val = 1.0
		diffuseshader.val = 0.1
		specularshader.val = 1.0
		roughnessshader.val = 0.2
	if (shadermenu.val == 5):
		ambientshader.val = 1
		diffuseshader.val = 0.5
		specularshader.val = 0.5
		roughnessshader.val = 0.1
		textshader.val = 'Surface "plastic"'
		textshaderd.val = 'Displacement "bumpy"'
		texturem.val = ''
		textnor.val = 0
		illuminationmenu.val = 0

def writeShader(ribfile, name, meshobj, anim):
	global texturefile, txdone
	if (arealightMESH.val == 0):
		MName = '"plastic"'
	else: 
		MName = ''
	if ((uv.val == 1) and (meshobj != 10)):
		mesh = Blender.NMesh.GetRawFromObject(name)
		if mesh.faces[0].image:
			imagename = mesh.faces[0].image.filename
			imagename = imagename[:-3]
			imagename = imagename
			imagename = imagename + 'tif'
			imagename = '%s/%s/textures/%s'%(rootdir, dirw.val, os.path.split(imagename)[1])
			rawimage = '%s/%s/textures/%s'%(rootdir, dirw.val, os.path.split(mesh.faces[0].image.filename)[1])
			infile = open(mesh.faces[0].image.filename, 'rb')
			outfile = open(rawimage, 'wb')
			outfile.write(infile.read())
			outfile.close()
			infile.close()
			imagenameconv = texturesdir + os.path.split(mesh.faces[0].image.filename)[1][:-3] + 'tif'
			txname = texturesdir + os.path.split(mesh.faces[0].image.filename)[1][:-3] + 'tx'
			if os.path.isfile(imagename) == 0:
				try:
					totiff.SetOutDir(texturesdir)
					ok = totiff.ConvertShort(rawimage)
					if not ok[0]: # error occurred
						print ok[1] # print error message
				except:
					if (os.name != "posix"):
						try:
							os.system('/ImageMagick/convert.exe -depth 8 -compress none %s %s'%(rawimage, imagename))
						except OSError:
							print "\nYou do not have ImageMagic installed in c:\ImageMagic so texture conversion will not be possible. You will havto do it manualy.\nYou can find the textures to convert in c:\BlenderMan\PROJECTNAME\textures\\n"
					else:
						try:
							os.system('convert -depth 8 -compress none %s %s'%(rawimage, imagename))
						except OSError:
							print "\nCould not find ImageMagic\n"
			imagename = mesh.faces[0].image.filename
			imagename = imagename[:-3]
			imagename = imagename
			tdlname = imagename + 'tx'
			imagename = imagename + 'tif'
			imagename = '%s'%(os.path.split(imagename)[1])
			tdlname = '%s'%(os.path.split(tdlname)[1])
			MName = '"bms" "string texname" ["%s"] "float maptype" 3'%(tdlname)
			if ((os.path.isfile(txname) == 0) or (reconvert.val == 1)):
				if menuval.val == 1:
					os.system('%s %s %s'%(prmantexmaker.val, imagenameconv, txname))
				elif menuval.val == 2:
					os.system('%s %s %s'%(bmrttexmaker.val, imagenameconv, txname))
				elif menuval.val == 3:
					os.system('%s %s %s'%(entropytexmaker.val, imagenameconv, txname))
				elif menuval.val == 4:
					os.system('%s %s %s'%(dltexmaker.val, imagenameconv, txname))
				elif menuval.val == 5:
					os.system('%s %s %s'%(aqsistexmaker.val, imagenameconv, txname))
	else: 
		MName = '"plastic"'
	if (transmittcaustics.val == 1):
		mesh = Blender.NMesh.GetRawFromObject(name)
		try:
			material = Blender.Material.Get(mesh.materials[0].name)
			mr = material.R
			mg = material.G
			mb = material.B
		except: # if the mesh doesnt have a material default to white
			mr = 1
			mg = 1
			mb = 1
		ribfile.write('\t\tAttribute "caustic" "color refractioncolor" [%s %s %s]\n'%(mr, mg, mb))
		ribfile.write('\t\tAttribute "caustic" "float refractionindex" [%s]\n'%transmittcausticsslider.val)
	if (reflectcaustics.val == 1):
		ribfile.write('\t\tAttribute "caustic" "color specularcolor" [.9 .9 .9]\n')
	if (recievecaustics.val == 1):
		ribfile.write('\t\tIlluminate 2 1\n')
	if ((shadermenu.val == 1) & (arealightMESH.val != 1)):
		if ((convertbm.val != 0) or (meshobj == 10)):
			ribfile.write('\t\tSurface "plastic" "Ka" %s "Kd" %s "Ks" %s "roughness" %s\n'%(ambientshader.val, diffuseshader.val, specularshader.val, roughnessshader.val))
	elif ((shadermenu.val == 2) & (arealightMESH.val != 1)):
		if ((convertbm.val != 0) or (meshobj == 10)):
			ribfile.write('\t\tDeclare "eta" "uniform float"\n')
			ribfile.write('\t\tDeclare "blur" "uniform float"\n')
			ribfile.write('\t\tDeclare "refrblur" "uniform float"\n')
			ribfile.write('\t\tDeclare "raysamples" "uniform float"\n')
			ribfile.write('\t\tDeclare "refrraysamples" "uniform float"\n')
			ribfile.write('\t\tDeclare "envname" "uniform string"\n')
			if (menuval.val == 3):
				evar = ('"float reflblur" [%s] "float refrblur" [%s] "string envname" ["reflection"] "string envspace" ["current"] "refrblur" %s "eta" %s'%(reflblurshader.val, refrsamples.val, refrblurshader.val, coefshader.val))
			elif (menuval.val == 2):
				evar = '"blur" [%s] "raysamples" [%s] "refrraysamples" %s "refrblur" %s "eta" %s'%(reflblurshader.val, reflsamples.val, refrsamples.val, refrblurshader.val, coefshader.val)
			else:
				evar = ''
			ribfile.write('\t\tSurface "glass" "Ka" %s "Kd" %s "Ks" %s "roughness" %s "Kr" %s %s\n'%(ambientshader.val, diffuseshader.val, specularshader.val, roughnessshader.val, kr.val, evar))
	elif ((shadermenu.val == 3) & (arealightMESH.val != 1)):
		if ((convertbm.val != 0) or (meshobj == 10)):
			ribfile.write('\t\tDeclare "blur" "uniform float"\n')
			ribfile.write('\t\tDeclare "raysamples" "uniform float"\n')
			if (menuval.val == 3):
				evar = '"string envname" ["reflection"] "string envspace" ["current"] "float twosided" 1'
			elif (menuval.val == 2):
				evar = ('"raysamples" %s "blur" %s'%(reflsamples.val, reflblurshader.val))
			else:
				evar = ''
			ribfile.write('\t\tSurface "shiny" "Ka" %s "Kd" %s "Ks" %s "roughness" %s %s\n'%(ambientshader.val, diffuseshader.val, specularshader.val, roughnessshader.val, evar))
	elif ((shadermenu.val == 4) & (arealightMESH.val != 1)):
		if ((convertbm.val != 0) or (meshobj == 10)):
			ribfile.write('\t\tAttribute "displacementbound" "sphere" [ %s ] "coordinatesystem" [ "object" ]\n'%norm.val)
			ribfile.write('\t\t%s\n'%textshader.val)
			ribfile.write('\t\t%s\n'%textshaderd.val)
	elif ((shadermenu.val == 5) & (arealightMESH.val != 1)):
		if ((convertbm.val != 0) or (meshobj == 10)):
			ribfile.write('\t\tAttribute "displacementbound" "sphere" [ %s ] "coordinatesystem" [ "object" ]\n'%norm.val)
			txname = texturem.val[:-3]
			txname = txname + 'tx'
			tdlname = txname
			txname = texturesdir + txname
			converttex = 1
			if os.path.isfile(txname) == 0:
				for texture in txdone:
					if (txname == texture):
						converttex = 0
						break
				if (converttex == 1):
					if (menuval.val == 1):
						os.system('%s %s %s'%(prmantexmaker.val, texturesdir + texturem.val, txname))
					elif (menuval.val == 2):
						os.system('%s %s %s'%(bmrttexmaker.val, texturesdir + texturem.val, txname))
					elif (menuval.val == 3):
						os.system('%s %s %s'%(entropytexmaker.val, texturesdir + texturem.val, txname))
					elif (menuval.val == 4):
						os.system('%s %s %s'%(dltexmaker.val, texturesdir + texturem.val, txname))
					elif (menuval.val == 5):
						os.system('%s %s %s'%(aqsistexmaker.val, texturesdir + texturem.val, txname))
			txdone = txdone + [txname]
			txname = tdlname
			if (textnor.val == 1):
				ribfile.write('\t\tDisplacement "bmd" "string texname" ["%s"] "float maptype" %s "float Km" %s\n'%(txname, projection.val - 1, norm.val))
			ribfile.write('\t\tSurface "bms" "Ka" %s "Kd" %s "Ks" %s "roughness" %s "string texname" ["%s"] "float maptype" %s\n'%(ambientshader.val, diffuseshader.val, specularshader.val, roughnessshader.val, txname, projection.val - 1))
	if ((convertbm.val == 0) & (meshobj != 10)):
		mesh = Blender.NMesh.GetRawFromObject(name)
		if (MName == ''):
			return
		try:
			material = Blender.Material.Get(mesh.materials[0].name)
			hard = 1-(material.hard*0.00392)
			ribfile.write('\t\tSurface %s "Ka" %s "Kd" %s "Ks" %s "roughness" %s\n'%(MName, material.amb, material.ref, material.spec, hard))
		except:
			ribfile.write('\t\tSurface %s\n'%(MName))
	if (meshobj != 10):
		mesh = Blender.NMesh.GetRawFromObject(name)
		try:
			if mesh.materials:
				material = Blender.Material.Get(mesh.materials[0].name)
				ribfile.write("\t\tColor [%s %s %s]\n" %(material.R, material.G, material.B))
		except:	pass
	if (arealightMESH.val == 1):
		ribfile.write('\t\tSurface "constant"\n')
		ribfile.write('\t\tAttribute "light" "nsamples" [%s]\n'%areasamples.val)
		if (menuval.val == 2):
			ribfile.write('\t\tAttribute "light" "shadows" ["on"]\n')
		mesh = Blender.NMesh.GetRawFromObject(name)
		try:
			material = Blender.Material.Get(mesh.materials[0].name)
			if (menuval.val == 3):
				ribfile.write('\t\tAreaLightSource "arealight" 999 "intensity" [%s] "lightcolor" [%s %s %s] "string shadowname" ["shadow"] "float shadowbias" [0.15]\n'%(material.emit * 100, material.R, material.G, material.B))
			elif (menuval.val == 2):
				ribfile.write('\t\tAreaLightSource "arealight" 999 "intensity" [%s] "lightcolor" [%s %s %s]\n'%(material.emit * 100, material.R, material.G, material.B))
		except:
			ribfile.write('\t\tAreaLightSource "arealight" 999 "intensity" [%s] "lightcolor" [%s %s %s]\n'%(0.7 * 100, 1, 1, 1))

def createObjects(ribfile, cframe, shadows, frames2):
	global sshader, selectedname, thisone, meshobjects, xbar, meshnum, partitionname
	meshobjects = 0.0
	thisone = 0
	meshnum = 0
	objecttime = ''
	for objects in Blender.Object.Get():
		name = objects.getName()
		try:
			if (objects.getType() == "Mesh"): meshobjects = meshobjects + 1
		except: 
			pass
	for objects in Blender.Object.Get():
		name = objects.getName()
		resetObject()
		area = 0
		obj = Blender.Object.Get(name)
		i = 0
		while (obj.Layer>>i): # convert layer number
			i = i + 1
		if (i == hiddenlayer.val):
			layer = 1
		else:
			layer = 0
		if ((objects.getType() == "Mesh") & (layer == 0)):
			meshnum = meshnum + 1
			meshobj = Blender.Object.Get(name)
			obj = Blender.Object.Get(name)
			selectedname = name
			Register (gui, event, bevent)
			saveload(0,0,'object')
			if (shadows == 0):
				ribfile.write('\tAttributeBegin\n')
			if ((shadows == 0) and (objectra.val == 1)):
				ribfile.write('\t\tReadArchive "%s"\n' %(objectrafile.val))
			if (shadows != 1):
				mesh = Blender.NMesh.GetRawFromObject(name)
				if (len(mesh.faces) == 0):
					ribfile.write('\tAttributeEnd\n')
					del(mesh)
					return
				if (mesh.materials):
					try:
						material = Blender.Material.Get(mesh.materials[0].name)
						ribfile.write("\t\tColor [%s %s %s]\n" %(material.R, material.G, material.B))
						ribfile.write("\t\tOpacity [%s %s %s]\n" %(material.alpha, material.alpha, material.alpha))
					except:
						pass
				del(mesh)

			if (motionblur.val == 1):
				ribfile.write('\tMotionBegin [0 1]\n')
				Blender.Set('curframe', cframe) 
				writeMatrix(ribfile, name)
				Blender.Set('curframe', cframe + motionblurframe.val)
				writeMatrix(ribfile, name)
				ribfile.write('\tMotionEnd\n')
				Blender.Set('curframe', cframe) 
			else:
				writeMatrix(ribfile, name)
			namelist = string.split(sshader, "|")
			if len(namelist) > 1: 
				surfaceshadername.val = namelist[objectshadermenu.val - 1] 
			Register (gui, event, bevent)
			if (convertbm.val == 1):
				saveload(0,1,'shader')
				Register (gui, event, bevent)
			if (castshadow.val == 1):
				op = "Os"
			elif (castshadow.val == 2):
				op = "opaque"
			elif (castshadow.val == 3):
				if (menuval.val == 1) or (menuval.val == 4):
					op = "shader"
				else:
					op = "shade"
			elif (castshadow.val == 4):
				if (menuval.val == 1) or (menuval.val == 4):
					op = "transparent"
				else:
					op = "none"
			if (menuval.val == 1) or (menuval.val == 4):
				ribfile.write('\t\tAttribute "visibility" "string transmission" ["%s"]\n'%op)
			elif (menuval.val == 2) or (menuval.val == 3):
				ribfile.write('\t\tAttribute "render" "string casts_shadows" ["%s"]\n'%op)
			if (rvisibility.val == 1):
				if (menuval.val == 3):
					ribfile.write('\t\tAttribute "visibility" "integer camera" [0]\n')
				elif (menuval.val == 2):
					ribfile.write('\t\tAttribute "render" "integer visibility" [2]\n')
			if (shadows == 0):
				writeShader(ribfile, name, meshobj, frames2)
			if (amesh.val == 1):
				objecttime = frames2
			else:
				objecttime = ''
			if (motionblur.val != 1):
				if ((frames2 == 0) or amesh.val == 1):
					if (geom.val == 1):
						meshfile = open('%s/%s%s.rib'%(meshesdir,name, objecttime), 'w')
						check(name, meshfile, meshobj, meshnum, shadows)
				if ((menuval.val == 4) & (os.name != "posix")): ribfile.write('\t\tReadArchive "%s%s"\n'%(os.path.splitdrive( os.path.normcase( os.path.normpath(partitionname + meshesdir + name + str(objecttime) + ".rib")))))
				else: ribfile.write('\t\tReadArchive "%s/%s%s.rib"\n'%(meshesdir, name, objecttime))
			else:
				if (meshexport.val == 2):
					if ((frames2 == 0) or amesh.val == 1):
						if (geom.val == 1):
							meshfile = open('%s/%s%s.rib'%(meshesdir,name, objecttime), 'w')
							check(name, meshfile, meshobj, meshnum, shadows)
					if ((menuval.val == 4) & (os.name != "posix")): ribfile.write('\t\tReadArchive "%s%s"\n'%(os.path.splitdrive( os.path.normcase( os.path.normpath(partitionname + meshesdir + name + objecttime + ".rib")))))
					else: ribfile.write('\t\tReadArchive "%s/%s%s.rib"\n'%(meshesdir, name, objecttime))
				else:
					if (amesh.val != 1):
						if (frames2 == 0):
							if (geom.val == 1):
								meshfile = open('%s/%s%s.rib'%(meshesdir,name, objecttime), 'w')
								check(name, meshfile, meshobj, meshnum, shadows)
						if ((menuval.val == 4) & (os.name != "posix")): ribfile.write('\t\tReadArchive "%s%s"\n'%(os.path.splitdrive( os.path.normcase( os.path.normpath(partitionname + meshesdir + name + objecttime + ".rib")))))
						else: ribfile.write('\t\tReadArchive "%s/%s%s.rib"\n'%(meshesdir, name, objecttime))
					else:
						Blender.Set('curframe', cframe) 
						Blender.Window.RedrawAll()
						if (geom.val == 1):
							meshfile = open('%s/%s%s.rib'%(meshesdir,name, objecttime), 'w')
							check(name, meshfile, meshobj, meshnum, shadows)
						ribfile.write('\tMotionBegin [0 1]\n')
						if ((menuval.val == 4) & (os.name != "posix")): ribfile.write('\t\tReadArchive "%s%s"\n'%(os.path.splitdrive( os.path.normcase( os.path.normpath(partitionname + meshesdir + name + str(objecttime) + ".rib")))))
						else: ribfile.write('\t\tReadArchive "%s/%s%s.rib"\n'%(meshesdir, name, objecttime))
						Blender.Set('curframe', cframe + motionblurframe.val)
						Blender.Window.RedrawAll()
						objecttime = frames2 + motionblurframe.val
						if (geom.val == 1):
							meshfile = open('%s/%s%s.rib'%(meshesdir,name, str(objecttime)), 'w')
							check(name, meshfile, meshobj, meshnum, shadows)
						if ((menuval.val == 4) & (os.name != "posix")): ribfile.write('\t\tReadArchive "%s%s"\n'%(os.path.splitdrive( os.path.normcase( os.path.normpath(partitionname + meshesdir + name + str(objecttime) + ".rib")))))
						else: ribfile.write('\t\tReadArchive "%s/%s%s.rib"\n'%(meshesdir, name, str(objecttime)))
						ribfile.write('\tMotionEnd\n')
						Blender.Set('curframe', cframe) 
						Blender.Window.RedrawAll()
						objecttime = frames2
			if (shadows == 0):
				ribfile.write('\tAttributeEnd\n')
			if (arealightMESH.val == 1):
				ribfile.write('\tIlluminate 999 1\n')

def drawBar(meshnum, qwerty, pixels):
	if (progress.val == 1):
		global thisone, meshobjects, xbar
		xbar = 0
		xbar = qwerty
		if (xbar < 10): xbar = 11
		Draw()
	elif ((progress.val == 0) & (anim.val == 1)):
		testtime = Blender.Get('curframe')

def check(name, meshfile, meshobj, meshnum, shadows):
	if (shadows != 1):
		print 'Exporting: %s'%name
		mesh = Blender.NMesh.GetRawFromObject(name)
		if (len(mesh.faces) == 0):
			del(mesh)
			return
		if (meshexport.val == 1):
			pointspolygon(meshfile, mesh)
		elif (meshexport.val == 2):
			polygon(mesh, meshfile, meshobj, meshnum)
		elif (meshexport.val == 3):
			subdivmesh(meshfile, mesh)
		elif (meshexport.val == 4):
			points(meshfile, mesh)
		elif (meshexport.val == 5):
			general(meshfile, mesh)
		elif (meshexport.val == 6):
			bilinear(meshfile, mesh)
		del(mesh)
	meshfile.close()

def pointspolygon(file, mesh): 
	global meshnum
	testtime = Blender.Get('curframe')
	update = 5
	colorif = 0
	perso = 0.0
	pixels = 0.0
	update = 100
	index = 0
	facenum = len(mesh.faces)
	if menuval.val != 2:
		if mesh.hasFaceUV() == 1:
			file.write('Declare "st" "facevarying float[2]"\n')
	file.write("PointsPolygons [");
	for face in mesh.faces:
		#if len(face.v) == 4 and menuval.val == 2:
		if len(face.v) == 4:
			file.write('3 3 ')
		else:
			file.write('%s '%(len(face.v)))
		index = index + 1
	if ((index == update) or (index == facenum)):
		update = update + 100
		pixels = 619
		perso = float(index) / float(facenum)
		qwerty = float(pixels) * float(perso) 
		drawBar(meshnum, qwerty, pixels)
	file.write("] ")
	file.write("[ ")
	for face in mesh.faces:
		num = len(face.v)
		if num == 3 or num == 4:
			#if num == 4 and menuval.val == 2:
			if num == 4:
				for i in (0,1,2,0,2,3):
					file.write('%s ' % face.v[i].index)
			else:
				for vert in face.v:
					file.write('%s ' % vert.index)
	file.write("]")
	file.write('\n"P" [')
	for vert in mesh.verts:
		file.write("%s %s %s " % (vert.co[0], vert.co[1], vert.co[2]))
	file.write('] ')
	if mesh.faces[0].smooth:
		file.write(' "N" [')
		for vert in mesh.verts:
			file.write("%s %s %s " % (vert.no[0], vert.no[1], vert.no[2]))
		file.write(']')
	if mesh.hasVertexColours() == 1:
		vertexcol = range(len(mesh.verts))
		file.write('\n"Cs" [')
		for face in mesh.faces:
			num = len(face.v)
			if num == 3 or num == 4:
				for vi in range(len(face.v)):
					vertexcol[face.v[vi].index] = face.col[vi]
		for vc in vertexcol:
			file.write('%s %s %s ' % (vc.r/256.0, vc.g/256.0, vc.b/256.0))
		file.write(']')
	if (menuval.val != 2):
		if mesh.hasFaceUV() == 1:
			file.write('\n"st" [')
			for face in mesh.faces:
				num = len(face.v)
				if num == 3 or num == 4:
					for vi in range(len(face.v)):
						file.write('%s %s ' % (face.uv[vi][0], 1.0 - face.uv[vi][1]))
			file.write(']')
	else:
        	if (len(mesh.faces[0].uv) != 0):
			file.write('\n"st" [ ')
			vtuv = [] 
			for i in range(len(mesh.verts)): 
				vtuv.append(0) 
			for f in mesh.faces: 
				for i in range(len(f.uv)): 
					uv = f.uv[i]
					uv = uv[0], 1.0 - uv[1]
					vtuv[f.v[i].index] = uv
			for c in vtuv:
				for d in c: 
					file.write('%s '%d)
			file.write(']')
	file.write('\n')

def polygon(mesh, meshfile, meshobj, meshnum):
	global meshobjects, i
	i = 0
	update = 5
	colorif = 0
	perso = 0.0
	pixels = 0.0
	facenum = len(mesh.faces)
	for face in mesh.faces:
		if mesh.materials:
			matname=mesh.materials[0].name
			try:
				material = Blender.Material.Get(matname)
				if (colorif !=  material.R + material.G + material.B):
					colorif = material.R + material.G + material.B
					meshfile.write("\t\tColor [%s %s %s]\n" %(material.R, material.G, material.B))
			except:
				pass
		if len(mesh.faces[i].v) == 3:
			vertar = 3
		else:
			vertar = 4
		if face.smooth:
			smooth = 1
		else:
			smooth = 0
		writePoly(mesh,face, meshfile, i, vertar, smooth);
		i = i + 1
		if ((i == update) or (i == facenum)):
			update = update + 5
			pixels = 619
			perso = float(i) / float(facenum)
			qwerty = float(pixels) * float(perso)
			drawBar(meshnum, qwerty, pixels)

def textureUV(i, mesh, va, vb, vc, meshfile, face, number):
	try:
		etest = face.uv[0][0]
	except:
		return
	meshfile.write('"st" [ ')
	if (number == 1):
		for x in [0, 1, 2]:
			meshfile.write("%s "%face.uv[x][0])
			meshfile.write("%s "%(1.0 - face.uv[x][1]))
	if (number == 2):
		if len(mesh.faces[i].v) == 3: 
			return
		for x in [0, 2, 3]:
			meshfile.write("%s "%face.uv[x][0])
			meshfile.write("%s "%(1.0 - face.uv[x][1]))
	meshfile.write(']\n')

def writePoly(mesh, face, meshfile, i, vertar, smooth):
	meshfile.write('Polygon "P" [ ')
	for x in [0, 1, 2]:
		for y in range(3):
			try:
				meshfile.write('%s '%mesh.faces[i].v[x].co[y])
			except Error:
				print i
				print x
				print y
	meshfile.write(']\n')
	if (smooth == 1):
		meshfile.write('"N" [')
		for x in [0, 1, 2]:
			for y in range(3):
				meshfile.write('%s '%mesh.faces[i].v[x].no[y])
		meshfile.write(']\n')
	number = 1
	textureUV(i,mesh,0,1,2, meshfile, face, number)
	if (vertar == 4):
		meshfile.write('Polygon "P" [ ')
		for x in [0, 2, 3]:
			for y in range(3):
				meshfile.write('%s '%mesh.faces[i].v[x].co[y])
		meshfile.write(']\n')
		if (smooth == 1):
			meshfile.write('"N" [')
			for x in [0, 2, 3]:
				for y in range(3):
					meshfile.write('%s '%mesh.faces[i].v[x].no[y])
			meshfile.write(']\n')
		number = 2
		textureUV(i,mesh,0,1,2, meshfile, face, number)

def general(meshfile, mesh):
	index = 0
	meshfile.write('PointsGeneralPolygons [')
	for face in mesh.faces:
		meshfile.write('1 ')
		index = index + 1
	meshfile.write(']\n[')
	index = 0
	for face in mesh.faces:
		meshfile.write(str(nr) + ' ')
		index = index + 1
	meshfile.write(']\n[')
	index = 0
	for f in mesh.faces:
		for v in f.v:
			meshfile.write(str(v.index) + ' ')
		index = index + 1
	meshfile.write(']\n"P" [')
	index = 0
	for x in range(len(mesh.verts)):
		for y in range(3):
			meshfile.write(str(mesh.verts[x].co[y]) + '')
		index = index + 1
	meshfile.write(']\n')

def bilinear(meshfile, mesh):
	f = 0
	for face in mesh.faces:
		meshfile.write('Patch "bilinear" "P" [ ')
		for x in [3, 2, 0, 1]:
			for y in range(3):
				meshfile.write('%s '%mesh.faces[f].v[x].co[y])
		meshfile.write(']\n')
		if face.smooth:
			meshfile.write('"N" [ ')
			for x in [3, 2, 0, 1]:
				for y in range(3):
					meshfile.write('%s '%mesh.faces[f].v[x].no[y])
			meshfile.write(']\n')
		f = f + 1

def subdivmesh(meshfile, mesh):
	if menuval.val != 2:
		if mesh.hasFaceUV() == 1:
			meshfile.write('Declare "st" "facevarying float[2]"\n')
	meshfile.write('SubdivisionMesh "catmull-clark" [')
	for face in mesh.faces:
		num = len(face.v)
		meshfile.write('%s '%(num))
	meshfile.write(']\n[')
	for face in mesh.faces:
		for vert in face.v:
			meshfile.write('%s ' % vert.index)
	meshfile.write(']\n["interpolateboundary"] [0 0] [] []\n"P" [') 
	for vert in mesh.verts:
		meshfile.write("%s %s %s " % (vert.co[0], vert.co[1], vert.co[2]))
	meshfile.write(']')
	if (menuval.val != 2):
		if mesh.hasFaceUV() == 1:
			meshfile.write('\n"st" [')
			for face in mesh.faces:
				num = len(face.v)
				if num == 3 or num == 4:
					for vi in range(len(face.v)):
						meshfile.write('%s %s ' % (face.uv[vi][0], 1.0 - face.uv[vi][1]))
			meshfile.write(']')
	if mesh.hasVertexColours() == 1:
		vertexcol = range(len(mesh.verts))
		meshfile.write('\n"Cs" [')
		for face in mesh.faces:
			num = len(face.v)
			if num == 3 or num == 4:
				for vi in range(len(face.v)):
					vertexcol[face.v[vi].index] = face.col[vi]
		for vc in vertexcol:
			meshfile.write('%s %s %s ' % (vc.r/256.0, vc.g/256.0, vc.b/256.0))
		meshfile.write(']')
	meshfile.write('\n')

def points(meshfile, mesh):
	meshfile.write('Points "P" [')
	for vert in mesh.verts:
		meshfile.write("%s %s %s " % (vert.co[0], vert.co[1], vert.co[2]))
	meshfile.write('] "constantwidth" [%s]\n' % (pointwidth.val))

def createGI(ribfile, frames):
	if (areagi.val == 1):
		ribfile.write('\tDeclare "casts_shadows" "string"\n')
		ribfile.write('\tAttribute "render" "casts_shadows" ["opaque"]\n')
		ribfile.write('\tLightSource "ambientlight" 998 "lightcolor" [0.02 0.02 0.02]\n')
		ribfile.write('\tAttributeBegin\n')
		ribfile.write('\t\tAttribute "identifier" "name" ["sky"]\n')
		ribfile.write('\t\tConcatTransform [1 0 0 0 0 1 0 0 0 0 1 0 -1573.14 307.099 -988.299 1]\n')
		ribfile.write('\t\tAttribute "render" "visibility" [3]\n') # invisible to the camera
		ribfile.write('\t\tAttributeBegin\n')
		ribfile.write('\t\t\tColor [1 1 1]\n')
		ribfile.write('\t\t\tOrientation "rh"\n')
		ribfile.write('\t\t\tReverseOrientation\n')
		ribfile.write('\t\t\tSurface "constant"\n')
		ribfile.write('\t\t\tAttribute "light" "nsamples" [%s]\n'%gisteps.val)
		ribfile.write('\t\t\tAttribute "light" "shadows" ["on"]\n')
		ribfile.write('\t\t\tAreaLightSource "arealight" 1 "lightcolor" [1 1 1] "intensity" [10000000000]\n')
		ribfile.write('\t\t\tSphere 50000 0 50000 360\n')
		ribfile.write('\t\tAttributeEnd\n')
		ribfile.write('\tAttributeEnd\n')
		ribfile.write('\tIlluminate 1 1\n')
	if (indirectgi.val == 1):
		if (finalgather.val == 0):
			ribfile.write('\tAttribute "indirect" "float maxerror" [%s]\n'%maxerror.val)
			ribfile.write('\tAttribute "indirect" "float maxpixeldist" [%s]\n'%maxpixeldist.val)
		else:
			ribfile.write('\tAttribute "indirect" "float maxpixeldist" [0]\n')
		ribfile.write('\tAttribute "indirect" "integer nsamples" [%s]\n'%gisteps.val)
		ribfile.write('\tLightSource "indirect" 42\n\n')
		if (gisphere.val == 1):
			ribfile.write('\tAttributeBegin\n')
			ribfile.write('\t\tAttribute "render" "visibility" [3]\n') # invisible to the camera
			ribfile.write('\t\tColor [1 1 1]\n')
			ribfile.write('\t\tSurface "constant"\n')
			ribfile.write('\t\tSphere 2000 -2000 2000 360\n')
			ribfile.write('\tAttributeEnd\n')

def render(ribfile):
	if (os.name != "posix"):
		if (menuval.val == 1):
			os.system('%s %s'%(prmanrenderer.val, ribfile))
		elif (menuval.val == 2):
			os.system('%s %s'%(bmrtrenderer.val, ribfile))
		elif (menuval.val == 3):
			os.system('%s %s'%(entropyrenderer.val, ribfile))
		elif (menuval.val == 4):
			os.system('%s %s'%(dlrenderer.val, ribfile))
		elif (menuval.val == 5):
			os.system('%s %s'%(aqsisrenderer.val, ribfile))
	elif (os.name == "posix"):
		if (menuval.val == 1):
			os.system('%s %s &'%(prmanrenderer.val, ribfile))
		elif (menuval.val == 2):
			os.system('%s %s &'%(bmrtrenderer.val, ribfile))
		elif (menuval.val == 3):
			os.system('%s %s &'%(entropyrenderer.val, ribfile))
		elif (menuval.val == 4):
			os.system('%s %s &'%(dlrenderer.val, ribfile))
		elif (menuval.val == 5):
			os.system('%s %s &'%(aqsisrenderer.val, ribfile))

def closeFile(ribfile):
	ribfile.close()

def writeFrame(ribfile):
	global sshader, cframe
	frames2 = 0
	display = Blender.Scene.GetCurrent()
	if (anim.val == 1):
		frames = display.endFrame() - display.startFrame()
	else:
		frames = 0
	if (anim.val == 1):
		cframe = display.startFrame()
	else:
		cframe = Blender.Get('curframe')
	for x in range(frames + 1):
		Blender.Set('curframe', cframe)
		print 'Exporting Frame: %s'%Blender.Get('curframe')
		renderShadows(ribfile, frames2)
		ribfile.write('\nFrameBegin %s\n'%cframe)
		writeHeader(ribfile, frames, frames2)
		lampobj = 0
		if (motionblur.val == 1):
			ribfile.write('MotionBegin [0 1]\n')
			Blender.Set('curframe', cframe)
			writeTransform(ribfile, lampobj)
			Blender.Set('curframe', cframe + motionblurframe.val)
			writeTransform(ribfile, lampobj)
			Blender.Set('curframe', cframe)
			ribfile.write('MotionEnd\n')
		else:
			writeTransform(ribfile, lampobj)
		if (rgba.val == 1):
			channels = "rgb"
		elif (rgba.val == 2):
			channels = "rgba"
		elif (rgba.val == 3):
			channels = "rgbz"
		elif (rgba.val == 4):
			channels = "z"
		if (buffer.val == 1):
			ribfile.write('Display "%s/%s%05d.tif" "framebuffer" "%s"\n'%(imagesdir, image.val, cframe, channels))
		if (file.val == 1):
			ribfile.write('Display "%s/%s%05d.tif" "file" "%s"\n'%(imagesdir, image.val, cframe, channels))
		if (preworldra.val == 1):
			ribfile.write('ReadArchive "%s"\n'%(preworldrafile.val))
		ribfile.write('WorldBegin \n')
		if (postworldra.val == 1):
			ribfile.write('\tReadArchive "%s"\n'%(postworldrafile.val))
		if (menuval.val == 1) or (menuval.val == 4):
			ribfile.write('\tAttribute "visibility" "integer trace" [1]\n')
		if (menuval.val == 3):
			ribfile.write('\tAttribute "visibility" "reflection" [1]\n')
			ribfile.write('\tAttribute "visibility" "shadow" [1]\n')
		ambientLight(ribfile)
		checkLamps(ribfile)
		if (gi.val == 1):
			createGI(ribfile, frames)
		if (hdri.val == 1):
			if (menuval.val == 3):
				ribfile.write('\tAttributeBegin\n')
				ribfile.write('\t\tAttribute "visibility" "reflection" [1]\n')
				ribfile.write('\t\tSurface "envsurf" "string envname" ["%s"] "float blur" [0]\n'%(hdritex.val))
				ribfile.write('\t\tSphere 50000 0 50000 360\n')
				ribfile.write('\tAttributeEnd\n')
		createObjects(ribfile, cframe, 0, frames2) # 0 is for the shadows variable
		ribfile.write('WorldEnd \n')
		ribfile.write('FrameEnd\n')
		cframe = cframe + 1
		frames2 = frames2 + 1

def export(): # start the export stuff from here
	global selectedtype, selectedname, xbar, texturefile, shadernametemp, partitionname
	newdirs() # make sure all the directories are there
	xbar = 10 # reset the progressbar
	selectedtype = 'Type:' # reset the object editor stuff
	selectedname = ''
	filedir = os.path.normpath(pdir + filename.val)
	ribfile = open(filedir, 'w') # open the main ribfile (default.rib)
	if ((menuval.val == 4) & (os.name != "posix")):
		ribfile.write('ReadArchive "%s%s"\n'% os.path.splitdrive( os.path.normcase( os.path.normpath(partitionname + pdir + "/texturefile.rib") ) ) )
	else:
		ribfile.write('ReadArchive "%s"\n'% (pdir + "/texturefile.rib"))
	texturefile = open(os.path.normpath(pdir + 'texturefile.rib'), 'w') # open the texture rib file
	if (menuval.val == 5):
		texturefile.write('#\n')
	writeFrame(ribfile) # write all the frames
	closeFile(ribfile) # close the main ribfile
	texturefile.close()
	selectedtype = 'Type:' # reset the object editor stuff
	selectedname = ''
	surfaceshadername.val = shadernametemp
	Register (gui, event, bevent) # make sure all gui values are in place
	if (rendert.val == 1): # if the renderbutton is on
		render(pdir + filename.val) # render the rib file
