# This file was created automatically by SWIG.
# Don't modify this file, modify the SWIG interface instead.
# This file is compatible with both classic and new-style classes.
import _rib
def _swig_setattr(self,class_type,name,value):
    if (name == "this"):
        if isinstance(value, class_type):
            self.__dict__[name] = value.this
            if hasattr(value,"thisown"): self.__dict__["thisown"] = value.thisown
            del value.thisown
            return
    method = class_type.__swig_setmethods__.get(name,None)
    if method: return method(self,value)
    self.__dict__[name] = value

def _swig_getattr(self,class_type,name):
    method = class_type.__swig_getmethods__.get(name,None)
    if method: return method(self)
    raise AttributeError,name

import types
try:
    _object = types.ObjectType
    _newclass = 1
except AttributeError:
    class _object : pass
    _newclass = 0


RiGaussianFilter = _rib.RiGaussianFilter

RiBoxFilter = _rib.RiBoxFilter

RiTriangleFilter = _rib.RiTriangleFilter

RiCatmullRomFilter = _rib.RiCatmullRomFilter

RiSincFilter = _rib.RiSincFilter

RiGeiContext = _rib.RiGeiContext

RiContext = _rib.RiContext

RiDeclare = _rib.RiDeclare

RiBegin = _rib.RiBegin

RiEnd = _rib.RiEnd

RiFrameBegin = _rib.RiFrameBegin

RiFrameEnd = _rib.RiFrameEnd

RiWorldBegin = _rib.RiWorldBegin

RiWorldEnd = _rib.RiWorldEnd

RiFormat = _rib.RiFormat

RiFrameAspectRatio = _rib.RiFrameAspectRatio

RiScreenWindow = _rib.RiScreenWindow

RiCropWindow = _rib.RiCropWindow

RiProjection = _rib.RiProjection

RiProjectionV = _rib.RiProjectionV

RiClipping = _rib.RiClipping

RiClippingPlane = _rib.RiClippingPlane

RiDepthOfField = _rib.RiDepthOfField

RiShutter = _rib.RiShutter

RiQuantize = _rib.RiQuantize

RiShadingInterpolation = _rib.RiShadingInterpolation

RiPixelFilter = _rib.RiPixelFilter

RiExposure = _rib.RiExposure

RiDisplay = _rib.RiDisplay

RiDisplayV = _rib.RiDisplayV

RiOption = _rib.RiOption

RiOptionV = _rib.RiOptionV

RiShadingRate = _rib.RiShadingRate

RiIdentity = _rib.RiIdentity

RiTransform = _rib.RiTransform

RiConcatTransform = _rib.RiConcatTransform

RiPerspective = _rib.RiPerspective

RiTranslate = _rib.RiTranslate

RiRotate = _rib.RiRotate

RiScale = _rib.RiScale

RiCoordinateSystem = _rib.RiCoordinateSystem

RiPolygon = _rib.RiPolygon

RiPolygonV = _rib.RiPolygonV

RiPointsPolygons = _rib.RiPointsPolygons

RiPointsPolygonsV = _rib.RiPointsPolygonsV

RiPointsGeneralPolygons = _rib.RiPointsGeneralPolygons

RiPointsGeneralPolygonsV = _rib.RiPointsGeneralPolygonsV

RiSphere = _rib.RiSphere

RiSphereV = _rib.RiSphereV

RiAttribute = _rib.RiAttribute

RiAttributeV = _rib.RiAttributeV

RiAttributeBegin = _rib.RiAttributeBegin

RiAttributeEnd = _rib.RiAttributeEnd

RiColor = _rib.RiColor

RiOpacity = _rib.RiOpacity

RiTextureCoordinates = _rib.RiTextureCoordinates

RiAreaLightSource = _rib.RiAreaLightSource

RiAreaLightSourceV = _rib.RiAreaLightSourceV

RiLightSource = _rib.RiLightSource

RiLightSourceV = _rib.RiLightSourceV

RiTransformBegin = _rib.RiTransformBegin

RiTransformEnd = _rib.RiTransformEnd

RiOrientation = _rib.RiOrientation

RiSides = _rib.RiSides

RiPixelSamples = _rib.RiPixelSamples

RiSurface = _rib.RiSurface

RiSurfaceV = _rib.RiSurfaceV

RiImager = _rib.RiImager

RiImagerV = _rib.RiImagerV

RiIlluminate = _rib.RiIlluminate

RiDisplatement = _rib.RiDisplatement

RiDisplacementV = _rib.RiDisplacementV

RiAtmosphere = _rib.RiAtmosphere

RiAtmosphereV = _rib.RiAtmosphereV

RiBasis = _rib.RiBasis

RiMatte = _rib.RiMatte

RiMotionBegin = _rib.RiMotionBegin

RiMotionBeginV = _rib.RiMotionBeginV

RiMotionEnd = _rib.RiMotionEnd

RiTrimCurve = _rib.RiTrimCurve

RiNuPatchV = _rib.RiNuPatchV

RiSubdivisionMesh = _rib.RiSubdivisionMesh

RiSubdivisionMeshV = _rib.RiSubdivisionMeshV

RiCurvesV = _rib.RiCurvesV

RiArchiveRecord = _rib.RiArchiveRecord

RiPoints = _rib.RiPoints

RiPointsV = _rib.RiPointsV

RiErrorHandler = _rib.RiErrorHandler

RiHider = _rib.RiHider

RiHiderV = _rib.RiHiderV

cvar = _rib.cvar

