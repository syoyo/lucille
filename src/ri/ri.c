#include "ri.h"

RtToken RI_P            = "P";
RtToken RI_N            = "N";
RtToken RI_ST           = "ST";
RtToken RI_CS           = "Cs";
RtToken RI_FILE         = "file";
RtToken RI_FRAMEBUFFER  = "framebuffer";
RtToken RI_RGB          = "rgb";
RtToken RI_RGBA         = "rgba";
RtToken RI_PERSPECTIVE  = "perspective";
RtToken RI_ORTHOGRAPHIC = "orthographic";
RtToken RI_FOV          = "fov";
RtToken RI_RH           = "rh";
RtToken RI_LH           = "lh";
RtToken RI_COMMENT      = "comment";

RtBasis RiBSplineBasis  = {{-1.0,  3.0, -3.0, 1.0},
               { 3.0, -6.0,  3.0, 0.0},
               {-3.0,  0.0,  3.0, 0.0},
               { 1.0,  4.0,  1.0, 0.0}};

