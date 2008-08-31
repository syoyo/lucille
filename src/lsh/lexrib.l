%{
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif

#include "memory.h"
#include "array.h"
#include "parserib.h"
#include "render.h"

#define YY_MALLOC_DECL

/* for ReadArchive */
typedef struct _ribstack_t
{
    int                 line_num;
    const char         *rib;
    YY_BUFFER_STATE     state;
    struct _ribstack_t *next;
} ribstack_t;

static ribstack_t *ribstack = NULL;
static const char *gribfile = NULL;

int line_num = 0;
int str_pos;

int lexrib_mode_param;
int lexrib_mode_skip;

static FILE *ribin;

void add_string_char(char c)
{
    yylval.string[str_pos++] = c;
    yylval.string[str_pos]   = '\0';
}

static void read_archive(const char *rib)
{
    FILE            *fp;
    YY_BUFFER_STATE  state;
    ribstack_t      *stack;
    ri_option_t     *opt;
    char             fullpath[1024];

    if (!rib) return;

    if (rib[0] == '\0') return;

#ifdef DEBUG
    fprintf(stdout, "ReadArchive = %s\n", rib); 
#endif
    

    opt = ri_render_get()->context->option;

    if (!ri_option_find_file(fullpath, opt, rib)) {
        fprintf(stderr,
                "[warning] ReadArchive: Can't open RIB \"%s\"\n",
            rib);
        ri_option_show_searchpath(opt);
        return;
    } else {
        fp = fopen(fullpath, "r");
        if (!fp) {
            fprintf(stderr,
                "[warning] ReadArchive: Can't open RIB ");
            fprintf(stderr,
                "\"%s\"\n", rib);
            return;
        }
    }

    stack = (ribstack_t *)ri_mem_alloc(sizeof(ribstack_t));
    stack->state = YY_CURRENT_BUFFER;
    stack->rib   = gribfile;
    stack->next  = ribstack;
    ribstack     = stack;

    state = yy_create_buffer(fp, YY_BUF_SIZE);

    /* Change reading input RIB file. */
    yy_switch_to_buffer(state);

    ribin = fp;

    /* tear down is done in <<EOF>> pattern */
}


%}
%option nounput
%option noyywrap
%option stack
WHITESPACE    [[:blank:]]*
NUMBER        [-+]?([0-9]+|(([0-9]+)|([0-9]+\.[0-9]*)|(\.[0-9]+))([eE][-+]?[0-9]+)?)
IDENT        [a-zA-Z_][a-zA-Z_0-9]*
%x COMMENT STR
%s PARAM
%%
    if (lexrib_mode_skip) {
        BEGIN INITIAL;
        lexrib_mode_skip = 0;
    }
    
    if (lexrib_mode_param) {
        BEGIN PARAM;
        lexrib_mode_param = 0;
    }

"#" { BEGIN COMMENT; }
<COMMENT>.        /* skip all character */
<COMMENT>\n             { line_num++; BEGIN INITIAL;                        }
AreaLightSource         { return AREALIGHTSOURCE;                           }
Attribute               { return ATTRIBUTE;                                 }
AttributeBegin          { return ATTRIBUTEBEGIN;                            }
AttributeEnd            { return ATTRIBUTEEND;                              }
Atmosphere              { return ATMOSPHERE;                                }
Clipping                { return CLIPPING;                                  }
Color                   { return COLOR;                                     }
ConcatTransform         { return CONCATTRANSFORM;                           }
CoordinateSystem        { return COORDINATESYSTEM;                          }
Declare                 { return DECLARE;                                   }
DepthOfField            { return DEPTHOFFIELD;                              }
Displacement            { return DISPLACEMENT;                              }
Display                 { return DISPLAY;                                   }
Exposure                { return EXPOSURE;                                  }
FrameBegin              { return FRAMEBEGIN;                                }
FrameEnd                { return FRAMEEND;                                  }
FrameAspectRatio        { return FRAMEASPECTRATIO;                          }
Format                  { return FORMAT;                                    }
Hider                   { return HIDER;                                     }
Identity                { return IDENTITY;                                  }
Illuminate              { return ILLUMINATE;                                }
Imager                  { return IMAGER;                                    }
LightSource             { return LIGHTSOURCE;                               }
MotionBegin             { return MOTIONBEGIN;                               }
MotionEnd               { return MOTIONEND;                                 }
Opacity                 { return OPACITY;                                   }
Option                  { return OPTION;                                    }
Orientation             { return ORIENTATION;                               }
Perspective             { return PERSPECTIVE;                               }
PixelFilter             { return PIXELFILTER;                               }
PixelSamples            { return PIXELSAMPLES;                              }
PointsPolygons          { return POINTSPOLYGONS;                            }
PointsGeneralPolygons   { return POINTSGENERALPOLYGONS;                     }
Polygon                 { return POLYGON;                                   }
Projection              { return PROJECTION;                                }
Quantize                { return QUANTIZE;                                  }
Rotate                  { return ROTATE;                                    }
Scale                   { return SCALE;                                     }
ScreenWindow            { return SCREENWINDOW;                              }
ShadingInterpolation    { return SHADINGINTERPOLATION;                      }
ShadingRate             { return SHADINGRATE;                               }
Shutter                 { return SHUTTER;                                   }
Sides                   { return SIDES;                                     }
Sphere                  { return SPHERE;                                    }
SubdivisionMesh         { return SUBDIVISIONMESH;                           }
Surface                 { return SURFACE;                                   }
Transform               { return TRANSFORM;                                 }
TransformBegin          { return TRANSFORMBEGIN;                            }
TransformEnd            { return TRANSFORMEND;                              }
Translate               { return TRANSLATE;                                 }
version                 { return RIBVERSION;                                }
WorldBegin              { return WORLDBEGIN;                                }
WorldEnd                { return WORLDEND;                                  }

ReadArchive[ \t]*\"[^\"]*\" {
    char buf[1024];
    sscanf(yytext, "ReadArchive \"%s\"", buf);
    /* '\"' -> '\0' */
    buf[strlen(buf)-1] = '\0';
    read_archive(buf);
}
{IDENT}             { strcpy(yylval.string, yytext);
                      return UNKNOWN;                                       }
<PARAM>"["          { return LBRACKET;                                      }
<PARAM>"]"          { return RBRACKET;                                      }
<PARAM>\"           { str_pos = 0; yy_push_state(STR);                      }
<STR>\\n            { add_string_char('\n');                                }
<STR>\\t            { add_string_char('\t');                                }
<STR>\\r            { add_string_char('\r');                                }
<STR>\\b            { add_string_char('\b');                                }
<STR>\\f            { add_string_char('\f');                                }
<STR>\\\"           { add_string_char('\"');                                }
<STR>\\\\           { add_string_char('\\');                                }
<STR>\\[0-9]{3}     {
    int val = atoi(yytext + 1);
    while (val > 256) val -= 256;
    add_string_char(val);
}
<STR>\\\n           { line_num++;                                           }
<STR>\\.            { add_string_char(yytext[1]);                           }
<STR>\"             { yy_pop_state(); return STRING;                        }
<STR>.              { add_string_char(yytext[0]);                           }
<STR>\n             { printf("unterminated string!");                       }
<PARAM>{NUMBER}     {
    yylval.num = atof(yytext);
    return NUM;
}
{WHITESPACE}        /* skip */
\n                  { line_num++;                                           }
\r\n                { line_num++;                                           }
.                   { printf( "[RIB parse] Illegal character: %c at line %d\n",yytext[0], line_num ); }
<<EOF>>             {
    if (ribstack) {
        ribstack_t *next = ribstack->next;

        yy_delete_buffer(YY_CURRENT_BUFFER);
        fclose(ribin);
        yy_switch_to_buffer(ribstack->state);

        gribfile = ribstack->rib;

        free(ribstack);
        ribstack = next;
    } else {
        yyterminate();
    }
}
%%
