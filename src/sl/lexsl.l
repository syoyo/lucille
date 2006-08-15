%{
#include <string.h>

#include "tree.h"
#include "parsesl.h" /* tokens */

int nlines = 0;

%}
IDENT  [a-zA-Z_][a-zA-Z_0-9]*
exp    [eE][-+]?[0-9]+
NUM    [-+]?([0-9]+|(([0-9]+)|([0-9]+\.[0-9]*)|(\.[0-9]+)){exp}?)
STR    \"([^\"\n]|\"\")*\"

%x COMMENT

%%

"float"		{ return FLOAT;					}
"vector"        { return VECTOR;				}
"normal"	{ return NORMAL;				}
"point"		{ return POINT;					}
"color"		{ return COLOR;					}
"string"	{ return STRING;				}

"uniform"	{ return UNIFORM;				}
"varying"	{ return VARYING;				}

"surface"	{ return SURFACE;				}

"for"		{ return FOR;					}
"while"		{ return WHILE;					}
"if"		{ return IF;					}
"else"		{ return ELSE;					}

"illuminance"	{ return ILLUMINANCE;				}

"ambient"	{ return AMBIENT;				}
"diffuse"	{ return DIFFUSE;				}
"specular"	{ return SPECULAR;				}
"texture"	{ return TEXTURE;				}
"environment"	{ return ENVIRONMENT;				}
"occlusion"	{ return OCCLUSION;				}
"trace"		{ return TRACE;					}

"radians"	{ return RADIANS;				}
"degrees"	{ return DEGREES;				}
"abs"		{ return ABS;					}
"floor"		{ return FLOOR;					}
"ceil"		{ return CEIL;					}
"round"		{ return ROUND;					}
"mix"		{ return MIX;					}
"mod"		{ return MOD;					}
"noise"		{ return NOISE;					}
"step"		{ return STEP;					}
"smoothstep"	{ return SMOOTHSTEP;				}
"sqrt"		{ return SQRT;					}
"sin"		{ return SIN;					}
"asin"		{ return ASIN;					}
"cos"		{ return COS;					}
"acos"		{ return ACOS;					}
"tan"		{ return TAN;					}
"atan"		{ return ATAN;					}
"pow"		{ return POW;					}
"exp"		{ return EXP;					}
"inversesqrt"	{ return INVERSESQRT;				}
"log"		{ return LOG;					}
"sign"		{ return SIGN;					}
"random"	{ return RANDOM;				}
"PI"		{ return MATH_PI;				}

"refract"	{ return REFRACT;				}

"area"		{ return AREA;					}
"length"	{ return LENGTH;				}
"xcomp"		{ return XCOMP;					}
"ycomp"		{ return YCOMP;					}
"zcomp"		{ return ZCOMP;					}
"setxcomp"	{ return SETXCOMP;				}
"setycomp"	{ return SETYCOMP;				}
"setzcomp"	{ return SETZCOMP;				}

"+="		{ return PLUSEQ;				}
"-="		{ return MINUSEQ;				}
"*="		{ return MULEQ;					}

"!="		{ return OPNEQ;					}

"/*"		BEGIN(COMMENT);
<COMMENT>[^\n]	/* do noting */
<COMMENT>\n	{ nlines++;					}
<COMMENT><<EOF>> { yyerror("EOF in comment");			}
<COMMENT>"*/"	BEGIN(INITIAL);

{IDENT}		{ yylval.string = strdup((const char *)yytext);
		  return IDENTIFIER;				}
{NUM}		{ yylval.fval = atof(yytext);
		  return NUMBER;				}
{STR}		{ yylval.string = strdup((const char *)yytext);
		  return STRINGCONSTANT;			}
[ \t]+		  /* blank, tab */
\n		{ /* new line */
		  nlines++;					}
.		{ return yytext[0];				}
%%
