/* A Bison parser, made by GNU Bison 1.875d.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     STRING = 258,
     ID = 259,
     NUM = 260,
     LBRACKET = 261,
     RBRACKET = 262,
     AREALIGHTSOURCE = 263,
     ATTRIBUTE = 264,
     ATTRIBUTEBEGIN = 265,
     ATTRIBUTEEND = 266,
     CLIPPING = 267,
     CONCATTRANSFORM = 268,
     COLOR = 269,
     COORDINATESYSTEM = 270,
     DECLARE = 271,
     DEPTHOFFIELD = 272,
     DISPLAY = 273,
     EXPOSURE = 274,
     FORMAT = 275,
     FRAMEBEGIN = 276,
     FRAMEEND = 277,
     FRAMEASPECTRATIO = 278,
     HIDER = 279,
     IDENTITY = 280,
     ILLUMINATE = 281,
     IMAGER = 282,
     LIGHTSOURCE = 283,
     MOTIONBEGIN = 284,
     MOTIONEND = 285,
     OPACITY = 286,
     OPTION = 287,
     ORIENTATION = 288,
     PERSPECTIVE = 289,
     PIXELFILTER = 290,
     PIXELSAMPLES = 291,
     POINTSPOLYGONS = 292,
     POINTSGENERALPOLYGONS = 293,
     PROJECTION = 294,
     POLYGON = 295,
     QUANTIZE = 296,
     ROTATE = 297,
     RIBVERSION = 298,
     SCALE = 299,
     SHADINGRATE = 300,
     SHADINGINTERPOLATION = 301,
     SHUTTER = 302,
     SIDES = 303,
     SPHERE = 304,
     SUBDIVISIONMESH = 305,
     SURFACE = 306,
     TRANSFORM = 307,
     TRANSFORMBEGIN = 308,
     TRANSFORMEND = 309,
     TRANSLATE = 310,
     WORLDBEGIN = 311,
     WORLDEND = 312,
     HIGH_PRECEDENCE = 313,
     UNKNOWN = 314
   };
#endif
#define STRING 258
#define ID 259
#define NUM 260
#define LBRACKET 261
#define RBRACKET 262
#define AREALIGHTSOURCE 263
#define ATTRIBUTE 264
#define ATTRIBUTEBEGIN 265
#define ATTRIBUTEEND 266
#define CLIPPING 267
#define CONCATTRANSFORM 268
#define COLOR 269
#define COORDINATESYSTEM 270
#define DECLARE 271
#define DEPTHOFFIELD 272
#define DISPLAY 273
#define EXPOSURE 274
#define FORMAT 275
#define FRAMEBEGIN 276
#define FRAMEEND 277
#define FRAMEASPECTRATIO 278
#define HIDER 279
#define IDENTITY 280
#define ILLUMINATE 281
#define IMAGER 282
#define LIGHTSOURCE 283
#define MOTIONBEGIN 284
#define MOTIONEND 285
#define OPACITY 286
#define OPTION 287
#define ORIENTATION 288
#define PERSPECTIVE 289
#define PIXELFILTER 290
#define PIXELSAMPLES 291
#define POINTSPOLYGONS 292
#define POINTSGENERALPOLYGONS 293
#define PROJECTION 294
#define POLYGON 295
#define QUANTIZE 296
#define ROTATE 297
#define RIBVERSION 298
#define SCALE 299
#define SHADINGRATE 300
#define SHADINGINTERPOLATION 301
#define SHUTTER 302
#define SIDES 303
#define SPHERE 304
#define SUBDIVISIONMESH 305
#define SURFACE 306
#define TRANSFORM 307
#define TRANSFORMBEGIN 308
#define TRANSFORMEND 309
#define TRANSLATE 310
#define WORLDBEGIN 311
#define WORLDEND 312
#define HIGH_PRECEDENCE 313
#define UNKNOWN 314




#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 117 "parserib.y"
typedef union YYSTYPE {
char            string[1024];
float           num;
ri_ptr_array_t *paramarray;
} YYSTYPE;
/* Line 1285 of yacc.c.  */
#line 161 "y.tab.h"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;



