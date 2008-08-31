/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

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
     ATMOSPHERE = 267,
     CLIPPING = 268,
     CONCATTRANSFORM = 269,
     COLOR = 270,
     COORDINATESYSTEM = 271,
     DECLARE = 272,
     DEPTHOFFIELD = 273,
     DISPLACEMENT = 274,
     DISPLAY = 275,
     EXPOSURE = 276,
     FORMAT = 277,
     FRAMEBEGIN = 278,
     FRAMEEND = 279,
     FRAMEASPECTRATIO = 280,
     HIDER = 281,
     IDENTITY = 282,
     ILLUMINATE = 283,
     IMAGER = 284,
     LIGHTSOURCE = 285,
     MOTIONBEGIN = 286,
     MOTIONEND = 287,
     OPACITY = 288,
     OPTION = 289,
     ORIENTATION = 290,
     PERSPECTIVE = 291,
     PIXELFILTER = 292,
     PIXELSAMPLES = 293,
     POINTSPOLYGONS = 294,
     POINTSGENERALPOLYGONS = 295,
     PROJECTION = 296,
     POLYGON = 297,
     QUANTIZE = 298,
     ROTATE = 299,
     RIBVERSION = 300,
     SCALE = 301,
     SCREENWINDOW = 302,
     SHADINGRATE = 303,
     SHADINGINTERPOLATION = 304,
     SHUTTER = 305,
     SIDES = 306,
     SPHERE = 307,
     SUBDIVISIONMESH = 308,
     SURFACE = 309,
     TRANSFORM = 310,
     TRANSFORMBEGIN = 311,
     TRANSFORMEND = 312,
     TRANSLATE = 313,
     WORLDBEGIN = 314,
     WORLDEND = 315,
     HIGH_PRECEDENCE = 316,
     UNKNOWN = 317
   };
#endif
/* Tokens.  */
#define STRING 258
#define ID 259
#define NUM 260
#define LBRACKET 261
#define RBRACKET 262
#define AREALIGHTSOURCE 263
#define ATTRIBUTE 264
#define ATTRIBUTEBEGIN 265
#define ATTRIBUTEEND 266
#define ATMOSPHERE 267
#define CLIPPING 268
#define CONCATTRANSFORM 269
#define COLOR 270
#define COORDINATESYSTEM 271
#define DECLARE 272
#define DEPTHOFFIELD 273
#define DISPLACEMENT 274
#define DISPLAY 275
#define EXPOSURE 276
#define FORMAT 277
#define FRAMEBEGIN 278
#define FRAMEEND 279
#define FRAMEASPECTRATIO 280
#define HIDER 281
#define IDENTITY 282
#define ILLUMINATE 283
#define IMAGER 284
#define LIGHTSOURCE 285
#define MOTIONBEGIN 286
#define MOTIONEND 287
#define OPACITY 288
#define OPTION 289
#define ORIENTATION 290
#define PERSPECTIVE 291
#define PIXELFILTER 292
#define PIXELSAMPLES 293
#define POINTSPOLYGONS 294
#define POINTSGENERALPOLYGONS 295
#define PROJECTION 296
#define POLYGON 297
#define QUANTIZE 298
#define ROTATE 299
#define RIBVERSION 300
#define SCALE 301
#define SCREENWINDOW 302
#define SHADINGRATE 303
#define SHADINGINTERPOLATION 304
#define SHUTTER 305
#define SIDES 306
#define SPHERE 307
#define SUBDIVISIONMESH 308
#define SURFACE 309
#define TRANSFORM 310
#define TRANSFORMBEGIN 311
#define TRANSFORMEND 312
#define TRANSLATE 313
#define WORLDBEGIN 314
#define WORLDEND 315
#define HIGH_PRECEDENCE 316
#define UNKNOWN 317




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 117 "src/lsh/parserib.y"
{
char            string[1024];
float           num;
ri_ptr_array_t *paramarray;
}
/* Line 1529 of yacc.c.  */
#line 179 "src/lsh/parserib.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

