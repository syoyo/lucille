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
     SCREENWINDOW = 300,
     SHADINGRATE = 301,
     SHADINGINTERPOLATION = 302,
     SHUTTER = 303,
     SIDES = 304,
     SPHERE = 305,
     SUBDIVISIONMESH = 306,
     SURFACE = 307,
     TRANSFORM = 308,
     TRANSFORMBEGIN = 309,
     TRANSFORMEND = 310,
     TRANSLATE = 311,
     WORLDBEGIN = 312,
     WORLDEND = 313,
     HIGH_PRECEDENCE = 314,
     UNKNOWN = 315
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
#define SCREENWINDOW 300
#define SHADINGRATE 301
#define SHADINGINTERPOLATION 302
#define SHUTTER 303
#define SIDES 304
#define SPHERE 305
#define SUBDIVISIONMESH 306
#define SURFACE 307
#define TRANSFORM 308
#define TRANSFORMBEGIN 309
#define TRANSFORMEND 310
#define TRANSLATE 311
#define WORLDBEGIN 312
#define WORLDEND 313
#define HIGH_PRECEDENCE 314
#define UNKNOWN 315




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 117 "src/lsh/parserib.y"
{
char            string[1024];
float           num;
ri_ptr_array_t *paramarray;
}
/* Line 1489 of yacc.c.  */
#line 175 "src/lsh/parserib.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

