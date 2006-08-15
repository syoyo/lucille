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
     SURFACE = 258,
     IDENTIFIER = 259,
     NUMBER = 260,
     STRINGCONSTANT = 261,
     VOID = 262,
     FLOAT = 263,
     NORMAL = 264,
     VECTOR = 265,
     COLOR = 266,
     POINT = 267,
     STRING = 268,
     LIGHTSOURCE = 269,
     VARYING = 270,
     UNIFORM = 271,
     ENVIRONMENT = 272,
     TEXTURE = 273,
     RADIANS = 274,
     DEGREES = 275,
     ABS = 276,
     FLOOR = 277,
     CEIL = 278,
     ROUND = 279,
     MIX = 280,
     MOD = 281,
     NOISE = 282,
     STEP = 283,
     SMOOTHSTEP = 284,
     SQRT = 285,
     INVERSESQRT = 286,
     LENGTH = 287,
     SIN = 288,
     ASIN = 289,
     COS = 290,
     ACOS = 291,
     TAN = 292,
     ATAN = 293,
     POW = 294,
     EXP = 295,
     LOG = 296,
     SIGN = 297,
     RANDOM = 298,
     MATH_PI = 299,
     REFRACT = 300,
     OCCLUSION = 301,
     TRACE = 302,
     AMBIENT = 303,
     DIFFUSE = 304,
     SPECULAR = 305,
     PLUSEQ = 306,
     MINUSEQ = 307,
     MULEQ = 308,
     XCOMP = 309,
     YCOMP = 310,
     ZCOMP = 311,
     SETXCOMP = 312,
     SETYCOMP = 313,
     SETZCOMP = 314,
     AREA = 315,
     FOR = 316,
     WHILE = 317,
     IF = 318,
     ELSE = 319,
     ILLUMINANCE = 320,
     OPASSIGN = 321,
     OPVARDEF = 322,
     OPMUL = 323,
     OPDIV = 324,
     OPDOT = 325,
     OPADD = 326,
     OPSUB = 327,
     OPNEG = 328,
     OPFUNC = 329,
     OPFUNCARG = 330,
     OPASSIGNADD = 331,
     OPASSIGNSUB = 332,
     OPASSIGNMUL = 333,
     OPCOND = 334,
     OPPARENT = 335,
     OPLE = 336,
     OPNEQ = 337,
     OPFTOV = 338,
     OPFOR = 339,
     OPFORCOND = 340,
     OPWHILE = 341,
     OPIF = 342,
     OPILLUMINANCE = 343,
     TYPECAST = 344,
     UMINUS = 345
   };
#endif
#define SURFACE 258
#define IDENTIFIER 259
#define NUMBER 260
#define STRINGCONSTANT 261
#define VOID 262
#define FLOAT 263
#define NORMAL 264
#define VECTOR 265
#define COLOR 266
#define POINT 267
#define STRING 268
#define LIGHTSOURCE 269
#define VARYING 270
#define UNIFORM 271
#define ENVIRONMENT 272
#define TEXTURE 273
#define RADIANS 274
#define DEGREES 275
#define ABS 276
#define FLOOR 277
#define CEIL 278
#define ROUND 279
#define MIX 280
#define MOD 281
#define NOISE 282
#define STEP 283
#define SMOOTHSTEP 284
#define SQRT 285
#define INVERSESQRT 286
#define LENGTH 287
#define SIN 288
#define ASIN 289
#define COS 290
#define ACOS 291
#define TAN 292
#define ATAN 293
#define POW 294
#define EXP 295
#define LOG 296
#define SIGN 297
#define RANDOM 298
#define MATH_PI 299
#define REFRACT 300
#define OCCLUSION 301
#define TRACE 302
#define AMBIENT 303
#define DIFFUSE 304
#define SPECULAR 305
#define PLUSEQ 306
#define MINUSEQ 307
#define MULEQ 308
#define XCOMP 309
#define YCOMP 310
#define ZCOMP 311
#define SETXCOMP 312
#define SETYCOMP 313
#define SETZCOMP 314
#define AREA 315
#define FOR 316
#define WHILE 317
#define IF 318
#define ELSE 319
#define ILLUMINANCE 320
#define OPASSIGN 321
#define OPVARDEF 322
#define OPMUL 323
#define OPDIV 324
#define OPDOT 325
#define OPADD 326
#define OPSUB 327
#define OPNEG 328
#define OPFUNC 329
#define OPFUNCARG 330
#define OPASSIGNADD 331
#define OPASSIGNSUB 332
#define OPASSIGNMUL 333
#define OPCOND 334
#define OPPARENT 335
#define OPLE 336
#define OPNEQ 337
#define OPFTOV 338
#define OPFOR 339
#define OPFORCOND 340
#define OPWHILE 341
#define OPIF 342
#define OPILLUMINANCE 343
#define TYPECAST 344
#define UMINUS 345




#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 350 "parsesl.y"
typedef union YYSTYPE {
	char   *string;
	node_t *np;
	double  fval;
} YYSTYPE;
/* Line 1285 of yacc.c.  */
#line 223 "y.tab.h"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;



