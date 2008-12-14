-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.Sema
---- Copyright   :  (c) Syoyo Fujita
---- License     :  BSD-style
----
---- Maintainer  :  syoyo@lucillerender.org
---- Stability   :  experimental
---- Portability :  GHC 6.8
----
---- RSLParser   :  Semantic analysis module for RenderMan SL.
----                This module is interoperated with RSL.Parser.
----
-------------------------------------------------------------------------------

module RSL.Sema where

import RSL.AST

--
-- | List of RSL builtin variables
--
builtinShaderVariables :: [Symbol]
builtinShaderVariables =
  [ (SymVar "Ci" TyColor  Varying KindVariable)
  , (SymVar "Oi" TyColor  Varying KindVariable)
  , (SymVar "Cs" TyColor  Varying KindVariable)
  , (SymVar "Os" TyColor  Varying KindVariable)
  , (SymVar "P"  TyPoint  Varying KindVariable)
  , (SymVar "I"  TyVector Varying KindVariable)
  , (SymVar "E"  TyPoint  Uniform KindVariable)
  , (SymVar "N"  TyNormal Varying KindVariable)
  , (SymVar "Ng" TyNormal Varying KindVariable)
  , (SymVar "Cl" TyColor  Varying KindVariable)
  , (SymVar "Ol" TyColor  Varying KindVariable)
  ] -- More is TODO
  

builtinShaderFunctions :: [Symbol]
builtinShaderFunctions =
  [ 
  -- 15.1 Mathematical Functions
    (SymFunc "radians"     f [f]    [])
  , (SymFunc "degrees"     f [f]    [])
  , (SymFunc "sin"         f [f]    [])
  , (SymFunc "asin"        f [f]    [])
  , (SymFunc "cos"         f [f]    [])
  , (SymFunc "acos"        f [f]    [])
  , (SymFunc "tan"         f [f]    [])
  , (SymFunc "atan"        f [f]    [])
  , (SymFunc "atan"        f [f, f] [])
  , (SymFunc "pow"         f [f, f] [])
  , (SymFunc "exp"         f [f]    [])
  , (SymFunc "sqrt"        f [f]    [])
  , (SymFunc "inversesqrt" f [f]    [])
  , (SymFunc "log"         f [f]    [])
  , (SymFunc "log"         f [f, f] [])
  , (SymFunc "mod"         f [f, f] [])
  , (SymFunc "abs"         f [f]    [])
  , (SymFunc "sign"        f [f]    [])
  , (SymFunc "min"         f [f, f] []) -- float, point, vector, normal, color
  , (SymFunc "max"         f [f, f] []) -- float, point, vector, normal, color
  , (SymFunc "clamp"       f [f, f] []) -- float, point, vector, normal, color
  , (SymFunc "mix"         f [f, f, f] []) -- float, point, vector, normal, color
  -- TODO...

  -- 15.2 Geometric Functions
  , (SymFunc "xcomp"       f [v]       [])
  , (SymFunc "xcomp"       f [p]       [])
  , (SymFunc "xcomp"       f [n]       [])
  , (SymFunc "ycomp"       f [v]       [])
  , (SymFunc "ycomp"       f [p]       [])
  , (SymFunc "ycomp"       f [n]       [])
  , (SymFunc "zcomp"       f [v]       [])
  , (SymFunc "zcomp"       f [p]       [])
  , (SymFunc "zcomp"       f [n]       [])
  -- , (SymFunc "setxcomp"       f [n]    [])
  -- , (SymFunc "setycomp"       f [n]    [])
  -- , (SymFunc "setzcomp"       f [n]    [])
  , (SymFunc "length"      f [v]       [])
  , (SymFunc "normalize"   v [v]       [])
  , (SymFunc "distance"    f [p, p]    [])
  -- , (SymFunc "ptlined"    f [p, p] [])
  -- , (SymFunc "rotate"    f [p, p] [])
  , (SymFunc "area"        f [p]       [])
  , (SymFunc "faceforward" v [v, v]    []) -- has opt arg
  , (SymFunc "reflect"     v [v, v]    [])
  , (SymFunc "refract"     v [v, v, f] [])
  , (SymFunc "fresnel"     v [v, v, f, f, f] [])  -- TODO
  , (SymFunc "transform"   p [s, p]    []) 
  , (SymFunc "transform"   p [s, s, p] []) 
  , (SymFunc "transform"   p [m, p]    []) 
  , (SymFunc "transform"   p [s, m, p] []) 
  , (SymFunc "transform"   v [s, v]    []) 
  , (SymFunc "transform"   v [s, s, v] []) 
  , (SymFunc "transform"   v [m, v]    []) 
  , (SymFunc "transform"   v [s, m, v] []) 
  , (SymFunc "transform"   n [s, n]    []) 
  , (SymFunc "transform"   n [s, s, n] []) 
  , (SymFunc "transform"   n [m, n]    []) 
  , (SymFunc "transform"   n [s, m, n] []) 
  , (SymFunc "depth"       f [p]       []) 
  , (SymFunc "depth"       f [p]       []) 

  -- 15.6 Shading and Lighting Functions
  , (SymFunc "ambient"     c []     [])
  , (SymFunc "diffuse"     c []     [])


  ] -- More is TODO

  where

    f = TyFloat
    c = TyColor
    v = TyVector
    p = TyPoint
    n = TyNormal
    s = TyString
    m = TyMatrix
    void = TyVoid


