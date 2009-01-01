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
  [ (SymVar "Ci" TyColor  Varying KindBuiltinVariable)
  , (SymVar "Oi" TyColor  Varying KindBuiltinVariable)
  , (SymVar "Cs" TyColor  Varying KindBuiltinVariable)
  , (SymVar "Os" TyColor  Varying KindBuiltinVariable)
  , (SymVar "P"  TyPoint  Varying KindBuiltinVariable)
  , (SymVar "I"  TyVector Varying KindBuiltinVariable)
  , (SymVar "E"  TyPoint  Uniform KindBuiltinVariable)
  , (SymVar "N"  TyNormal Varying KindBuiltinVariable)
  , (SymVar "Ng" TyNormal Varying KindBuiltinVariable)
  , (SymVar "Cl" TyColor  Varying KindBuiltinVariable)
  , (SymVar "Ol" TyColor  Varying KindBuiltinVariable)
  , (SymVar "s"  TyFloat  Varying KindBuiltinVariable)
  , (SymVar "t"  TyFloat  Varying KindBuiltinVariable)
  ] -- More is TODO
  

builtinOutputShaderVariables :: [Symbol]
builtinOutputShaderVariables =
  [ (SymVar "Ci" TyColor  Varying KindBuiltinVariable)
  , (SymVar "Oi" TyColor  Varying KindBuiltinVariable)
  ]

builtinShaderFunctions :: [Symbol]
builtinShaderFunctions =
  [ 
  -- 15.1 Mathematical Functions
    (SymBuiltinFunc "radians"        f [f]    [])
  , (SymBuiltinFunc "degrees"        f [f]    [])
  , (SymBuiltinFunc "sin"            f [f]    [])
  , (SymBuiltinFunc "asin"           f [f]    [])
  , (SymBuiltinFunc "cos"            f [f]    [])
  , (SymBuiltinFunc "acos"           f [f]    [])
  , (SymBuiltinFunc "tan"            f [f]    [])
  , (SymBuiltinFunc "atan"           f [f]    [])
  , (SymBuiltinFunc "atan"           f [f, f] [])
  , (SymBuiltinFunc "pow"            f [f, f] [])
  , (SymBuiltinFunc "exp"            f [f]    [])
  , (SymBuiltinFunc "sqrt"           f [f]    [])
  , (SymBuiltinFunc "inversesqrt"    f [f]    [])
  , (SymBuiltinFunc "log"            f [f]    [])
  , (SymBuiltinFunc "log"            f [f, f] [])
  , (SymBuiltinFunc "mod"            f [f, f] [])
  , (SymBuiltinFunc "abs"            f [f]    [])
  , (SymBuiltinFunc "sign"           f [f]    [])
  -- TODO: float, point, vector, normal, color
  , (SymBuiltinFunc "min"            f [f, f] [])
  -- TODO: float, point, vector, normal, color
  , (SymBuiltinFunc "max"            f [f, f] [])
  -- TODO: float, point, vector, normal, color
  , (SymBuiltinFunc "clamp"          f [f, f] [])
  -- float, point, vector, normal, color
  , (SymBuiltinFunc "mix"            f [f, f, f] [])
  -- TODO...
  , (SymBuiltinFunc "random"         f []        [])
  , (SymBuiltinFunc "random"         c []        [])
  , (SymBuiltinFunc "random"         p []        [])
  , (SymBuiltinFunc "noise"          f [f]       [])
  -- TODO...

  -- 15.2 Geometric Functions
  , (SymBuiltinFunc "xcomp"          f [v]       [])
  , (SymBuiltinFunc "xcomp"          f [p]       [])
  , (SymBuiltinFunc "xcomp"          f [n]       [])
  , (SymBuiltinFunc "ycomp"          f [v]       [])
  , (SymBuiltinFunc "ycomp"          f [p]       [])
  , (SymBuiltinFunc "ycomp"          f [n]       [])
  , (SymBuiltinFunc "zcomp"          f [v]       [])
  , (SymBuiltinFunc "zcomp"          f [p]       [])
  , (SymBuiltinFunc "zcomp"          f [n]       [])
  -- , (SymFunc "setxcomp"       f [n]    [])
  -- , (SymFunc "setycomp"       f [n]    [])
  -- , (SymFunc "setzcomp"       f [n]    [])
  , (SymBuiltinFunc "length"         f [v]       [])
  , (SymBuiltinFunc "normalize"      v [v]       [])
  , (SymBuiltinFunc "distance"       f [p, p]    [])
  -- , (SymFunc "ptlined"    f [p, p] [])
  -- , (SymFunc "rotate"    f [p, p] [])
  , (SymBuiltinFunc "area"           f [p]       [])
  , (SymBuiltinFunc "faceforward"    v [v, v]    []) -- has opt arg
  , (SymBuiltinFunc "reflect"        v [v, v]    [])
  , (SymBuiltinFunc "refract"        v [v, v, f] [])
  , (SymBuiltinFunc "fresnel"        v [v, v, f, f, f] [])  -- TODO
  , (SymBuiltinFunc "transform"      p [s, p]    []) 
  , (SymBuiltinFunc "transform"      p [s, s, p] []) 
  , (SymBuiltinFunc "transform"      p [m, p]    []) 
  , (SymBuiltinFunc "transform"      p [s, m, p] []) 
  , (SymBuiltinFunc "transform"      v [s, v]    []) 
  , (SymBuiltinFunc "transform"      v [s, s, v] []) 
  , (SymBuiltinFunc "transform"      v [m, v]    []) 
  , (SymBuiltinFunc "transform"      v [s, m, v] []) 
  , (SymBuiltinFunc "transform"      n [s, n]    []) 
  , (SymBuiltinFunc "transform"      n [s, s, n] []) 
  , (SymBuiltinFunc "transform"      n [m, n]    []) 
  , (SymBuiltinFunc "transform"      n [s, m, n] []) 
  , (SymBuiltinFunc "depth"          f [p]       []) 
  , (SymBuiltinFunc "depth"          f [p]       []) 

  -- 15.6 Shading and Lighting Functions
  , (SymBuiltinFunc "ambient"        c []        [])
  , (SymBuiltinFunc "diffuse"        c [n]       [])
  , (SymBuiltinFunc "specular"       c [n, v, f] [])
  , (SymBuiltinFunc "specularbrdf"   c [v, n, v, f] [])
  , (SymBuiltinFunc "phong"          c [n, v, f] [])
  , (SymBuiltinFunc "trace"          c [p, p]    [])


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


lookupVariable :: [Symbol] -> String -> [Symbol]
lookupVariable syms name = filter (\(SymVar sname _ _ _) -> sname == name) syms

lookupBuiltinFunc :: [Symbol] -> String -> [Symbol]
lookupBuiltinFunc syms name = filter (\(SymBuiltinFunc fname _ _ _) -> fname == name) syms
