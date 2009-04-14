-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.Sema
---- Copyright   :  (c) Syoyo Fujita
---- License     :  Modified BSD
----
---- Maintainer  :  syoyo@lucillerender.org
---- Stability   :  experimental
---- Portability :  GHC 6.8
----
---- RSLParser   :  Semantic analysis module for RenderMan SL.
----                This module is interoperated with RSL.Parser.
----
-------------------------------------------------------------------------------

{-
 
 List of builtin varibles and functions based on RI spec 3.2

-}

module RSL.Sema where

import RSL.AST

--
-- | List of RSL builtin variables
--
builtinShaderVariables :: [Symbol]
builtinShaderVariables =
  [ (SymVar "Ci"        TyColor  Varying KindBuiltinVariable)
  , (SymVar "Oi"        TyColor  Varying KindBuiltinVariable)
  , (SymVar "Cs"        TyColor  Varying KindBuiltinVariable)
  , (SymVar "Os"        TyColor  Varying KindBuiltinVariable)
  , (SymVar "P"         TyPoint  Varying KindBuiltinVariable)
  , (SymVar "dPdu"      TyVector Varying KindBuiltinVariable)
  , (SymVar "dPdv"      TyVector Varying KindBuiltinVariable)
  , (SymVar "Ps"        TyPoint  Varying KindBuiltinVariable)
  , (SymVar "I"         TyVector Varying KindBuiltinVariable)
  , (SymVar "E"         TyPoint  Uniform KindBuiltinVariable)
  , (SymVar "L"         TyVector Uniform KindBuiltinVariable)
  , (SymVar "N"         TyNormal Varying KindBuiltinVariable)
  , (SymVar "Ng"        TyNormal Varying KindBuiltinVariable)
  , (SymVar "Ns"        TyNormal Varying KindBuiltinVariable)
  , (SymVar "Cl"        TyColor  Varying KindBuiltinVariable)
  , (SymVar "Ol"        TyColor  Varying KindBuiltinVariable)
  , (SymVar "s"         TyFloat  Varying KindBuiltinVariable)
  , (SymVar "t"         TyFloat  Varying KindBuiltinVariable)
  , (SymVar "u"         TyFloat  Varying KindBuiltinVariable)
  , (SymVar "v"         TyFloat  Varying KindBuiltinVariable)
  , (SymVar "du"        TyFloat  Varying KindBuiltinVariable)
  , (SymVar "dv"        TyFloat  Varying KindBuiltinVariable)
  , (SymVar "ncompos"   TyFloat  Uniform KindBuiltinVariable)
  , (SymVar "time"      TyFloat  Uniform KindBuiltinVariable)
  , (SymVar "dtime"     TyFloat  Uniform KindBuiltinVariable)
  , (SymVar "dPdtime"   TyVector Varying KindBuiltinVariable)

  -- extension for lucille.
  
  , (SymVar "x"  TyFloat  Varying KindBuiltinVariable)
  , (SymVar "y"  TyFloat  Varying KindBuiltinVariable)
  , (SymVar "z"  TyFloat  Varying KindBuiltinVariable)
  , (SymVar "w"  TyFloat  Varying KindBuiltinVariable)
  , (SymVar "sx" TyInt    Varying KindBuiltinVariable)
  , (SymVar "sy" TyInt    Varying KindBuiltinVariable)

  -- Constant
  , (SymVar "PI" TyFloat  Uniform KindBuiltinVariable)

  ] -- More is TODO
  

builtinOutputShaderVariables :: [Symbol]
builtinOutputShaderVariables =
  [ (SymVar "Ci" TyColor  Varying KindBuiltinVariable)
  , (SymVar "Oi" TyColor  Varying KindBuiltinVariable)
  ]

builtinShaderFunctions :: [Symbol]
builtinShaderFunctions =
  [ 
  -- [15.1] Mathematical Functions
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
  , (SymBuiltinFunc "floor"          f [f]       [])
  , (SymBuiltinFunc "ceil"           f [f]       [])
  , (SymBuiltinFunc "round"          f [f]       [])
  , (SymBuiltinFunc "step"           f [f, f]    [])
  , (SymBuiltinFunc "smoothstep"     f [f, f, f] [])
  -- TODO...
  , (SymBuiltinFunc "random"         f []        [])
  , (SymBuiltinFunc "random"         c []        [])
  , (SymBuiltinFunc "random"         p []        [])
  , (SymBuiltinFunc "noise"          f [f]       [])
  , (SymBuiltinFunc "noise"          f [p]       [])
  , (SymBuiltinFunc "noise"          f [v]       [])
  , (SymBuiltinFunc "noise"          f [n]       [])
  , (SymBuiltinFunc "noise"          f [f, f]    [])
  , (SymBuiltinFunc "pnoise"         f [f, f]    [])
  , (SymBuiltinFunc "pnoise"         f [f, f, f, f]    [])
  , (SymBuiltinFunc "pnoise"         f [p, p]    [])
  , (SymBuiltinFunc "pnoise"         f [p, f, p, f]    [])
  , (SymBuiltinFunc "pnoise"         c [f, f]    [])
  , (SymBuiltinFunc "pnoise"         c [f, f, f, f]    [])
  , (SymBuiltinFunc "pnoise"         c [p, p]    [])
  , (SymBuiltinFunc "pnoise"         c [p, f, p, f]    [])
  , (SymBuiltinFunc "pnoise"         p [f, f]    [])
  , (SymBuiltinFunc "pnoise"         p [f, f, f, f]    [])
  , (SymBuiltinFunc "pnoise"         p [p, p]    [])
  , (SymBuiltinFunc "pnoise"         p [p, f, p, f]    [])
  , (SymBuiltinFunc "pnoise"         v [f, f]    [])
  , (SymBuiltinFunc "pnoise"         v [f, f, f, f]    [])
  , (SymBuiltinFunc "pnoise"         v [p, p]    [])
  , (SymBuiltinFunc "pnoise"         v [p, f, p, f]    [])
  , (SymBuiltinFunc "cellnoise"      f [f]       [])
  , (SymBuiltinFunc "cellnoise"      f [f, f]    [])
  , (SymBuiltinFunc "cellnoise"      f [p]       [])
  , (SymBuiltinFunc "cellnoise"      f [p, f]    [])
  , (SymBuiltinFunc "cellnoise"      c [f]       [])
  , (SymBuiltinFunc "cellnoise"      c [f, f]    [])
  , (SymBuiltinFunc "cellnoise"      c [p]       [])
  , (SymBuiltinFunc "cellnoise"      c [p, f]    [])
  , (SymBuiltinFunc "cellnoise"      p [f]       [])
  , (SymBuiltinFunc "cellnoise"      p [f, f]    [])
  , (SymBuiltinFunc "cellnoise"      p [p]       [])
  , (SymBuiltinFunc "cellnoise"      p [p, f]    [])
  , (SymBuiltinFunc "cellnoise"      v [f]       [])
  , (SymBuiltinFunc "cellnoise"      v [f, f]    [])
  , (SymBuiltinFunc "cellnoise"      v [p]       [])
  , (SymBuiltinFunc "cellnoise"      v [p, f]    [])

  -- [15.2] Geometric Functions
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
  , (SymBuiltinFunc "length"         f [p]       [])
  , (SymBuiltinFunc "length"         f [n]       [])
  , (SymBuiltinFunc "normalize"      v [v]       [])
  , (SymBuiltinFunc "normalize"      v [n]       [])
  , (SymBuiltinFunc "distance"       f [p, p]    [])
  -- , (SymFunc "ptlined"    f [p, p] [])
  -- , (SymFunc "rotate"    f [p, p] [])
  , (SymBuiltinFunc "area"           f [p]       [])
  , (SymBuiltinFunc "faceforward"    v [v, v]    []) -- has opt arg
  , (SymBuiltinFunc "faceforward"    v [n, v]    []) -- has opt arg
  , (SymBuiltinFunc "faceforward"    v [n, p]    []) -- has opt arg
  , (SymBuiltinFunc "faceforward"    v [v, p]    []) -- has opt arg
  , (SymBuiltinFunc "reflect"        v [v, v]    [])
  , (SymBuiltinFunc "reflect"        v [v, n]    [])
  , (SymBuiltinFunc "reflect"        v [v, p]    [])
  , (SymBuiltinFunc "refract"        v [v, v, f] [])
  , (SymBuiltinFunc "refract"        v [v, n, f] [])
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
  , (SymBuiltinFunc "vtransform"     v [s, s, v] []) 
  , (SymBuiltinFunc "depth"          f [p]       []) 
  , (SymBuiltinFunc "calculatenormal" p [p]       [])

  -- [15.3] Color Functions
  , (SymBuiltinFunc "comp"           f [c, f]    []) 
  -- , (SymBuiltinFunc "setcomp"        void [c, f, f]    []) 
  , (SymBuiltinFunc "mix"            c [c, c, f] []) 

  , (SymBuiltinFunc "Du"             f [f]       []) 
  , (SymBuiltinFunc "Du"             c [c]       []) 
  , (SymBuiltinFunc "Du"             v [p]       []) 
  , (SymBuiltinFunc "Du"             v [v]       []) 
  , (SymBuiltinFunc "Dv"             f [f]       []) 
  , (SymBuiltinFunc "Dv"             c [c]       []) 
  , (SymBuiltinFunc "Dv"             v [p]       []) 
  , (SymBuiltinFunc "Dv"             v [v]       []) 
  , (SymBuiltinFunc "Deriv"          f [f, f]    []) 
  , (SymBuiltinFunc "Deriv"          c [c, f]    []) 
  , (SymBuiltinFunc "Deriv"          p [p, f]    []) 
  , (SymBuiltinFunc "Deriv"          v [v, f]    []) 

  -- [15.5] String Functions
  --
  -- FIXME: Need a special treatment for string functions.
  --
  , (SymBuiltinFunc "printf"         void [s] []) 

  -- [15.6] Shading and Lighting Functions
  , (SymBuiltinFunc "ambient"        c []        [])
  , (SymBuiltinFunc "diffuse"        c [n]       [])
  , (SymBuiltinFunc "diffuse"        c [p]       [])
  , (SymBuiltinFunc "diffuse"        c [v]       [])
  , (SymBuiltinFunc "specular"       c [n, v, f] [])
  , (SymBuiltinFunc "specular"       c [p, p, f] [])
  , (SymBuiltinFunc "specularbrdf"   c [v, n, v, f] [])
  , (SymBuiltinFunc "phong"          c [n, v, f] [])
  , (SymBuiltinFunc "trace"          c [p, p]    [])
  , (SymBuiltinFunc "trace"          c [p, v]    [])

  -- [15.7] Texture Mapping Functions
  , (SymBuiltinFunc "texture"        c [s]       [])
  , (SymBuiltinFunc "environment"    c [s]       [])
  , (SymBuiltinFunc "environment"    c [s, v]    [])

  -- extension for lucille 
  , (SymBuiltinFunc "save_cache" void [i, i, i, c]      [])
  , (SymBuiltinFunc "save_cache" void [i, i, i, f]      [])
  , (SymBuiltinFunc "load_cache" c    [i, i, i]         [])
  , (SymBuiltinFunc "load_cache" f    [i, i, i]         [])
  , (SymBuiltinFunc "turb"       c    [p]               [])
  , (SymBuiltinFunc "occlusion"  f    [p, n]            [])
  , (SymBuiltinFunc "occlusion"  f    [p, n, f]         [])

  ] -- More is TODO

  where

    f = TyFloat
    c = TyColor
    v = TyVector
    p = TyPoint
    n = TyNormal
    s = TyString
    m = TyMatrix
    i = TyInt
    void = TyVoid


lookupVariable :: [Symbol] -> String -> [Symbol]
lookupVariable syms name = filter (\(SymVar sname _ _ _) -> sname == name) syms

lookupBuiltinFunc :: [Symbol] -> String -> [Symbol]
lookupBuiltinFunc syms name = filter (\(SymBuiltinFunc fname _ _ _) -> fname == name) syms

lookupBuiltinFuncWithArgumentSignature :: [Symbol] -> String -> [Type] -> [Symbol]
lookupBuiltinFuncWithArgumentSignature syms name argTys = 
  filter (\(SymBuiltinFunc fname _ tys _) -> (fname == name) && (tys == argTys) ) syms
