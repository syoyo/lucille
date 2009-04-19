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

 TODO: - Support "output" spec
       - Support array type
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
    (SymBuiltinFunc "radians"        f [f]    Nothing)
  , (SymBuiltinFunc "degrees"        f [f]    Nothing)
  , (SymBuiltinFunc "sin"            f [f]    Nothing)
  , (SymBuiltinFunc "asin"           f [f]    Nothing)
  , (SymBuiltinFunc "cos"            f [f]    Nothing)
  , (SymBuiltinFunc "acos"           f [f]    Nothing)
  , (SymBuiltinFunc "tan"            f [f]    Nothing)
  , (SymBuiltinFunc "atan"           f [f]    Nothing)
  , (SymBuiltinFunc "atan"           f [f, f] Nothing)
  , (SymBuiltinFunc "pow"            f [f, f] Nothing)
  , (SymBuiltinFunc "exp"            f [f]    Nothing)
  , (SymBuiltinFunc "sqrt"           f [f]    Nothing)
  , (SymBuiltinFunc "inversesqrt"    f [f]    Nothing)
  , (SymBuiltinFunc "log"            f [f]    Nothing)
  , (SymBuiltinFunc "log"            f [f, f] Nothing)
  , (SymBuiltinFunc "mod"            f [f, f] Nothing)
  , (SymBuiltinFunc "abs"            f [f]    Nothing)
  , (SymBuiltinFunc "sign"           f [f]    Nothing)
  -- TODO: float, point, vector, normal, color
  , (SymBuiltinFunc "min"            f [f, f] Nothing)
  -- TODO: float, point, vector, normal, color
  , (SymBuiltinFunc "max"            f [f, f] Nothing)
  -- TODO: float, point, vector, normal, color
  , (SymBuiltinFunc "clamp"          f [f, f] Nothing)
  -- float, point, vector, normal, color
  , (SymBuiltinFunc "mix"            f [f, f, f] Nothing)
  , (SymBuiltinFunc "floor"          f [f]       Nothing)
  , (SymBuiltinFunc "ceil"           f [f]       Nothing)
  , (SymBuiltinFunc "round"          f [f]       Nothing)
  , (SymBuiltinFunc "step"           f [f, f]    Nothing)
  , (SymBuiltinFunc "smoothstep"     f [f, f, f] Nothing)
  , (SymBuiltinFunc "filterstep"     f [f, f, f] Nothing)
  -- TODO: filterstep(f, f, parameterlist)
  -- TODO: filterstep(f, f, f, parameterlist)
  , (SymBuiltinFunc "spline"         f [f, f, f] (Just f))
  , (SymBuiltinFunc "spline"         c [f, c, c] (Just c))
  , (SymBuiltinFunc "spline"         p [f, p, p] (Just p))
  , (SymBuiltinFunc "spline"         v [f, v, v] (Just v))
  -- TODO: support spline(f, f[]), spline([string basis], ...)
  , (SymBuiltinFunc "Du"             f [f]       Nothing) 
  , (SymBuiltinFunc "Du"             c [c]       Nothing) 
  , (SymBuiltinFunc "Du"             v [p]       Nothing) 
  , (SymBuiltinFunc "Du"             v [v]       Nothing) 
  , (SymBuiltinFunc "Dv"             f [f]       Nothing) 
  , (SymBuiltinFunc "Dv"             c [c]       Nothing) 
  , (SymBuiltinFunc "Dv"             v [p]       Nothing) 
  , (SymBuiltinFunc "Dv"             v [v]       Nothing) 
  , (SymBuiltinFunc "Deriv"          f [f, f]    Nothing) 
  , (SymBuiltinFunc "Deriv"          c [c, f]    Nothing) 
  , (SymBuiltinFunc "Deriv"          p [p, f]    Nothing) 
  , (SymBuiltinFunc "Deriv"          v [v, f]    Nothing) 
  , (SymBuiltinFunc "random"         f []        Nothing)
  , (SymBuiltinFunc "random"         c []        Nothing)
  , (SymBuiltinFunc "random"         p []        Nothing)
  , (SymBuiltinFunc "noise"          f [f]       Nothing)
  , (SymBuiltinFunc "noise"          f [p]       Nothing)
  , (SymBuiltinFunc "noise"          f [v]       Nothing)
  , (SymBuiltinFunc "noise"          f [n]       Nothing)
  , (SymBuiltinFunc "noise"          f [f, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         f [f, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         f [f, f, f, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         f [p, p]    Nothing)
  , (SymBuiltinFunc "pnoise"         f [p, f, p, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         c [f, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         c [f, f, f, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         c [p, p]    Nothing)
  , (SymBuiltinFunc "pnoise"         c [p, f, p, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         p [f, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         p [f, f, f, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         p [p, p]    Nothing)
  , (SymBuiltinFunc "pnoise"         p [p, f, p, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         v [f, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         v [f, f, f, f]    Nothing)
  , (SymBuiltinFunc "pnoise"         v [p, p]    Nothing)
  , (SymBuiltinFunc "pnoise"         v [p, f, p, f]    Nothing)
  , (SymBuiltinFunc "cellnoise"      f [f]       Nothing)
  , (SymBuiltinFunc "cellnoise"      f [f, f]    Nothing)
  , (SymBuiltinFunc "cellnoise"      f [p]       Nothing)
  , (SymBuiltinFunc "cellnoise"      f [p, f]    Nothing)
  , (SymBuiltinFunc "cellnoise"      c [f]       Nothing)
  , (SymBuiltinFunc "cellnoise"      c [f, f]    Nothing)
  , (SymBuiltinFunc "cellnoise"      c [p]       Nothing)
  , (SymBuiltinFunc "cellnoise"      c [p, f]    Nothing)
  , (SymBuiltinFunc "cellnoise"      p [f]       Nothing)
  , (SymBuiltinFunc "cellnoise"      p [f, f]    Nothing)
  , (SymBuiltinFunc "cellnoise"      p [p]       Nothing)
  , (SymBuiltinFunc "cellnoise"      p [p, f]    Nothing)
  , (SymBuiltinFunc "cellnoise"      v [f]       Nothing)
  , (SymBuiltinFunc "cellnoise"      v [f, f]    Nothing)
  , (SymBuiltinFunc "cellnoise"      v [p]       Nothing)
  , (SymBuiltinFunc "cellnoise"      v [p, f]    Nothing)

  -- [15.2] Geometric Functions
  --
  , (SymBuiltinFunc "xcomp"          f [v]       Nothing)
  , (SymBuiltinFunc "xcomp"          f [p]       Nothing)
  , (SymBuiltinFunc "xcomp"          f [n]       Nothing)
  , (SymBuiltinFunc "ycomp"          f [v]       Nothing)
  , (SymBuiltinFunc "ycomp"          f [p]       Nothing)
  , (SymBuiltinFunc "ycomp"          f [n]       Nothing)
  , (SymBuiltinFunc "zcomp"          f [v]       Nothing)
  , (SymBuiltinFunc "zcomp"          f [p]       Nothing)
  , (SymBuiltinFunc "zcomp"          f [n]       Nothing)
  -- , (SymFunc "setxcomp"       f [n]    [])
  -- , (SymFunc "setycomp"       f [n]    [])
  -- , (SymFunc "setzcomp"       f [n]    [])
  , (SymBuiltinFunc "length"         f [v]       Nothing)
  , (SymBuiltinFunc "length"         f [p]       Nothing)
  , (SymBuiltinFunc "length"         f [n]       Nothing)
  , (SymBuiltinFunc "normalize"      v [v]       Nothing)
  , (SymBuiltinFunc "normalize"      v [n]       Nothing)
  , (SymBuiltinFunc "distance"       f [p, p]    Nothing)
  -- , (SymFunc "ptlined"    f [p, p] [])
  -- , (SymFunc "rotate"    f [p, p] [])
  , (SymBuiltinFunc "area"           f [p]       Nothing)
  , (SymBuiltinFunc "faceforward"    v [v, v]    Nothing) -- has opt arg
  , (SymBuiltinFunc "faceforward"    v [n, v]    Nothing) -- has opt arg
  , (SymBuiltinFunc "faceforward"    v [n, p]    Nothing) -- has opt arg
  , (SymBuiltinFunc "faceforward"    v [v, p]    Nothing) -- has opt arg
  , (SymBuiltinFunc "reflect"        v [v, v]    Nothing)
  , (SymBuiltinFunc "reflect"        v [v, n]    Nothing)
  , (SymBuiltinFunc "reflect"        v [v, p]    Nothing)
  , (SymBuiltinFunc "refract"        v [v, v, f] Nothing)
  , (SymBuiltinFunc "refract"        v [v, n, f] Nothing)
  , (SymBuiltinFunc "fresnel"        v [v, v, f, f, f] Nothing)  -- TODO
  , (SymBuiltinFunc "transform"      p [s, p]    Nothing) 
  , (SymBuiltinFunc "transform"      p [s, s, p] Nothing) 
  , (SymBuiltinFunc "transform"      p [m, p]    Nothing) 
  , (SymBuiltinFunc "transform"      p [s, m, p] Nothing) 
  , (SymBuiltinFunc "transform"      v [s, v]    Nothing) 
  , (SymBuiltinFunc "transform"      v [s, s, v] Nothing) 
  , (SymBuiltinFunc "transform"      v [m, v]    Nothing) 
  , (SymBuiltinFunc "transform"      v [s, m, v] Nothing) 
  , (SymBuiltinFunc "transform"      n [s, n]    Nothing) 
  , (SymBuiltinFunc "transform"      n [s, s, n] Nothing) 
  , (SymBuiltinFunc "transform"      n [m, n]    Nothing) 
  , (SymBuiltinFunc "transform"      n [s, m, n] Nothing) 
  , (SymBuiltinFunc "vtransform"     v [s, s, v] Nothing) 
  , (SymBuiltinFunc "depth"          f [p]       Nothing) 
  , (SymBuiltinFunc "depth"          f [p]       Nothing) 
  , (SymBuiltinFunc "calculatenormal" p [p]      Nothing)

  -- [15.3] Color Functions
  , (SymBuiltinFunc "comp"           f [c, f]    Nothing) 
  -- , (SymBuiltinFunc "setcomp"        void [c, f, f]    []) 
  , (SymBuiltinFunc "mix"            c [c, c, f] Nothing) 


  -- [15.5] String Functions
  --
  -- FIXME: Need a special treatment for string functions.
  --
  , (SymBuiltinFunc "printf"         void [s]    Nothing) 

  -- [15.6] Shading and Lighting Functions
  , (SymBuiltinFunc "ambient"        c []        Nothing)
  , (SymBuiltinFunc "diffuse"        c [n]       Nothing)
  , (SymBuiltinFunc "diffuse"        c [p]       Nothing)
  , (SymBuiltinFunc "diffuse"        c [v]       Nothing)
  , (SymBuiltinFunc "specular"       c [n, v, f] Nothing)
  , (SymBuiltinFunc "specular"       c [p, p, f] Nothing)
  , (SymBuiltinFunc "specularbrdf"   c [v, n, v, f] Nothing)
  , (SymBuiltinFunc "phong"          c [n, v, f] Nothing)
  , (SymBuiltinFunc "trace"          c [p, p]    Nothing)
  , (SymBuiltinFunc "trace"          c [p, v]    Nothing)

  -- [15.7] Texture Mapping Functions
  , (SymBuiltinFunc "texture"        c [s]       Nothing)
  , (SymBuiltinFunc "environment"    c [s]       Nothing)
  , (SymBuiltinFunc "environment"    c [s, v]    Nothing)

  -- extension for lucille 
  , (SymBuiltinFunc "save_cache" void [i, i, i, c]      Nothing)
  , (SymBuiltinFunc "save_cache" void [i, i, i, f]      Nothing)
  , (SymBuiltinFunc "load_cache" c    [i, i, i]         Nothing)
  , (SymBuiltinFunc "load_cache" f    [i, i, i]         Nothing)
  , (SymBuiltinFunc "turb"       c    [p]               Nothing)
  , (SymBuiltinFunc "occlusion"  f    [p, n]            Nothing)
  , (SymBuiltinFunc "occlusion"  f    [p, n, f]         Nothing)

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

checkArgTy (SymBuiltinFunc name _ fargTys Nothing) argTys = (fargTys == argTys)

-- Check signature of a function with vaargs.
checkArgTy (SymBuiltinFunc name _ fargTys (Just vaTy)) argTys 
  | n >  m = False   -- Insufficient # of args.
  | n == m = (fargTys == argTys)
  | n <  m = let (fTys, vaTys) = splitAt n argTys in
             (fTys == fargTys) && ((replicate (m-n) vaTy) == vaTys)

  where

    n = length fargTys
    m = length argTys

lookupBuiltinFuncWithArgumentSignature :: [Symbol] -> String -> [Type] -> [Symbol]
lookupBuiltinFuncWithArgumentSignature syms name argTys = 
  filter (\sym@(SymBuiltinFunc fname _ tys _) -> (fname == name) && (checkArgTy sym argTys ) ) syms
  -- filter (\(SymBuiltinFunc fname _ tys _) -> (fname == name) && (tys == argTys) ) syms
