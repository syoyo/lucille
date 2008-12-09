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
  -- 15.2 Geometric Functions
    (SymFunc "length"      f [v]    [])
  , (SymFunc "normalize"   v [v]    [])
  , (SymFunc "distance"    f [p, p] [])
  , (SymFunc "faceforward" v [v, v] [])

  -- 15.6 Shading and Lighting Functions
  , (SymFunc "ambient"     c []     [])
  , (SymFunc "diffuse"     c []     [])

  ] -- More is TODO

  where

    f = TyFloat
    c = TyColor
    v = TyVector
    p = TyPoint


