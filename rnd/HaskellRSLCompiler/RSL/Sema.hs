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
  [ (Symbol "Ci" TyColor  Varying KindVariable)
  , (Symbol "Oi" TyColor  Varying KindVariable)
  , (Symbol "Cs" TyColor  Varying KindVariable)
  , (Symbol "Os" TyColor  Varying KindVariable)
  , (Symbol "P"  TyPoint  Varying KindVariable)
  , (Symbol "I"  TyVector Varying KindVariable)
  , (Symbol "E"  TyPoint  Uniform KindVariable)
  , (Symbol "N"  TyNormal Varying KindVariable)
  , (Symbol "Ng" TyNormal Varying KindVariable)
  , (Symbol "Cl" TyColor  Varying KindVariable)
  , (Symbol "Ol" TyColor  Varying KindVariable)
  ] -- More is TODO
  

{-
builtinShaderFunctions :: [Symbol]
builtinShaderFunctions =
  [ (Symbol "faceforward" TyVector Varying KindVariable)
  ] -- More is TODO
-}




