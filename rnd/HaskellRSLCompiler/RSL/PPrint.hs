-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.PPrint
---- Copyright   :  (c) Syoyo Fujita
---- License     :  BSD-style
----
---- Maintainer  :  syoyo@lucillerender.org
---- Stability   :  experimental
---- Portability :  GHC 6.8
----
---- RSLParser   :  Pretty printer for RSL AST.
----
-------------------------------------------------------------------------------

module RSL.PPrint where

import RSL.AST

class AST a where
  pprint :: a -> String
  pprintList :: [a] -> String
  pprintList = concat . map pprint


instance AST a => AST [a] where
  pprint = pprintList

instance AST ShaderType where
  pprint ty = case ty of
    Surface       -> "surface"
    Volume        -> "volume"
    Displacement  -> "displacement"
    Imager        -> "imager"


instance AST Type where
  pprint ty = case ty of
    TyVoid        -> "void"
    TyFloat       -> "float"
    TyVector      -> "vector"
    TyColor       -> "color"
    TyPoint       -> "point"
    TyMatrix      -> "matrix"

instance AST FormalDecl where
  pprint decl = case decl of
    FormalDecl ty name Nothing    -> pprint ty ++ name
    FormalDecl ty name (Just val) -> pprint ty ++ name


instance AST Func where
  pprint f = case f of
    ShaderFunc ty name Nothing      -> pprint ty ++ name
    ShaderFunc ty name (Just decls) -> pprint ty ++ name ++ pprint decls


