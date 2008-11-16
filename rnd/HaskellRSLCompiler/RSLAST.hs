-------------------------------------------------------------------------------
---- |
---- Module      :  RSLAST
---- Copyright   :  (c) Syoyo Fujita
---- License     :  BSD-style
----
---- Maintainer  :  syoyo@lucillerender.org
---- Stability   :  experimental
---- Portability :  GHC 6.8
----
---- RSLParser   :  Data structure for RSL Abstract Syntax Tree.
----
-------------------------------------------------------------------------------
module RSLAST where

data RSLType =
    TyVoid
  | TyVector
  | TyColor
  | TyString
    deriving (Show, Eq)


data RSLConst =
    I Int
  | F Double
  | S String
  | V [Double]       -- vector
  | M [Double]       -- matrix
    deriving (Show, Eq)

--
-- We define typed AST.
--
data Expr =
    Const   RSLConst
  | Var     RSLType String
  | Assign  RSLType Expr Expr
    deriving (Show, Eq)
  
