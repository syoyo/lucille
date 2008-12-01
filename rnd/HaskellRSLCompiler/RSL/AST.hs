-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.AST
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
module RSL.AST where

data Type =
    TyVoid
  | TyString
  | TyFloat
  | TyVector
  | TyColor
  | TyPoint
  | TyMatrix
    deriving (Show, Eq)

data ShaderType =
    Surface
  | Light
  | Volume
  | Displacement
  | Imager
    deriving (Show, Eq)

data Const =
    I Int             -- optinal?
  | F Double
  | S String
  | V [Double]        -- vector
  | M [Double]        -- matrix
    deriving (Show, Eq)

--
-- We define typed AST.
--
data Expr =
    Const   Const
  | Var     Type String
  | Assign  Type Expr Expr
    deriving (Show, Eq)
  

data Func = 
    ShaderFunc ShaderType String (Maybe [FormalDecl])
  | UserFunc Type String
    deriving (Show, Eq)


data FormalDecl =
    FormalDecl Type String (Maybe Const)
    deriving (Show, Eq)
