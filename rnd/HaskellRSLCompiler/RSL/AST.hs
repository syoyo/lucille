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

data Op =
    OpAdd
  | OpSub
  | OpMul
  | OpDiv
    deriving (Show, Eq)
  
data Type =
    TyUndef
  | TyVoid
  | TyString
  | TyFloat
  | TyVector
  | TyColor
  | TyPoint
  | TyNormal
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
  | Def     Type String (Maybe Expr)
  | BinOp   Type    -- Type of this operator
            Op      -- Operator
            [Expr]  -- Left & Right
  | Call    Type    -- Return type
            String  -- Name of procedure
            [Expr]  -- Arguments
    deriving (Show, Eq)
  

data Func = 
    ShaderFunc ShaderType String [FormalDecl] [Expr]
  | UserFunc Type String
    deriving (Show, Eq)


data FormalDecl =
    FormalDecl Type String (Maybe Const)
    deriving (Show, Eq)
