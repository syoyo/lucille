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

data Op
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
    deriving (Show, Eq)
  
data Type
  = TyUnknown
  | TyVoid
  | TyString
  | TyFloat
  | TyVector
  | TyColor
  | TyPoint
  | TyNormal
  | TyMatrix
    deriving (Show, Eq)

data StorageClass
  = Uniform
  | Varying
  | Vertex            -- Not in RI Spec3.2
  | FaceVarying       -- Not in RI Spec3.2
  | FaceVertex        -- Not in RI Spec3.2
    deriving (Show, Eq)

data ShaderType 
  = Surface
  | Light
  | Volume
  | Displacement
  | Imager
    deriving (Show, Eq)

data Const 
  = I Int             -- optinal?
  | F Double
  | S String
  | V [Double]        -- vector
  | M [Double]        -- matrix
    deriving (Show, Eq)

data Kind
  = KindVariable
  | KindFunction
    deriving (Show, Eq)

data Symbol
  = Symbol  String            -- name of the symbol
            Type              -- type of the symbol
            StorageClass      -- storage class of the symbol
            Kind              -- kind of the symbol

    deriving (Show, Eq)

type SymbolTable
  = [(String, [Symbol])]


--
-- We define typed AST.
--
data Expr 
  = Const   Const
  | Var     Symbol
  | Assign  Type Expr Expr
  | Def     Type String (Maybe Expr)
  | BinOp   Type    -- Type of this operator
            Op      -- Operator
            [Expr]  -- Left & Right
  | Call    Type    -- Return type
            String  -- Name of procedure
            [Expr]  -- Arguments
    deriving (Show, Eq)
  

data Func 
  = ShaderFunc ShaderType String [FormalDecl] [Expr]
  | UserFunc Type String
    deriving (Show, Eq)


data FormalDecl 
  = FormalDecl Type String (Maybe Const)
    deriving (Show, Eq)
