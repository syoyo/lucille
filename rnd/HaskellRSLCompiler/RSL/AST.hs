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
  | OpDot       -- .
  | OpNeg       -- !
  | OpGe        -- >=
  | OpGt        -- >
  | OpLe        -- <=
  | OpLt        -- <
  | OpEq        -- ==
  | OpNeq       -- !=
  | OpAssign    -- =
  | OpAddAssign -- +=
  | OpSubAssign -- -=
  | OpMulAssign -- *=
  | OpDivAssign -- /=
    deriving (Show, Eq)

-- | Type qualifier
data Qual
  = Output
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
  | TyQualified Qual Type
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
  = SymVar  String            -- name of the symbol
            Type              -- type of the symbol
            StorageClass      -- storage class of the symbol
            Kind              -- kind of the symbol

  | SymFunc String            -- name of the function
            Type              -- return type of the function
            [Type]            -- arguments of the function         
            [Type]            -- optional arguments

    deriving (Show, Eq)

getName (SymVar  name _ _ _) = name
getName (SymFunc name _ _ _) = name
getTy   (SymVar  _ ty _ _)   = ty
getTy   (SymFunc _ ty _ _)   = ty

type SymbolTable
  = [(String, [Symbol])]

type Statement = [Expr]

--
-- We define typed AST.
--
data Expr 
  = Const     Const
  | TypeCast  Type                        -- toType
              String                      -- spaceType if any
              Expr                        -- fromExpr
  | Var       Symbol
  | Assign    Op                          -- operator
              Expr                        -- lhs
              Expr                        -- rhs
  | Def       Type String (Maybe Expr)
  | UnaryOp   Op                          -- Operator
              Expr                        
  | BinOp     Op                          -- Operator
              [Expr]                      -- Left & Right
  | Call      Symbol                      -- Function signature
              [Expr]                      -- Arguments

  | Triple    [Expr]                       -- length(expr) == 3

  -- Stmt
  | If        Expr                        -- condition
              [Expr]                      -- statement
              (Maybe [Expr])              -- else statement
  | While     Expr                        -- condition
              [Expr]                      -- statement
  | Nil                                   -- null expr
    deriving (Show, Eq)
  

data Func 
  = ShaderFunc ShaderType String [FormalDecl] [Expr]
  | UserFunc   Type       String
    deriving (Show, Eq)


data FormalDecl 
  = FormalDecl Type String (Maybe Expr)     -- TODO: Allow const expression
    deriving (Show, Eq)
