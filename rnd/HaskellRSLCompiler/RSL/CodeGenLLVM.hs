-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.CodeGenLLVM
---- Copyright   :  (c) Syoyo Fujita
---- License     :  BSD-style
----
---- Maintainer  :  syoyo@lucillerender.org
---- Stability   :  experimental
---- Portability :  GHC 6.8
----
---- RSL.CodeGenLLVM :  Code generator for LLVM IR from RSL AST representation.
----
-------------------------------------------------------------------------------

module RSL.CodeGenLLVM where

import Control.Monad.State

import RSL.AST

indent :: Int -> String
indent n = concat $ replicate (4 * n) " "

data LLVMState = LLVMState  { globals :: [Symbol]
                            , n       :: Int        -- counter for variable
                                                    -- numbering
                            }

initLLVMState = LLVMState   { globals = []
                            , n       = 0
                            }

emitTy ty = case ty of
  TyVoid    -> "void"
  TyString  -> "string"
  TyFloat   -> "float"
  TyVector  -> "<4xfloat>"
  TyColor   -> "<4xfloat>"
  TyPoint   -> "<4xfloat>"
  TyNormal  -> "<4xfloat>"
  TyMatrix  -> "<16xfloat>"

class AST a where

  gen :: Int -> a -> String
  genList :: Int -> [a] -> String
  genList n = concat . map (gen n)


instance AST a => AST [a] where

  gen = genList

instance AST ShaderType where

  gen n ty = case ty of

    Surface       -> "surface"
    Volume        -> "volume"
    Displacement  -> "displacement"
    Imager        -> "imager"


emitOp op = case op of
  OpAdd       -> "add"
  OpSub       -> "sub"
  OpMul       -> "mul"
  OpDiv       -> "fdiv"   -- FIXME
  OpDot       -> "."
  OpNeg       -> "!"
  OpLe        -> "<="
  OpLt        -> "<"
  OpGe        -> ">="
  OpGt        -> ">"
  OpEq        -> "=="
  OpNeq       -> "!="
  OpAssign    -> "="
  OpAddAssign -> "+="
  OpSubAssign -> "-="
  OpMulAssign -> "*="
  OpDivAssign -> "/="

getReg :: Expr -> String
getReg expr = case expr of
  Const (Just sym) _              -> "%" ++ (getNameOfSym sym)
  Var   sym                       -> "%" ++ (getNameOfSym sym)
  UnaryOp (Just sym) _ _          -> "%" ++ (getNameOfSym sym)
  BinOp (Just sym) _ _            -> "%" ++ (getNameOfSym sym)

getTyOfExpr :: Expr -> Type
getTyOfExpr expr = case expr of
  Const (Just sym) _              -> getTyOfSym sym
  Var   sym                       -> getTyOfSym sym
  UnaryOp (Just sym) _ _          -> getTyOfSym sym
  BinOp (Just sym) _ _            -> getTyOfSym sym

instance AST Expr where
  
  gen n expr = case expr of

    TypeCast ty space expr      -> concat
      [ "TypeCast"
      ]

    Const (Just sym) (F val)    -> concat
      [ indent n ++ dst ++ " = "
      , "alloca "
      , ty ++ ";\n"
      --
      , indent n ++ "store "
      , ty ++ " "
      , (show val) ++ " "
      , ", " ++ ty ++ "* "
      , dst ++ ";\n"
      ]

        where
          dst = "%" ++ getNameOfSym sym
          ty = emitTy (getTyOfSym sym)

    Var    (SymVar name _ _ _)  -> "%" ++ name

    UnaryOp _ op expr           -> concat
      [ "Unary"
      ]

    BinOp _ op exprs            -> concat
      [ "Bin"
      ]


    Def    ty name Nothing -> concat 
      [ indent n
      , "%" ++ name ++ " "
      , "= "
      , "alloca "
      , emitTy ty
      , ";\n"
      ]


    Def    ty name (Just initExpr)  -> concat 
      [ indent n
      , "%" ++ name ++ " "
      , "= "
      , "alloca "
      , emitTy ty
      , ";\n"
      , concat [ gen n initExpr ]
      ]


    -- a = b
    -- -> store b, a
    Assign _ op lexpr rexpr -> concat 
      [ gen n rexpr
      , indent n
      , "store "
      , emitTy (getTyOfExpr lexpr) ++ " "
      , getReg rexpr ++ ", "
      , emitTy (getTyOfExpr lexpr) ++ "* "
      , getReg lexpr ++ " "
      , ";\n"
      ]


    Call _ (SymFunc name ty _ _) args  -> concat 
      [ name
      , "call("
      , ")"
      ]

      where

        genArgs []     = ""
        genArgs [x]    = gen 0 x
        genArgs (x:xs) = gen 0 x ++ ", " ++ genArgs xs

    Triple exprs                      -> concat
      [ "( "
      , gen 0 (exprs !! 0)
      , ", "
      , gen 1 (exprs !! 1)
      , ", "
      , gen 2 (exprs !! 2)
      , " )"
      ]

    While cond stms                   -> concat
      [ indent n
      , "while ( "
      , gen 0 cond
      , " ) {\n"
      , gen (n+1) stms
      , "\n" ++ indent n ++ "}"
      ]

    If cond thenStms (Just elseStms)  -> concat
      [ indent n
      , "if ( "
      , gen 0 cond 
      , " ) {\n"
      , gen (n+1) thenStms
      , indent n ++ "} else {\n"
      , gen (n+1) elseStms
      , indent n ++ "}"
      ]

    Nil                               -> "null"

    _                                 -> error $ "TODO: " ++ show expr

instance AST FormalDecl where

  gen n decl = case decl of

    FormalDecl ty name Nothing    -> emitTy ty ++ " " ++ "%" ++ name
    FormalDecl ty name (Just val) -> emitTy ty ++ " " ++ "%" ++ name


  genList n []     = ""
  genList n [x]    = gen n x
  genList n (x:xs) = gen n x ++ ", " ++ gen n xs

instance AST Func where

  gen n f = case f of

    ShaderFunc ty name decls stms -> concat 
      [ "define void "
      , "@" ++ name
      , "("
      , gen n decls
      , ") {\n"
      , gen (n+1) stms
      , "\n" ++ indent (n+1) ++ "ret void;\n"
      , "\n}\n"
      ]

  
-- codeGenLLVM ast = gen ast
