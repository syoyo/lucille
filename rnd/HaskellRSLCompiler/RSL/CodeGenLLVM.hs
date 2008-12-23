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
import Numeric
import Foreign
import Foreign.C.Types
import Data.IORef
import System.IO.Unsafe           -- A magical module ;-)

import RSL.AST
import RSL.Sema

--
-- Routine to generate unique identifier.
--

theCounterGen :: IORef Int
theCounterGen = unsafePerformIO $ do newIORef 0

setCounterGen :: Int -> IO ()
setCounterGen n = writeIORef theCounterGen n

getCounterGen :: IO Int
getCounterGen = readIORef theCounterGen

getCounter :: IO Int
getCounter = do
    n <- getCounterGen
    let n' = n + 1
    setCounterGen n'
    return n'

genUniqueReg :: String
genUniqueReg = "%reg" ++ (show $ unsafePerformIO getCounter)


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

-- LLVM IR requires 64bit hex value for 32bit floating point value to exactly
-- express floating point value in LLVM IR.
-- i.e., hex((double)(float)val)

doubleAsULLong :: Ptr CDouble -> IO CULLong
doubleAsULLong p = do
  ull <- peek (castPtr p)
  return ull

floatAsUInt :: Ptr CFloat -> IO CUInt
floatAsUInt p = do
  ui <- peek (castPtr p)
  return ui

bitcastFloatToUInt :: CFloat -> CUInt
bitcastFloatToUInt f = unsafePerformIO $ do
  r <- with f floatAsUInt
  return r

bitcastDoubleToULLong :: CDouble -> CULLong
bitcastDoubleToULLong d = unsafePerformIO $ do
  r <- with d doubleAsULLong
  return r

showHexReplOfFloat  f = "0x" ++ showHex (bitcastFloatToUInt    f) ""
showHexReplOfDouble d = "0x" ++ showHex (bitcastDoubleToULLong d) ""

doubleToCFloat :: Double -> CFloat
doubleToCFloat d = realToFrac d

cfloatToCDouble :: CFloat -> CDouble
cfloatToCDouble f = realToFrac f

emitLLVMFloatString :: Double -> String
emitLLVMFloatString f = showHexReplOfDouble $ cfloatToCDouble $ doubleToCFloat f


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

getLLNameOfSym :: Symbol -> String
getLLNameOfSym (SymVar name ty _ KindBuiltinVariable) = "@" ++ name
getLLNameOfSym (SymVar name _ _ _)                    = "%" ++ name

getReg :: Expr -> String
getReg expr = case expr of
  Const (Just sym) _              -> getLLNameOfSym sym
  Var (Just sym) _                -> getLLNameOfSym sym
  UnaryOp (Just sym) _ _          -> getLLNameOfSym sym
  BinOp (Just sym) _ _ _          -> getLLNameOfSym sym
  Call (Just sym) _ _             -> getLLNameOfSym sym
  _                               -> error $ "getReg, TODO:" ++ show expr


emitLoadFromFormalVariable :: Symbol -> Symbol -> String
emitLoadFromFormalVariable dst src = concat
  [ dstReg ++ " = " ++ "load " ++ emitTy (getTyOfSym dst) ++ "* "
  , srcReg ++ ".addr"
  , ";\n"
  ]
 
  where

    dstReg = "%" ++ getNameOfSym dst
    srcReg = "%" ++ getNameOfSym src

emitLoadFromVariable :: Symbol -> Symbol -> String
emitLoadFromVariable dst src = concat
  [ dstReg ++ " = " ++ "load " ++ emitTy (getTyOfSym dst) ++ "* " ++ srcReg
  , ";\n"
  ]
 
  where

    dstReg = "%" ++ getNameOfSym dst
    srcReg = "%" ++ getNameOfSym src
  
instance AST Expr where
  
  gen n expr = case expr of

    TypeCast ty space expr      -> concat
      [ "TypeCast"
      ]

    Const (Just sym) (F val)    -> concat

      -- %tmp.buf = alloca ty
      -- store ty val, ty* %tmp.buf
      -- %dst = load ty* %tmp.buf
      
      [ indent n ++ tmpReg ++ " = "
      , "alloca "
      , ty ++ ";\n"
      --
      , indent n ++ "store "
      , ty ++ " "
      , emitLLVMFloatString val ++ " "
      , ", " ++ ty ++ "* "
      , tmpReg ++ ";\n"
      --
      , indent n ++ dst ++ " = " ++ "load " ++ ty ++ "* " ++ tmpReg
      , ";\n"
      ]

        where
          dst     = "%" ++ getNameOfSym sym
          ty      = emitTy (getTyOfSym sym)
          tmpReg  = genUniqueReg ++ ".addr"


    Var (Just dstSym) srcSym -> case srcSym of

      (SymVar name _ _ KindFormalVariable)  -> concat
        [ indent n ++ emitLoadFromFormalVariable dstSym srcSym ]

      (SymVar name _ _ KindBuiltinVariable) -> ""

      _                                     -> concat
        [ indent n ++ emitLoadFromVariable dstSym srcSym ]


    UnaryOp (Just sym) op expr           -> concat
      [ gen n expr
      , indent n ++ "%" ++ getNameOfSym sym ++ " = "
      , emitOp op ++ " "
      , emitTy (getTyOfSym sym) ++ " zeroinitializer , "
      , emitTy (getTyOfSym sym) ++ " " ++ getReg expr
      , ";\n"
      ]

    BinOp (Just sym) op e0 e1            -> concat
      [ gen n e0
      , gen n e1
      , indent n ++ "%" ++ getNameOfSym sym ++ " = "
      , emitOp op ++ " "
      , emitTy (getTyOfSym sym) ++ " " ++ getReg e0 ++ " , "
      , emitTy (getTyOfSym sym) ++ " " ++ getReg e1
      , ";\n"
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
      , gen n initExpr
      --
      , indent n ++ "store "
      , emitTy (getTyOfExpr initExpr) ++ " " ++ (getReg initExpr) ++ " , "
      , emitTy (getTyOfExpr initExpr) ++ "* " ++ "%" ++ name
      , ";\n"
      ]


    -- a = b
    -- -> store b, a
    Assign _ op (Var _ sym) rexpr -> concat 
      [ gen n rexpr
      , indent n
      , "store "
      , emitTy (getTyOfExpr rexpr) ++ " "
      , getReg rexpr ++ ", "
      , emitTy (getTyOfExpr rexpr) ++ "* "
      , getLLNameOfSym sym
      , ";\n"
      ]


    Call (Just dst) (SymFunc name ty _ _) args  -> concat 
      [ gen n args
      , indent n ++ "%" ++ (getNameOfSym dst) ++ " = "
      , "call " ++ "@" ++ name ++ "()"
      , ";\n"
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


emitStoreFormalVariableToBuffer :: Int -> FormalDecl -> String
emitStoreFormalVariableToBuffer n (FormalDecl ty name _) = concat
  -- %var.addr = alloca ty
  -- store %var, %var.addr
  [ indent n ++ buf ++ " = " ++ "alloca " ++ tyStr ++ ";\n"
  , indent n ++ "store " ++ tyStr ++ " " ++ src ++ " , " ++ tyStr ++ "* " ++ buf
  , ";\n"
  ]

  where

    tyStr  = emitTy ty
    buf = "%" ++ name ++ ".addr"
    src = "%" ++ name


emitBuiltinVariableDef :: Symbol -> String
emitBuiltinVariableDef (SymVar name ty _ _) =
  "@" ++ name ++ " = " ++ "external global " ++ emitTy ty ++ ";\n"

genHeader = 
  concatMap (emitBuiltinVariableDef) builtinShaderVariables

instance AST Func where

  gen n f = case f of

    ShaderFunc ty name decls stms -> concat 
      [ "define void "
      , "@" ++ name
      , "("
      , gen n decls
      , ") {\n"
      , concatMap (emitStoreFormalVariableToBuffer (n+1)) decls
      , gen (n+1) stms
      , "\n" ++ indent (n+1) ++ "ret void;\n"
      , "\n}\n"
      ]

  
-- codeGenLLVM ast = gen ast
