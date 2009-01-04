-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.CodeGenLLVM
---- Copyright   :  (c) Syoyo Fujita
---- License     :  Modified BSD
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
  TyString  -> "i8*"
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

  genGlobal :: a -> String
  genGlobalList :: [a] -> String
  genGlobalList = concat . map genGlobal

instance AST a => AST [a] where

  gen = genList
  genGlobal = genGlobalList

instance AST ShaderType where

  gen n ty = case ty of

    Surface       -> "surface"
    Volume        -> "volume"
    Displacement  -> "displacement"
    Imager        -> "imager"

  genGlobal ty = gen 0 ty

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
  TypeCast (Just sym) _ _ _       -> getLLNameOfSym sym
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

emitLoadFromBuiltinVariable :: Symbol -> Symbol -> String
emitLoadFromBuiltinVariable dst src = concat
  [ dstReg ++ " = " ++ "call " ++ emitTy (getTyOfSym dst) ++ " "
  , "@rsl_get" ++ (getNameOfSym src) ++ "()"
  , ";\n"
  ]
 
  where

    dstReg = "%" ++ getNameOfSym dst
  

emitInsertElement :: String -> String -> String -> String -> String -> Int -> String
emitInsertElement ret dst src dstTy srcTy idx =
  indent 1 ++ ret ++ " = insertelement " ++ dstTy ++ " " ++ dst ++ ", " ++ srcTy ++ " " ++ src ++ ", i32 " ++ (show idx) ++ ";\n"

--   %src : float, %dst : <4xfloat>
--   %tmp0 = insertelement undef %src 0
--   %tmp1 = insertelement %tmp0 %src 1
--   %tmp2 = insertelement %tmp1 %src 2
--   %dst  = insertelement %tmp2 %src 3
--
emitFtoV :: String -> String -> String
emitFtoV dst src = concat
  [ emitInsertElement tmpReg0 "undef" src dstTy srcTy 0
  , emitInsertElement tmpReg1 tmpReg0 src dstTy srcTy 1
  , emitInsertElement tmpReg2 tmpReg1 src dstTy srcTy 2
  , emitInsertElement dst     tmpReg2 src dstTy srcTy 3
  ]

  where

    tmpReg0 = dst ++ ".tmp0"
    tmpReg1 = dst ++ ".tmp1"
    tmpReg2 = dst ++ ".tmp2"

    dstTy   = emitTy TyVector
    srcTy   = emitTy TyFloat


emitTypeCast :: Type -> Type -> String -> String -> String
emitTypeCast toTy fromTy dst src = case (toTy, fromTy) of
  (TyColor, TyFloat) -> emitFtoV dst src

instance AST Expr where
  
  gen n expr = case expr of

    TypeCast (Just sym) ty space expr      -> concat
      [ gen n expr
      , emitTypeCast ty (getTyOfExpr expr) (getLLNameOfSym sym) (getReg expr)
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

    Const (Just sym) (S str)    -> concat
      [ indent n
      , dst ++ " = " ++ "getelementptr [" ++ (show $ length str + 1) ++ " x i8]* @" ++ (getNameOfSym sym) ++ ".str, i32 0, i32 0"
      , ";\n"
      ]
        
        where

          dst = getLLNameOfSym sym
          

    Var (Just dstSym) srcSym -> case srcSym of

      (SymVar name _ _ KindFormalVariable)  -> concat
        [ indent n ++ emitLoadFromFormalVariable dstSym srcSym ]

      (SymVar name _ _ KindBuiltinVariable) -> concat
        [ indent n ++ emitLoadFromBuiltinVariable dstSym srcSym ]

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
      , getReg e1
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


    -- shadervar(var) = b
    -- -> call @rsl_setVar(b)
    -- 
    -- a = b
    -- -> store b, a
    
    Assign _ op (Var _ sym) rexpr -> case sym of

      (SymVar _ _ _ KindBuiltinVariable) -> concat

        [ gen n rexpr
        , indent n
        , "call void "
        , "@rsl_set" ++ getNameOfSym sym ++ "( "
        , emitTy (getTyOfExpr rexpr) ++ " "
        , getReg rexpr ++ " "
        , ");\n"
        ]


      _ -> concat

        [ gen n rexpr
        , indent n
        , "store "
        , emitTy (getTyOfExpr rexpr) ++ " "
        , getReg rexpr ++ ", "
        , emitTy (getTyOfExpr rexpr) ++ "* "
        , getLLNameOfSym sym
        , ";\n"
        ]


    -- Call builtin function requires special treatment.
    -- 
    -- e.g.,
    --
    -- ambient()
    --
    -- ->
    --
    -- %a.buf = alloca color
    -- call void @ambient_c(%a.buf)
    -- %dst = load color, %a.buf
    --
    Call (Just dst) (SymBuiltinFunc name retTy argTys _) args  -> concat 
      [ gen n args
      , indent n ++ tmpReg ++ " = alloca " ++ (emitTy retTy) ++ ";\n" -- TODO: consider void case
      , indent n ++ "call void @" ++ name ++ "_" ++ getFunctionSuffix retTy argTys ++ "("
      , genArgForRet ++ (if (length args) > 0 then ", " else "") ++ genArgs args
      , ");\n"
      , indent n ++ "%" ++ (getNameOfSym dst) ++ " = "
      , "load " ++ emitTy (getTyOfSym dst) ++ "* " ++ tmpReg
      , ";\n"
      ]

      where

        genArgForRet = if (retTy == TyVoid) then "" else (emitTy retTy) ++ "* " ++ tmpReg
        genArgs []     = ""
        genArgs [x]    = emitTy (getTyOfExpr x) ++ " " ++ getReg x
        genArgs (x:xs) = emitTy (getTyOfExpr x) ++ " " ++ getReg x ++ ", " ++ genArgs xs

        tmpReg         = "%" ++ getNameOfSym dst ++ ".buf"

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

    _                                 -> error $ "[CodeGen] TODO: " ++ show expr


  genGlobal e = emitGlobal e

instance AST FormalDecl where

  gen n decl = case decl of

    FormalDecl ty name Nothing    -> emitTy ty ++ " " ++ "%" ++ name
    FormalDecl ty name (Just val) -> emitTy ty ++ " " ++ "%" ++ name


  genList n []     = ""
  genList n [x]    = gen n x
  genList n (x:xs) = gen n x ++ ", " ++ gen n xs


  genGlobal decl = "" -- TODO
  genGlobalList []     = ""
  genGlobalList [x]    = genGlobal x
  genGlobalList (x:xs) = genGlobal x ++ genGlobal xs
  

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


emitBuiltinVariableSetter :: Symbol -> String
emitBuiltinVariableSetter (SymVar name ty _ _) =
  "declare void @rsl_set" ++ name ++ "(" ++ tyStr ++ ")\n"

  where

    tyStr = emitTy ty

emitBuiltinVariableGetter :: Symbol -> String
emitBuiltinVariableGetter (SymVar name ty _ _) =
  "declare " ++ tyStr ++ " @rsl_get" ++ name ++ "()\n"
  
  where

    tyStr = emitTy ty

getSuffix :: Type -> String
getSuffix ty = case ty of
  TyVector -> "v"
  TyVoid   -> ""      -- no suffix letter
  TyFloat  -> "f"
  TyNormal -> "n"
  TyColor  -> "c"
  TyPoint  -> "p"
  TyString -> "s"
  TyMatrix -> "m"

getFunctionSuffix :: Type -> [Type] -> String
getFunctionSuffix retTy argTys = getSuffix retTy ++ concatMap getSuffix argTys

-- TODO: emit signature for optional argument.
emitBuiltinFunctionDef :: Symbol -> String
emitBuiltinFunctionDef (SymBuiltinFunc name retTy argTys _) = concat
  [ "declare void " ++ "@" ++ name ++ "_" ++ getFunctionSuffix retTy argTys ++ "("
  , retArgSig
  , if (length argTys) > 0 then ", " else ""
  , argSigs argTys
  , ")"
  , ";\n"
  ]

  where

    retArgSig  = if retTy == TyVoid then "" else (emitTy retTy) ++ "*"
    argSigs []     = ""
    argSigs [x]    = emitTy x
    argSigs (x:xs) = emitTy x ++ ", " ++ argSigs xs
  

emitGlobal :: Expr -> String
emitGlobal e = case e of
  Const (Just sym) (S str) -> "@" ++ (getNameOfSym sym) ++ ".str = internal constant [" ++ show (length str + 1) ++ " x i8] c\"" ++ str ++ "\\00\"\n"
  TypeCast _ _ _ expr -> emitGlobal expr
  Var _ sym -> ""
  Assign _ _ lexpr rexpr  -> emitGlobal lexpr ++ emitGlobal rexpr
  Def _ _ (Just expr)     -> emitGlobal expr
  UnaryOp _ _ expr        -> emitGlobal expr
  BinOp _ _ expr0 expr1   -> emitGlobal expr0 ++ emitGlobal expr1
  Call _ _ exprs          -> concatMap emitGlobal exprs
  {- TODO
  | Triple    [Expr]                       -- length(expr) == 3
  | If        Expr                        -- condition
              [Expr]                      -- statement
              (Maybe [Expr])              -- else statement
  | While     Expr                        -- condition
              [Expr]                      -- statement
  | Extract  (Maybe Symbol)
              Char                        -- x, y, z, or w
              Expr                        -- Should be vector expr.
  -}


genHeader = concatMap (emitBuiltinVariableGetter) builtinShaderVariables ++
            concatMap (emitBuiltinVariableSetter) builtinOutputShaderVariables ++
            concatMap (emitBuiltinFunctionDef) builtinShaderFunctions

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

  genGlobal f = case f of
    ShaderFunc ty name decls stms -> concat 
      [ genGlobal stms
      , "\n"
      ]
  
-- codeGenLLVM ast = gen ast
