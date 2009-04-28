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

{-
 - gen        : generate shader code.
 - genStatic  : generate precomputation code. 
 - genDynamic : generate dynamic, specialized code. 
 -}

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

genUniqueID  :: Int
genUniqueID  = unsafePerformIO getCounter

indent :: Int -> String
indent n = concat $ replicate (4 * n) " "

data LLVMState = LLVMState  { globals :: [Symbol]
                            , n       :: Int        -- counter for variable
                                                    -- numbering
                            }

initLLVMState = LLVMState   { globals = []
                            , n       = 0
                            }

type Register = String

emitTy ty = case ty of
  TyVoid        -> "void"
  TyInt         -> "i32"
  TyString      -> "i8*"
  TyFloat       -> "float"
  TyVector      -> "<4 x float>"
  TyColor       -> "<4 x float>"
  TyPoint       -> "<4 x float>"
  TyNormal      -> "<4 x float>"
  TyMatrix      -> "<16 x float>"
  TyBool        -> "i1"
  TyArray n ty  -> emitTy ty ++ "*"

getLLVMTySize ty = case ty of
  TyVoid        -> 4        -- FIXME: Consider 64bit env
  TyInt         -> 4
  TyString      -> 4        -- FIXME: Consider 64bit env
  TyFloat       -> 4
  TyVector      -> 16
  TyColor       -> 16 
  TyPoint       -> 16
  TyNormal      -> 16
  TyMatrix      -> 64
  TyBool        -> 4
  TyArray _ _   -> 4        -- FIXME: Consider 64bit env

class AST a where

  gen :: Int -> a -> String
  genList :: Int -> [a] -> String
  genList n = concat . map (gen n)

  genStatic :: Int -> a -> String
  genStaticList :: Int -> [a] -> String
  genStaticList n = concat . map (genStatic n)

  genDynamic :: Int -> a -> String
  genDynamicList :: Int -> [a] -> String
  genDynamicList n = concat . map (genDynamic n)

  genGlobal :: a -> String
  genGlobalList :: [a] -> String
  genGlobalList = concat . map genGlobal

instance AST a => AST [a] where

  gen        = genList
  genStatic  = genStaticList
  genDynamic = genDynamicList
  genGlobal  = genGlobalList

instance AST ShaderType where

  gen n ty = case ty of

    Surface       -> "surface"
    Volume        -> "volume"
    Displacement  -> "displacement"
    Imager        -> "imager"

  genGlobal    ty = gen 0 ty
  genStatic  n ty = gen 0 ty
  genDynamic n ty = gen 0 ty

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
  OpDot       -> "."      -- FIXME
  OpNeg       -> "!"      -- FIXME
  OpLe        -> "fcmp ole"
  OpLt        -> "fcmp olt"
  OpGe        -> "fcmp oge"
  OpGt        -> "fcmp ogt"
  OpEq        -> "fcmp oeq"
  OpNeq       -> "fcmp une"
  OpOr        -> "or"
  OpAnd       -> "and"
  OpAssign    -> "="
  OpAddAssign -> "+="
  OpSubAssign -> "-="
  OpMulAssign -> "*="
  OpDivAssign -> "/="

getLLNameOfSym :: Symbol -> String
getLLNameOfSym (SymVar name ty _ _ KindBuiltinVariable) = "@" ++ name
getLLNameOfSym (SymVar name _ _ _ _)                    = "%" ++ name

getReg :: Expr -> String
getReg expr = case expr of
  Const (Just sym) _              -> getLLNameOfSym sym
  TypeCast (Just sym) _ _ _       -> getLLNameOfSym sym
  Var (Just sym) _                -> getLLNameOfSym sym
  UnaryOp (Just sym) _ _          -> getLLNameOfSym sym
  BinOp (Just sym) _ _ _          -> getLLNameOfSym sym
  Call (Just sym) _ _             -> getLLNameOfSym sym
  Triple (Just sym) _             -> getLLNameOfSym sym
  Conditional (Just sym) _ _ _    -> getLLNameOfSym sym
  _                               -> error $ "getReg, TODO:" ++ show expr


emitDotOp :: Int -> Symbol -> Expr -> Expr -> String
emitDotOp n dst e0 e1 = concat $ map (indent n ++)
  [ xReg0 ++ " = extractelement " ++ ty ++ " " ++ srcReg0 ++ ", i32 0;\n"
  , xReg1 ++ " = extractelement " ++ ty ++ " " ++ srcReg1 ++ ", i32 0;\n"
  , yReg0 ++ " = extractelement " ++ ty ++ " " ++ srcReg0 ++ ", i32 1;\n"
  , yReg1 ++ " = extractelement " ++ ty ++ " " ++ srcReg1 ++ ", i32 1;\n"
  , zReg0 ++ " = extractelement " ++ ty ++ " " ++ srcReg0 ++ ", i32 2;\n"
  , zReg1 ++ " = extractelement " ++ ty ++ " " ++ srcReg1 ++ ", i32 2;\n"
  , tmpMulReg0 ++ " = mul " ++ fty ++ " " ++ xReg0 ++ ", " ++ xReg1 ++ ";\n"
  , tmpMulReg1 ++ " = mul " ++ fty ++ " " ++ yReg0 ++ ", " ++ yReg1 ++ ";\n"
  , tmpMulReg2 ++ " = mul " ++ fty ++ " " ++ zReg0 ++ ", " ++ zReg1 ++ ";\n"
  , tmpAddReg0 ++ " = add " ++ fty ++ " " ++ tmpMulReg0 ++ ", " ++ tmpMulReg1 ++ ";\n"
  , dstReg     ++ " = add " ++ fty ++ " " ++ tmpMulReg2 ++ ", " ++ tmpAddReg0 ++ ";\n"
  ]

  where

    fty         = emitTy TyFloat
    ty          = emitTy (getTyOfExpr e0)
    srcReg0     = getReg e0
    srcReg1     = getReg e1
    dstReg      = getLLNameOfSym dst
    xReg0       = getLLNameOfSym dst ++ ".dotx0"
    xReg1       = getLLNameOfSym dst ++ ".dotx1"
    yReg0       = getLLNameOfSym dst ++ ".doty0"
    yReg1       = getLLNameOfSym dst ++ ".doty1"
    zReg0       = getLLNameOfSym dst ++ ".dotz0"
    zReg1       = getLLNameOfSym dst ++ ".dotz1"
    tmpMulReg0  = getLLNameOfSym dst ++ ".tmpmul0"
    tmpMulReg1  = getLLNameOfSym dst ++ ".tmpmul1"
    tmpMulReg2  = getLLNameOfSym dst ++ ".tmpmul2"
    tmpAddReg0  = getLLNameOfSym dst ++ ".tmpadd0"

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
  ret ++ " = insertelement " ++ dstTy ++ " " ++ dst ++ ", " ++ srcTy ++ " " ++ src ++ ", i32 " ++ (show idx) ++ ";\n"

--   %src : float, %dst : <4xfloat>
--   %tmp0 = insertelement undef %src 0
--   %tmp1 = insertelement %tmp0 %src 1
--   %tmp2 = insertelement %tmp1 %src 2
--   %dst  = insertelement %tmp2 %src 3
--
emitFtoV :: String -> String -> String
emitFtoV dst src = concat $ map (indent 1 ++)
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

--   %src[0:2] : float, %dst : <4xfloat>
--   %tmp0 = insertelement undef %src0 0
--   %tmp1 = insertelement %tmp0 %src1 1
--   %tmp2 = insertelement %tmp1 %src2 2
--   %dst  = insertelement %tmp2 %src2 3
--
emitTriple :: Int -> String -> String -> String -> String -> String
emitTriple n dst src0 src1 src2 = concat $ map (indent n ++)
  [ emitInsertElement tmpReg0 "undef" src0 dstTy srcTy 0
  , emitInsertElement tmpReg1 tmpReg0 src1 dstTy srcTy 1
  , emitInsertElement tmpReg2 tmpReg1 src2 dstTy srcTy 2
  , emitInsertElement dst     tmpReg2 src2 dstTy srcTy 3 -- FIXME
  ]

  where

    tmpReg0 = dst ++ ".tmp0"
    tmpReg1 = dst ++ ".tmp1"
    tmpReg2 = dst ++ ".tmp2"

    dstTy   = emitTy TyVector
    srcTy   = emitTy TyFloat


--   %tmp = alloca ty
--   store %src, %tmp
--   %dst = load %tmp
emitRegCopy :: Register -> Register -> Type -> String
emitRegCopy dst src ty = concat $ map (indent 1 ++)
  [ tmpReg ++ " = alloca " ++ emitTy ty  ++ ";\n"
  , "store " ++ emitTy ty ++ " " ++ src ++ ", " ++ emitTy ty ++ "* " ++ tmpReg ++ ";\n"    
  , dst ++ " = load " ++ emitTy ty ++ "* " ++ tmpReg ++ ";\n"    
  ] 

  where

    tmpReg = dst ++ ".tmp"

--
-- For vector-vector typecast, emit following insruction since LLVM IR does not
-- support reg-reg copy.
-- 
--   %tmp = alloca ty
--   store %src, %tmp
--   %dst = load %tmp
--
emitTypeCast :: Type -> Type -> String -> String -> String
emitTypeCast toTy fromTy dst src = case (toTy, fromTy) of
  (_       , TyFloat)  ->
    if isVectorTy toTy
      then emitFtoV dst src
      else error $ "[CodeGen] emitTypeCast: Invalid typecast?: " ++ "to: " ++ show toTy ++ ", from: " ++ show fromTy
  _                    ->
    if (isVectorTy toTy) && (isVectorTy fromTy)
      then emitRegCopy dst src toTy
      else error $ "[CodeGen] emitTypeCast: TODO: " ++ "to: " ++ show toTy ++ ", from: " ++ show fromTy

emitCacheSaver :: Int -> Int -> Symbol -> String
emitCacheSaver n layer sym = concat
  [ indent n ++ tempX ++ " = call i32 @rsl_getsx()\n"
  , indent n ++ tempY ++ " = call i32 @rsl_getsy()\n"
  , indent n ++ "call void @save_cache_iii" ++ (getSuffix (getTyOfSym sym)) ++ "("
  , "i32 " ++ (show layer) ++ ", "
  , "i32 " ++ tempX ++ ", " 
  , "i32 " ++ tempY ++ ", " 
  , emitTy (getTyOfSym sym) ++ " " ++ getLLNameOfSym sym
  , ")"
  , "\n"
  ]

  where

    tempX = "%" ++ (getNameOfSym sym) ++ ".x"
    tempY = "%" ++ (getNameOfSym sym) ++ ".y"

emitCacheLoader :: Int -> Int -> Symbol -> String
emitCacheLoader n layer sym = concat
  [ indent n ++ tmpX ++ " = call i32 @rsl_getsx();\n"
  , indent n ++ tmpY ++ " = call i32 @rsl_getsy();\n"
  , indent n ++ tmpReg ++ " = alloca " ++ ty ++ ";\n"
  , indent n ++ "call void @load_cache_" ++ (getSuffix (getTyOfSym sym)) ++ "iii("
  , ty ++ "* " ++ tmpReg ++ ", "
  , "i32 " ++ (show layer) ++ ", "
  , "i32 " ++ tmpX ++ ", " 
  , "i32 " ++ tmpY
  , ");\n"
  --
  , indent n ++ getLLNameOfSym sym ++ " = load " ++ ty ++ "* " ++ tmpReg
  , "\n"
  ]

  where

    ty      = emitTy (getTyOfSym sym)
    tmpX    = "%" ++ (getNameOfSym sym) ++ ".x"
    tmpY    = "%" ++ (getNameOfSym sym) ++ ".y"
    tmpReg  = "%" ++ (getNameOfSym sym) ++ ".tmp"


-- for (init; cond; inc) { stmt } =>
--
-- init:
--   br cond 
-- 
-- cond:
--   br body or after
--
-- body:
--   br inc
--
-- inc:
--   br cond or after
--
emitFor :: Int -> Expr -> Expr -> Expr -> [Expr] -> String
emitFor n init cond inc body = concat

  [ gen n init
  , indent n ++ "br label %" ++ condLabel ++ "\n\n"
  -- cond
  , indent (n-1) ++ condLabel ++ ":\n"
  , gen n cond
  , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ afterLabel ++ ", label %" ++ bodyLabel ++ "\n"
  , "\n"
  -- body
  , indent (n-1) ++ bodyLabel ++ ":\n"
  , gen n body
  , indent n ++ "br label %" ++ incLabel ++ "\n"
  , "\n"
  -- inc
  , indent (n-1) ++ incLabel ++ ":\n"
  , gen n inc
  , indent n ++ "br label %" ++ condLabel ++ "\n"
  , "\n"
  -- after
  , indent (n-1) ++ afterLabel ++ ":\n"
  , "\n"
  ]

  where

    condLabel   = "for"  ++ show num ++ ".cond"
    bodyLabel   = "for"  ++ show num ++ ".body"
    incLabel    = "for"  ++ show num ++ ".inc"
    afterLabel  = "for"  ++ show num ++ ".after"

    num       = unsafePerformIO getCounter


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

          dst     = getLLNameOfSym sym
          ty      = emitTy (getTyOfSym sym)
          tmpReg  = getLLNameOfSym sym ++ ".tmp" -- genUniqueReg ++ ".addr"

    Const (Just sym) (S str)    -> concat
      [ indent n
      , dst ++ " = " ++ "getelementptr [" ++ (show $ length str + 1) ++ " x i8]* @" ++ (getNameOfSym sym) ++ ".str, i32 0, i32 0"
      , ";\n"
      ]
        
        where

          dst = getLLNameOfSym sym
          

    Var (Just dstSym) srcSym -> case srcSym of

      (SymVar name _ _ _ KindFormalVariable)  -> concat
        [ indent n ++ emitLoadFromFormalVariable dstSym srcSym ]

      (SymVar name _ _ _ KindBuiltinVariable) -> concat
        [ indent n ++ emitLoadFromBuiltinVariable dstSym srcSym ]

      _                                     -> concat
        [ indent n ++ emitLoadFromVariable dstSym srcSym ]


    UnaryOp (Just sym) op expr           -> concat
      [ gen n expr
      , indent n ++ "%" ++ getNameOfSym sym ++ " = "
      , emitOp op ++ " "
      , emitTy (getTyOfSym sym) ++ " zeroinitializer , "
      , getReg expr
      , ";\n"
      ]

    BinOp (Just sym) OpDot e0 e1         -> concat
      [ gen n e0
      , gen n e1
      , emitDotOp n sym e0 e1
      ]

    BinOp (Just sym) op e0 e1            -> concat
      [ gen n e0
      , gen n e1
      , indent n ++ "%" ++ getNameOfSym sym ++ " = "
      , emitOp op ++ " "
      , emitTy (getTyOfExpr e0) ++ " " ++ getReg e0 ++ " , "
      , getReg e1
      , ";\n"
      ]


    Def    sym Nothing -> concat 
      [ indent n
      , "%" ++ (getNameOfSym sym) ++ " "
      , "= "
      , "alloca "
      , emitTy (getTyOfSym sym)
      , ";\n"
      ]


    Def    sym (Just initExpr)  -> concat 
      [ indent n
      , "%" ++ (getNameOfSym sym) ++ " "
      , "= "
      , "alloca "
      , emitTy (getTyOfSym sym)
      , ";\n"
      , gen n initExpr
      --
      , indent n ++ "store "
      , emitTy (getTyOfExpr initExpr) ++ " " ++ (getReg initExpr) ++ " , "
      , emitTy (getTyOfExpr initExpr) ++ "* " ++ "%" ++ (getNameOfSym sym)
      , ";\n"
      ]


    -- shadervar(var) = b
    -- -> call @rsl_setVar(b)
    -- 
    -- a = b
    -- -> store b, a
    --
    -- a (op)= b
    --
    -- -> tmp0 = load a
    --    tmp1 = op tmp0 b
    --    store tmp1 a
    
    Assign _ op (Var _ sym) rexpr -> case sym of

      (SymVar _ _ _ _ KindBuiltinVariable) -> concat

        [ gen n rexpr
        , indent n
        , "call void "
        , "@rsl_set" ++ getNameOfSym sym ++ "( "
        , emitTy (getTyOfExpr rexpr) ++ " "
        , getReg rexpr ++ " "
        , ");\n"
        ]


      (SymVar _ _ _ _ kind) -> concat

        [ gen n rexpr
        , indent n
        , "store "
        , emitTy (getTyOfExpr rexpr) ++ " "
        , getReg rexpr ++ ", "
        , emitTy (getTyOfExpr rexpr) ++ "* "
        , dstName
        , ";\n"
        ]

        where

          dstName = case kind of
            KindFormalVariable -> getLLNameOfSym sym ++ ".addr"
            _                  -> getLLNameOfSym sym



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

    Call (Just dst) (SymFunc name ty _ _ _) args  -> concat 
      [ gen n args
      , indent n ++ "%" ++ (getNameOfSym dst) ++ " = "
      , "call " ++ "@" ++ name ++ "()"
      , ";\n"
      ]

      where

        genArgs []     = ""
        genArgs [x]    = gen 0 x
        genArgs (x:xs) = gen 0 x ++ ", " ++ genArgs xs

    Triple (Just sym) exprs            -> concat
      [ gen n exprs
      , emitTriple n (getLLNameOfSym sym) (getReg (exprs !! 0)) (getReg (exprs !! 1)) (getReg (exprs !! 2))
      ]

    Return (Just sym) expr                        -> concat
      [ gen n expr
      , indent n ++ "ret " ++ emitTy (getTyOfSym sym) ++ " " ++ (getReg expr)
      , "\n"
      ]

    -- FIXME
    Conditional (Just sym) cond thenExpr elseExpr -> concat
      [ 
      -- cond
        gen n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ thenLabel ++ ", label %" ++ elseLabel ++ "\n"
      , "\n"
      -- then
      , indent (n-1) ++ thenLabel ++ ":\n"
      , gen n thenExpr
      , indent n ++ "br label %" ++ contLabel ++ "\n"
      , "\n"
      -- else
      , indent (n-1) ++ elseLabel ++ ":\n"
      , gen n elseExpr
      , indent n ++ "br label %" ++ contLabel ++ "\n"
      , "\n"
      -- cont
      , indent (n-1) ++ contLabel ++ ":\n"
      , indent n ++ getLLNameOfSym sym ++ " = phi " ++ emitTy (getTyOfSym sym)
      , " [ " ++ getReg thenExpr ++ ", %" ++ thenLabel ++ " ],"
      , " [ " ++ getReg elseExpr ++ ", %" ++ elseLabel ++ " ] ;\n"
      , "\n"
      ]

      where
 
        thenLabel = "if" ++ show num ++ ".then"
        elseLabel = "if" ++ show num ++ ".else"
        contLabel = "if" ++ show num ++ ".cont"

        num       = unsafePerformIO getCounter

    --
    -- while (cond) stmt
    --
    -- ->
    --
    -- br while.cond
    --
    -- while.cond:
    --
    --   <<emit cond>>
    --   br %cmp, label while.body, while.exit
    --
    -- while.body:
    --
    --   <<emit body>>
    --   br label while.cond
    --
    -- while.exit
    --
    --   <<emit.exit>>
    --
    While cond stms                   -> concat
      [ indent n ++ "br label %" ++ condLabel ++ "\n\n"
      -- cond
      , indent (n-1) ++ condLabel ++ ":\n"
      , gen n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ bodyLabel ++ ", label %" ++ exitLabel ++ "\n"
      , "\n"
      -- body
      , indent (n-1) ++ bodyLabel ++ ":\n"
      , gen n stms
      , indent n ++ "br label %" ++ condLabel ++ "\n"
      , "\n"
      -- exit
      , indent (n-1) ++ exitLabel ++ ":\n"
      , "\n"
      ]

      where

        condLabel = "while" ++ show num ++ ".cond"
        bodyLabel = "body"  ++ show num ++ ".body"
        exitLabel = "exit"  ++ show num ++ ".exit"

        num       = unsafePerformIO getCounter

    For init cond inc stms            -> emitFor n init cond inc stms

    If cond thenStms (Just elseStms)  -> concat
      [ 
      -- cond
        gen n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ thenLabel ++ ", label %" ++ elseLabel ++ "\n"
      , "\n"
      -- then
      , indent (n-1) ++ thenLabel ++ ":\n"
      , gen n thenStms
      , indent n ++ "br label %" ++ endLabel ++ "\n"
      , "\n"
      -- else
      , indent (n-1) ++ elseLabel ++ ":\n"
      , gen n elseStms
      , indent n ++ "br label %" ++ endLabel ++ "\n"
      , "\n"
      -- exit
      , indent (n-1) ++ endLabel ++ ":\n"
      , "\n"
      ]

      where
 
        thenLabel = "if" ++ show num ++ ".then"
        elseLabel = "if" ++ show num ++ ".else"
        endLabel  = "if" ++ show num ++ ".end"

        num       = unsafePerformIO getCounter

    If cond thenStms Nothing          -> concat
      [ 
      -- cond
        gen n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ thenLabel ++ ", label %" ++ endLabel ++ "\n"
      , "\n"
      -- then
      , indent (n-1) ++ thenLabel ++ ":\n"
      , gen n thenStms
      , indent n ++ "br label %" ++ endLabel ++ "\n"
      , "\n"
      -- exit
      , indent (n-1) ++ endLabel ++ ":\n"
      , "\n"
      ]

      where
 
        thenLabel = "if" ++ show num ++ ".then"
        elseLabel = "if" ++ show num ++ ".else"
        endLabel  = "if" ++ show num ++ ".end"

        num       = unsafePerformIO getCounter

    Illuminance posExpr normalExpr angleExpr category stms -> concat 
      [ gen n stms
      , gen n posExpr
      , gen n normalExpr
      , gen n angleExpr
      , indent n ++ "call void @rsl_illuminance("
      , genArgs [posExpr, normalExpr, angleExpr]
      , ");\n"
      ]

      where

        genArgs []     = ""
        genArgs [x]    = emitTy (getTyOfExpr x) ++ " " ++ getReg x
        genArgs (x:xs) = emitTy (getTyOfExpr x) ++ " " ++ getReg x ++ ", " ++ genArgs xs

    NestedFunc n resTy name decls stms  -> ""

    Nil                               -> "null"

    _                                 -> error $ "[CodeGen] TODO: " ++ show expr


  genStatic n expr = case expr of

    TypeCast (Just sym) ty space expr      -> concat
      [ genStatic n expr
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

          dst     = getLLNameOfSym sym
          ty      = emitTy (getTyOfSym sym)
          tmpReg  = getLLNameOfSym sym ++ ".tmp" -- genUniqueReg ++ ".addr"

    Const (Just sym) (S str)    -> concat
      [ indent n
      , dst ++ " = " ++ "getelementptr [" ++ (show $ length str + 1) ++ " x i8]* @" ++ (getNameOfSym sym) ++ ".str, i32 0, i32 0"
      , ";\n"
      ]
        
        where

          dst = getLLNameOfSym sym
          

    Var (Just dstSym) srcSym -> case srcSym of

      (SymVar name _ _ _ KindFormalVariable)  -> concat
        [ indent n ++ emitLoadFromFormalVariable dstSym srcSym ]

      (SymVar name _ _ _ KindBuiltinVariable) -> concat
        [ indent n ++ emitLoadFromBuiltinVariable dstSym srcSym ]

      _                                     -> concat
        [ indent n ++ emitLoadFromVariable dstSym srcSym ]


    UnaryOp (Just sym) op expr           -> concat
      [ genStatic n expr
      , indent n ++ "%" ++ getNameOfSym sym ++ " = "
      , emitOp op ++ " "
      , emitTy (getTyOfSym sym) ++ " zeroinitializer , "
      , getReg expr
      , ";\n"
      ]

    BinOp (Just sym) OpDot e0 e1         -> concat
      [ gen n e0
      , gen n e1
      , emitDotOp n sym e0 e1
      ]

    BinOp (Just sym) op e0 e1            -> concat
      [ genStatic n e0
      , genStatic n e1
      , indent n ++ "%" ++ getNameOfSym sym ++ " = "
      , emitOp op ++ " "
      , emitTy (getTyOfExpr e0) ++ " " ++ getReg e0 ++ " , "
      , getReg e1
      , ";\n"
      ]


    Def    sym Nothing -> concat 
      [ indent n
      , "%" ++ (getNameOfSym sym) ++ " "
      , "= "
      , "alloca "
      , emitTy (getTyOfSym sym)
      , ";\n"
      ]


    Def    sym (Just initExpr)  -> concat 
      [ indent n
      , "%" ++ (getNameOfSym sym) ++ " "
      , "= "
      , "alloca "
      , emitTy (getTyOfSym sym)
      , ";\n"
      , genStatic n initExpr
      --
      , indent n ++ "store "
      , emitTy (getTyOfExpr initExpr) ++ " " ++ (getReg initExpr) ++ " , "
      , emitTy (getTyOfExpr initExpr) ++ "* " ++ "%" ++ (getNameOfSym sym)
      , ";\n"
      ]


    -- shadervar(var) = b
    -- -> call @rsl_setVar(b)
    -- 
    -- a = b
    -- -> store b, a
    
    Assign _ op (Var _ sym) rexpr -> case sym of

      (SymVar _ _ _ _ KindBuiltinVariable) -> concat

        [ genStatic n rexpr
        , indent n
        , "call void "
        , "@rsl_set" ++ getNameOfSym sym ++ "( "
        , emitTy (getTyOfExpr rexpr) ++ " "
        , getReg rexpr ++ " "
        , ");\n"
        ]


      (SymVar _ _ _ _ kind) -> concat

        [ genStatic n rexpr
        , indent n
        , "store "
        , emitTy (getTyOfExpr rexpr) ++ " "
        , getReg rexpr ++ ", "
        , emitTy (getTyOfExpr rexpr) ++ "* "
        , dstName
        , ";\n"
        ]

        where

          dstName = case kind of
            KindFormalVariable -> getLLNameOfSym sym ++ ".addr"
            _                  -> getLLNameOfSym sym


    --
    -- If call was a texturue function, emit cache saver.
    -- 
    Call (Just dst) (SymBuiltinFunc name retTy argTys _) args  -> concat 
      [ genStatic n args
      , indent n ++ tmpReg ++ " = alloca " ++ (emitTy retTy) ++ ";\n" -- TODO: consider void case
      , indent n ++ "call void @" ++ name ++ "_" ++ getFunctionSuffix retTy argTys ++ "("
      , genArgForRet ++ (if (length args) > 0 then ", " else "") ++ genArgs args
      , ");\n"
      , indent n ++ "%" ++ (getNameOfSym dst) ++ " = "
      , "load " ++ emitTy (getTyOfSym dst) ++ "* " ++ tmpReg ++ ";\n"
      --
      , saveCache
      ]

      where

        genArgForRet = if (retTy == TyVoid) then "" else (emitTy retTy) ++ "* " ++ tmpReg
        genArgs []     = ""
        genArgs [x]    = emitTy (getTyOfExpr x) ++ " " ++ getReg x
        genArgs (x:xs) = emitTy (getTyOfExpr x) ++ " " ++ getReg x ++ ", " ++ genArgs xs

        saveCache      = case name of
          "texture"   -> emitCacheSaver n 0 dst
          "turb"      -> emitCacheSaver n 1 dst
          "occlusion" -> emitCacheSaver n 2 dst
          _           -> ""

        tmpReg         = "%" ++ getNameOfSym dst ++ ".buf"

    Call (Just dst) (SymFunc name ty _ _ _) args  -> concat 
      [ genStatic n args
      , indent n ++ "%" ++ (getNameOfSym dst) ++ " = "
      , "call " ++ "@" ++ name ++ "()"
      , ";\n"
      ]

      where

        genArgs []     = ""
        genArgs [x]    = genStatic 0 x
        genArgs (x:xs) = genStatic 0 x ++ ", " ++ genArgs xs

    Triple (Just sym) exprs            -> concat
      [ genStatic n exprs
      , emitTriple n (getLLNameOfSym sym) (getReg (exprs !! 0)) (getReg (exprs !! 1)) (getReg (exprs !! 2))
      ]

    Return (Just sym) expr                        -> concat
      [ genStatic n expr
      , indent n ++ "ret " ++ emitTy (getTyOfSym sym) ++ " " ++ (getReg expr)
      , "\n"
      ]

    -- FIXME
    Conditional (Just sym) cond thenExpr elseExpr -> concat
      [ 
      -- cond
        gen n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ thenLabel ++ ", label %" ++ elseLabel ++ "\n"
      , "\n"
      -- then
      , indent (n-1) ++ thenLabel ++ ":\n"
      , gen n thenExpr
      , indent n ++ "br label %" ++ contLabel ++ "\n"
      , "\n"
      -- else
      , indent (n-1) ++ elseLabel ++ ":\n"
      , gen n elseExpr
      , indent n ++ "br label %" ++ contLabel ++ "\n"
      , "\n"
      -- cont
      , indent (n-1) ++ contLabel ++ ":\n"
      , indent n ++ getLLNameOfSym sym ++ " = phi " ++ emitTy (getTyOfSym sym)
      , " [ " ++ getReg thenExpr ++ ", %" ++ thenLabel ++ " ],"
      , " [ " ++ getReg elseExpr ++ ", %" ++ elseLabel ++ " ] ;\n"
      , "\n"
      ]

      where
 
        thenLabel = "if" ++ show num ++ ".then"
        elseLabel = "if" ++ show num ++ ".else"
        contLabel = "if" ++ show num ++ ".cont"

        num       = unsafePerformIO getCounter

    While cond stms                   -> concat
      [ indent n ++ "br label %" ++ condLabel ++ "\n\n"
      -- cond
      , indent (n-1) ++ condLabel ++ ":\n"
      , genStatic n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ bodyLabel ++ ", label %" ++ exitLabel ++ "\n"
      , "\n"
      -- body
      , indent (n-1) ++ bodyLabel ++ ":\n"
      , genStatic n stms
      , indent n ++ "br label %" ++ condLabel ++ "\n"
      , "\n"
      -- exit
      , indent (n-1) ++ exitLabel ++ ":\n"
      , "\n"
      ]

      where

        condLabel = "while" ++ show num ++ ".cond"
        bodyLabel = "body"  ++ show num ++ ".body"
        exitLabel = "exit"  ++ show num ++ ".exit"

        num       = unsafePerformIO getCounter

    -- FIXME:
    For init cond inc stms            -> emitFor n init cond inc stms

    If cond thenStms (Just elseStms)  -> concat
      [ 
      -- cond
        gen n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ thenLabel ++ ", label %" ++ elseLabel ++ "\n"
      , "\n"
      -- then
      , indent (n-1) ++ thenLabel ++ ":\n"
      , gen n thenStms
      , indent n ++ "br label %" ++ endLabel ++ "\n"
      , "\n"
      -- else
      , indent (n-1) ++ elseLabel ++ ":\n"
      , gen n elseStms
      , indent n ++ "br label %" ++ endLabel ++ "\n"
      , "\n"
      -- exit
      , indent (n-1) ++ endLabel ++ ":\n"
      , "\n"
      ]

      where
 
        thenLabel = "if" ++ show num ++ ".then"
        elseLabel = "if" ++ show num ++ ".else"
        endLabel  = "if" ++ show num ++ ".end"

        num       = unsafePerformIO getCounter

    If cond thenStms Nothing          -> concat
      [ 
      -- cond
        gen n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ thenLabel ++ ", label %" ++ endLabel ++ "\n"
      , "\n"
      -- then
      , indent (n-1) ++ thenLabel ++ ":\n"
      , gen n thenStms
      , indent n ++ "br label %" ++ endLabel ++ "\n"
      , "\n"
      -- exit
      , indent (n-1) ++ endLabel ++ ":\n"
      , "\n"
      ]

      where
 
        thenLabel = "if" ++ show num ++ ".then"
        elseLabel = "if" ++ show num ++ ".else"
        endLabel  = "if" ++ show num ++ ".end"

        num       = unsafePerformIO getCounter

    Illuminance posExpr normalExpr angleExpr category stms -> concat 
      [ genStatic n stms
      , genStatic n posExpr
      , genStatic n normalExpr
      , genStatic n angleExpr
      , indent n ++ "call void @rsl_illuminance("
      , genArgs [posExpr, normalExpr, angleExpr]
      , ");\n"
      ]

      where

        genArgs []     = ""
        genArgs [x]    = emitTy (getTyOfExpr x) ++ " " ++ getReg x
        genArgs (x:xs) = emitTy (getTyOfExpr x) ++ " " ++ getReg x ++ ", " ++ genArgs xs

    NestedFunc n resTy name decls stms  -> ""

    Nil                               -> "null"

    _                                 -> error $ "[CodeGen] TODO: " ++ show expr

  genDynamic n e = case e of

    TypeCast (Just sym) ty space expr      -> concat
      [ genDynamic n expr
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

          dst     = getLLNameOfSym sym
          ty      = emitTy (getTyOfSym sym)
          tmpReg  = getLLNameOfSym sym ++ ".tmp" -- genUniqueReg ++ ".addr"

    Const (Just sym) (S str)    -> concat
      [ indent n
      , dst ++ " = " ++ "getelementptr [" ++ (show $ length str + 1) ++ " x i8]* @" ++ (getNameOfSym sym) ++ ".str, i32 0, i32 0"
      , ";\n"
      ]
        
        where

          dst = getLLNameOfSym sym
          

    Var (Just dstSym) srcSym -> case srcSym of

      (SymVar name _ _ _ KindFormalVariable)  -> concat
        [ indent n ++ emitLoadFromFormalVariable dstSym srcSym ]

      (SymVar name _ _ _ KindBuiltinVariable) -> concat
        [ indent n ++ emitLoadFromBuiltinVariable dstSym srcSym ]

      _                                     -> concat
        [ indent n ++ emitLoadFromVariable dstSym srcSym ]


    UnaryOp (Just sym) op expr           -> concat
      [ genDynamic n expr
      , indent n ++ "%" ++ getNameOfSym sym ++ " = "
      , emitOp op ++ " "
      , emitTy (getTyOfSym sym) ++ " zeroinitializer , "
      , getReg expr
      , ";\n"
      ]

    BinOp (Just sym) OpDot e0 e1         -> concat
      [ gen n e0
      , gen n e1
      , emitDotOp n sym e0 e1
      ]

    BinOp (Just sym) op e0 e1            -> concat
      [ genDynamic n e0
      , genDynamic n e1
      , indent n ++ "%" ++ getNameOfSym sym ++ " = "
      , emitOp op ++ " "
      , emitTy (getTyOfExpr e0) ++ " " ++ getReg e0 ++ " , "
      , getReg e1
      , ";\n"
      ]


    Def    sym Nothing -> concat 
      [ indent n
      , "%" ++ (getNameOfSym sym) ++ " "
      , "= "
      , "alloca "
      , emitTy (getTyOfSym sym)
      , ";\n"
      ]


    Def    sym (Just initExpr)  -> concat 
      [ indent n
      , "%" ++ (getNameOfSym sym) ++ " "
      , "= "
      , "alloca "
      , emitTy (getTyOfSym sym)
      , ";\n"
      , genDynamic n initExpr
      --
      , indent n ++ "store "
      , emitTy (getTyOfExpr initExpr) ++ " " ++ (getReg initExpr) ++ " , "
      , emitTy (getTyOfExpr initExpr) ++ "* " ++ "%" ++ (getNameOfSym sym)
      , ";\n"
      ]


    -- shadervar(var) = b
    -- -> call @rsl_setVar(b)
    -- 
    -- a = b
    -- -> store b, a
    
    Assign _ op (Var _ sym) rexpr -> case sym of

      (SymVar _ _ _ _ KindBuiltinVariable) -> concat

        [ genDynamic n rexpr
        , indent n
        , "call void "
        , "@rsl_set" ++ getNameOfSym sym ++ "( "
        , emitTy (getTyOfExpr rexpr) ++ " "
        , getReg rexpr ++ " "
        , ");\n"
        ]


      (SymVar _ _ _ _ kind) -> concat

        [ genDynamic n rexpr
        , indent n
        , "store "
        , emitTy (getTyOfExpr rexpr) ++ " "
        , getReg rexpr ++ ", "
        , emitTy (getTyOfExpr rexpr) ++ "* "
        , dstName
        , ";\n"
        ]

        where

          dstName = case kind of
            KindFormalVariable -> getLLNameOfSym sym ++ ".addr"
            _                  -> getLLNameOfSym sym


    --
    -- If call was a texturue function, emit cache loader.
    -- 
    Call (Just dst) (SymBuiltinFunc name retTy argTys _) args  -> concat 
      [ genDynamic n args
      , loadFromCacheIfPossible
      ]

      where

        loadFromCacheIfPossible = case name of
          "texture"   -> emitCacheLoader n 0 dst
          "turb"      -> emitCacheLoader n 1 dst
          "occlusion" -> emitCacheLoader n 2 dst
          _           -> concat
            [ indent n ++ tmpReg ++ " = alloca " ++ (emitTy retTy) ++ ";\n" -- TODO: consider void case
            , indent n ++ "call void @" ++ name ++ "_" ++ getFunctionSuffix retTy argTys ++ "("
            , genArgForRet ++ (if (length args) > 0 then ", " else "") ++ genArgs args
            , ");\n"
            , indent n ++ "%" ++ (getNameOfSym dst) ++ " = "
            , "load " ++ emitTy (getTyOfSym dst) ++ "* " ++ tmpReg ++ ";\n"
            ]

        genArgForRet = if (retTy == TyVoid) then "" else (emitTy retTy) ++ "* " ++ tmpReg
        genArgs []     = ""
        genArgs [x]    = emitTy (getTyOfExpr x) ++ " " ++ getReg x
        genArgs (x:xs) = emitTy (getTyOfExpr x) ++ " " ++ getReg x ++ ", " ++ genArgs xs

        tmpReg         = "%" ++ getNameOfSym dst ++ ".buf"

    Call (Just dst) (SymFunc name ty _ _ _) args  -> concat 
      [ genDynamic n args
      , indent n ++ "%" ++ (getNameOfSym dst) ++ " = "
      , "call " ++ "@" ++ name ++ "()"
      , ";\n"
      ]

      where

        genArgs []     = ""
        genArgs [x]    = genDynamic 0 x
        genArgs (x:xs) = genDynamic 0 x ++ ", " ++ genArgs xs

    Triple (Just sym) exprs            -> concat
      [ genDynamic n exprs
      , emitTriple n (getLLNameOfSym sym) (getReg (exprs !! 0)) (getReg (exprs !! 1)) (getReg (exprs !! 2))
      ]

    Return (Just sym) expr                        -> concat
      [ genDynamic n expr
      , indent n ++ "ret " ++ emitTy (getTyOfSym sym) ++ " " ++ (getReg expr)
      , "\n"
      ]

    Conditional (Just sym) cond thenExpr elseExpr -> concat
      [ 
      -- cond
        gen n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ thenLabel ++ ", label %" ++ elseLabel ++ "\n"
      , "\n"
      -- then
      , indent (n-1) ++ thenLabel ++ ":\n"
      , gen n thenExpr
      , indent n ++ "br label %" ++ contLabel ++ "\n"
      , "\n"
      -- else
      , indent (n-1) ++ elseLabel ++ ":\n"
      , gen n elseExpr
      , indent n ++ "br label %" ++ contLabel ++ "\n"
      , "\n"
      -- cont
      , indent (n-1) ++ contLabel ++ ":\n"
      , indent n ++ getLLNameOfSym sym ++ " = phi " ++ emitTy (getTyOfSym sym)
      , " [ " ++ getReg thenExpr ++ ", %" ++ thenLabel ++ " ],"
      , " [ " ++ getReg elseExpr ++ ", %" ++ elseLabel ++ " ] ;\n"
      , "\n"
      ]

      where
 
        thenLabel = "if" ++ show num ++ ".then"
        elseLabel = "if" ++ show num ++ ".else"
        contLabel = "if" ++ show num ++ ".cont"

        num       = unsafePerformIO getCounter

    While cond stms                   -> concat
      [ indent n ++ "br label %" ++ condLabel ++ "\n\n"
      -- cond
      , indent (n-1) ++ condLabel ++ ":\n"
      , genDynamic n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ bodyLabel ++ ", label %" ++ exitLabel ++ "\n"
      , "\n"
      -- body
      , indent (n-1) ++ bodyLabel ++ ":\n"
      , genDynamic n stms
      , indent n ++ "br label %" ++ condLabel ++ "\n"
      , "\n"
      -- exit
      , indent (n-1) ++ exitLabel ++ ":\n"
      , "\n"
      ]

      where

        condLabel = "while" ++ show num ++ ".cond"
        bodyLabel = "body"  ++ show num ++ ".body"
        exitLabel = "exit"  ++ show num ++ ".exit"

        num       = unsafePerformIO getCounter

    -- FIXME:
    For init cond inc stms            -> emitFor n init cond inc stms

    If cond thenStms (Just elseStms)  -> concat
      [ 
      -- cond
        gen n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ thenLabel ++ ", label %" ++ elseLabel ++ "\n"
      , "\n"
      -- then
      , indent (n-1) ++ thenLabel ++ ":\n"
      , gen n thenStms
      , indent n ++ "br label %" ++ endLabel ++ "\n"
      , "\n"
      -- else
      , indent (n-1) ++ elseLabel ++ ":\n"
      , gen n elseStms
      , indent n ++ "br label %" ++ endLabel ++ "\n"
      , "\n"
      -- exit
      , indent (n-1) ++ endLabel ++ ":\n"
      , "\n"
      ]

      where
 
        thenLabel = "if" ++ show num ++ ".then"
        elseLabel = "if" ++ show num ++ ".else"
        endLabel  = "if" ++ show num ++ ".end"

        num       = unsafePerformIO getCounter

    If cond thenStms Nothing          -> concat
      [ 
      -- cond
        gen n cond
      , indent n ++ "br i1 " ++ (getReg cond) ++ ", label %" ++ thenLabel ++ ", label %" ++ endLabel ++ "\n"
      , "\n"
      -- then
      , indent (n-1) ++ thenLabel ++ ":\n"
      , gen n thenStms
      , indent n ++ "br label %" ++ endLabel ++ "\n"
      , "\n"
      -- exit
      , indent (n-1) ++ endLabel ++ ":\n"
      , "\n"
      ]

      where
 
        thenLabel = "if" ++ show num ++ ".then"
        elseLabel = "if" ++ show num ++ ".else"
        endLabel  = "if" ++ show num ++ ".end"

        num       = unsafePerformIO getCounter


    Illuminance posExpr normalExpr angleExpr category stms -> concat 
      [ genDynamic n stms
      , genDynamic n posExpr
      , genDynamic n normalExpr
      , genDynamic n angleExpr
      , indent n ++ "call void @rsl_illuminance("
      , genArgs [posExpr, normalExpr, angleExpr]
      , ");\n"
      ]

      where

        genArgs []     = ""
        genArgs [x]    = emitTy (getTyOfExpr x) ++ " " ++ getReg x
        genArgs (x:xs) = emitTy (getTyOfExpr x) ++ " " ++ getReg x ++ ", " ++ genArgs xs

    NestedFunc n resTy name decls stms  -> ""

    Nil                               -> "null"

  genGlobal e = emitGlobal e

instance AST FormalDecl where

  gen n decl = case decl of

    FormalDecl ty os name Nothing    -> emitTy ty ++ " " ++ "%" ++ name
    FormalDecl ty os name (Just val) -> emitTy ty ++ " " ++ "%" ++ name


  genList n []     = ""
  genList n [x]    = gen n x
  genList n (x:xs) = gen n x ++ ", " ++ gen n xs

  genStatic n decl = case decl of

    FormalDecl ty os name Nothing    -> emitTy ty ++ " " ++ "%" ++ name
    FormalDecl ty os name (Just val) -> emitTy ty ++ " " ++ "%" ++ name


  genStaticList n []     = ""
  genStaticList n [x]    = genStatic n x
  genStaticList n (x:xs) = genStatic n x ++ ", " ++ genStatic n xs

  genDynamic n decl = case decl of

    FormalDecl ty os name Nothing    -> emitTy ty ++ " " ++ "%" ++ name
    FormalDecl ty os name (Just val) -> emitTy ty ++ " " ++ "%" ++ name


  genDynamicList n []     = ""
  genDynamicList n [x]    = genDynamic n x
  genDynamicList n (x:xs) = genDynamic n x ++ ", " ++ genDynamic n xs

  genGlobal decl = "" -- TODO
  genGlobalList []     = ""
  genGlobalList [x]    = genGlobal x
  genGlobalList (x:xs) = genGlobal x ++ genGlobal xs
  

emitStoreFormalVariableToBuffer :: Int -> FormalDecl -> String
emitStoreFormalVariableToBuffer n (FormalDecl ty os name _) = concat
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
emitBuiltinVariableSetter (SymVar name ty _ _ _) =
  "declare void @rsl_set" ++ name ++ "(" ++ tyStr ++ ")\n"

  where

    tyStr = emitTy ty

emitBuiltinVariableGetter :: Symbol -> String
emitBuiltinVariableGetter (SymVar name ty _ _ _) =
  "declare " ++ tyStr ++ " @rsl_get" ++ name ++ "()\n"
  
  where

    tyStr = emitTy ty

getSuffix :: Type -> String
getSuffix ty = case ty of
  TyVector -> "v"
  TyVoid   -> ""      -- no suffix letter
  TyInt    -> "i"
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
  , if ((retTy /= TyVoid) && (length argTys) > 0) then ", " else ""
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
  Const (Just sym) (S str)  -> "@" ++ (getNameOfSym sym) ++ ".str = internal constant [" ++ show (length str + 1) ++ " x i8] c\"" ++ str ++ "\\00\"\n"
  Const _ _                 -> ""
  TypeCast _ _ _ expr       -> emitGlobal expr
  Var _ sym                 -> ""
  Assign _ _ lexpr rexpr    -> emitGlobal lexpr ++ emitGlobal rexpr
  Def _ Nothing             -> ""
  Def _ (Just expr)         -> emitGlobal expr
  UnaryOp _ _ expr          -> emitGlobal expr
  BinOp _ _ expr0 expr1     -> emitGlobal expr0 ++ emitGlobal expr1
  Call _ _ exprs            -> concatMap emitGlobal exprs
  Triple _ exprs            -> concatMap emitGlobal exprs
  Conditional _ cond thenE elseE -> emitGlobal cond ++ emitGlobal thenE ++ emitGlobal elseE
  While cond stmt           -> emitGlobal cond ++ (concatMap emitGlobal stmt)
  For init cond inc stmt    -> emitGlobal init ++ emitGlobal cond ++ emitGlobal inc ++ (concatMap emitGlobal stmt)
  If  cond thenStmt Nothing -> emitGlobal cond ++ (concatMap emitGlobal thenStmt)
  If  cond thenStmt (Just elseStmt) -> emitGlobal cond ++ (concatMap emitGlobal thenStmt) ++ (concatMap emitGlobal elseStmt)
  Illuminance _ _ _ _ stmt  -> concatMap emitGlobal stmt
  NestedFunc _ _ _ _ _      -> "" -- TODO
  {- TODO
  | If        Expr                        -- condition
              [Expr]                      -- statement
              (Maybe [Expr])              -- else statement
  | Extract  (Maybe Symbol)
              Char                        -- x, y, z, or w
              Expr                        -- Should be vector expr.
  -}
  _                       -> error $ "[CodeGen] emitGlobal: TODO: " ++ show e


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

    -- TODO
    UserFunc ty name -> concat
      [ "defined void "
      , "@" ++ name
      , "("
      , ") {}\n"
      ]

    Preprocessor s -> ""

  genStatic n f = case f of

    ShaderFunc ty name decls stms -> concat 
      [ "define void "
      , "@" ++ name ++ "_cache_gen_pass"
      , "("
      , genStatic n decls
      , ") {\n"
      , concatMap (emitStoreFormalVariableToBuffer (n+1)) decls
      , genStatic (n+1) stms
      , "\n" ++ indent (n+1) ++ "ret void;\n"
      , "\n}\n"
      ]

    -- TODO
    UserFunc ty name -> concat
      [ "defined void "
      , "@" ++ name
      , "("
      , ") {}\n"
      ]

    Preprocessor s -> ""

  genDynamic n f = case f of

    ShaderFunc ty name decls stms -> concat 
      [ "define void "
      , "@" ++ name ++ "_dynamic_pass"
      , "("
      , genDynamic n decls
      , ") {\n"
      , concatMap (emitStoreFormalVariableToBuffer (n+1)) decls
      , genDynamic (n+1) stms
      , "\n" ++ indent (n+1) ++ "ret void;\n"
      , "\n}\n"
      ]

    -- TODO
    UserFunc ty name -> concat
      [ "defined void "
      , "@" ++ name
      , "("
      , ") {}\n"
      ]


    Preprocessor s -> ""

  genGlobal f = case f of
    ShaderFunc ty name decls stms -> concat 
      [
      -- emit shader param def
        emitShaderParamStructDef decls
      , "\n"

      -- emit frame struct for nested function if exist.
      , emitFrameStructDefs stms
      , "\n"

      -- emit definition of nested function.
      , emitNestedFunctionDefs stms
      , "\n"
      
      -- emit global values
      , genGlobal stms
      , "\n"
      ]

    -- TODO
    UserFunc ty name -> concat
      [ "defined void "
      , "@" ++ name
      , "("
      , ") {}\n"
      ]


    Preprocessor s -> ""
  
-- codeGenLLVM ast = gen ast

--
-- Emit definition for struct of shader parameters.
--
emitShaderParamStructDef :: [FormalDecl] -> String
emitShaderParamStructDef decls = concat
  [ "%struct._shader_param_t = type <{ "
  , emitTys decls
  , if (align16 - structSize) > 0 then delim ++ emitPad (align16 - structSize)
                                  else ""
  , " }>; 16 byte align\n"
  , "\n"
  , emitShaderParamStructSizeGetter align16
  , "\n"
  , emitShaderParamGetters 0 decls
  , "\n"
  , emitShaderParamSetters 0 decls
  , "\n"
  ]

  where

    delim = if length decls == 0 then "" else ", "

    emitTys []                           = ""
    emitTys [(FormalDecl ty _ _ _ )]     = emitTy ty
    emitTys ((FormalDecl ty _ _ _ ):xs)  = emitTy ty ++ ", " ++ emitTys xs

    structSize = sum $ map (getLLVMTySize . getTyField) decls

    align16 = ((structSize `div` 16) + 1) * 16

    getTyField :: FormalDecl -> Type
    getTyField (FormalDecl ty _ _ _) = ty

    emitPad :: Int -> String
    emitPad 0 = ""
    emitPad 1 = "i8"
    emitPad n = "i8, " ++ emitPad (n-1)

    emitShaderParamGetters :: Int -> [FormalDecl] -> String
    emitShaderParamGetters n []        = ""
    emitShaderParamGetters n [decl]    = emitShaderParamGetter n decl
    emitShaderParamGetters n (decl:ds) = emitShaderParamGetter n decl ++ "\n" ++
                                         emitShaderParamGetters (n+1) ds

    emitShaderParamSetters :: Int -> [FormalDecl] -> String
    emitShaderParamSetters n []        = ""
    emitShaderParamSetters n [decl]    = emitShaderParamSetter n decl
    emitShaderParamSetters n (decl:ds) = emitShaderParamSetter n decl ++ "\n" ++
                                         emitShaderParamSetters (n+1) ds

emitShaderParamStructSizeGetter :: Int -> String
emitShaderParamStructSizeGetter sz = concat
  [ "define i32 @get_shader_param_struct_size() {\n"
  , "entry:\n"
  , indent 1 ++ "ret i32 " ++ show sz ++ "\n"
  , "}\n"
  ]

--
-- Emit function which set/get shader variable.
--
emitShaderParamGetter :: Int -> FormalDecl -> String
emitShaderParamGetter offset (FormalDecl ty _ name _) = concat
  [ "define void @set_shader_param_" ++ name ++ "(%struct._shader_param_t* %param, " ++ emitTy ty ++" %x) {\n"
  , "entry:\n"
  , indent 1 ++ "%tmp = getelementptr %struct._shader_param_t* %param, i32 0, i32 " ++ show offset ++ "\n"
  , indent 1 ++ "store " ++ emitTy ty ++ " %x, " ++ emitTy ty ++ " *%tmp\n"
  , indent 1 ++ "ret void\n"
  , "}\n"
  ]


emitShaderParamSetter :: Int -> FormalDecl -> String
emitShaderParamSetter offset (FormalDecl ty _ name _) = concat
  [ "define " ++ emitTy ty ++ " @get_shader_param_" ++ name ++ "(%struct._shader_param_t *%param) {\n"
  , "entry:\n"
  , indent 1 ++ "%tmp = getelementptr %struct._shader_param_t* %param, i32 0, i32 " ++ show offset ++ "\n"
  , indent 1 ++ "%val = load " ++ emitTy ty ++ "* %tmp\n"
  , indent 1 ++ "ret " ++ emitTy ty ++ " %val\n"
  , "}\n"
  ]


--
-- Functions for handling nested function
--

{-
emitStoreExternVariableToBuffer :: [Symbols] -> (String, Int) -> String
emitStoreExternVariableToBuffer syms (fname, n) = concat
  -- %var.addr = alloca ty
  -- store %var, %var.addr
  [ indent n ++ buf ++ " = " ++ "alloca " ++ tyStr ++ ";\n"
  , indent n ++ "store " ++ tyStr ++ " " ++ src ++ " , " ++ tyStr ++ "* " ++ buf
  , ";\n"
  ]

  where

    frameName     = "%struct.frame." ++ fname ++ show n
    chainName     = "%chain." ++ fname ++ show n
    chainAddrName = chainName ++ ".addr"

    nsyms = zip syms [1..length (syms)] -- [a, b, c] -> [(1, a),(2, b),(3, c)]

    tyStr  = emitTy ty
    buf = "%" ++ name ++ ".addr"
    src = "%" ++ name
-}

extractExternVariables :: [Expr] -> [Symbol]
extractExternVariables exprs = map getSym (filter isExternVariableDef exprs)

  where

    isExternVariableDef e = case e of
      (Def (SymVar name _ _ _ kind) initExpr) -> if kind == KindExternalVariable then True else False
      _                                       -> False

    getSym e = case e of
      (Def sym _) -> sym
    


mkFrameStruct :: [Expr] -> String
mkFrameStruct stms = show $ extractExternVariables stms

emitFrameStructDef :: Expr -> String
emitFrameStructDef (NestedFunc n ty fname decls stms) = 

  if length syms > 0

    then

      concat

      [ "%struct.frame." ++ fname ++ (show n) ++ " = type <{ "
      , emitTys syms
      , "}>\n"
      ]

    else

      concat

      [ "%struct.frame." ++ fname ++ (show n) ++ " = type <{ "
      , "i8" ++ (concat $ replicate 15 ", i8")
      , "}>\n"
      ]
      
  where

    syms = extractExternVariables stms

    emitTys :: [Symbol] -> String
    emitTys []                      = ""
    emitTys [(SymVar _ ty _ _ _)]     = emitTy ty
    emitTys ((SymVar _ ty _ _ _):xs)  = emitTy ty ++ ", " ++ emitTys xs

emitFrameStructDef _ = ""

emitFrameStructDefs :: [Expr] -> String
emitFrameStructDefs []      = ""
emitFrameStructDefs [e]     = emitFrameStructDef e
emitFrameStructDefs (e:es)  = emitFrameStructDef e ++ emitFrameStructDefs es


emitNestedFunctionDef :: Expr -> String
emitNestedFunctionDef (NestedFunc n ty fname decls stms) = concat
  [ "define " ++ emitTy ty ++ " @" ++ fname ++ "." ++ show n ++ "(" ++ frameName ++ "* nest " ++ chainName ++ ", " ++ gen 0 decls ++ ")"
  , "{\n"
  , indent 1 ++ chainAddrName ++" = alloca " ++ frameName ++ "*\n"
  , indent 1 ++ "store " ++ frameName ++ "* " ++ chainName ++ ", " ++ frameName ++ "** " ++ chainAddrName ++ "\n"
  , concatMap (emitStoreFormalVariableToBuffer 1) decls
  , gen 1 stms
  , "}\n"
  ]

  where

    frameName     = "%struct.frame." ++ fname ++ show n
    chainName     = "%chain." ++ fname ++ show n
    chainAddrName = chainName ++ ".addr"

emitNestedFunctionDef _ = ""

emitNestedFunctionDefs :: [Expr] -> String
emitNestedFunctionDefs []      = ""
emitNestedFunctionDefs [e]     = emitNestedFunctionDef e
emitNestedFunctionDefs (e:es)  = emitNestedFunctionDef e ++ emitNestedFunctionDefs es
