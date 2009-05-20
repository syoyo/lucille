-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.CodeGenC
---- Copyright   :  (c) Syoyo Fujita
---- License     :  Modified BSD
----
---- Maintainer  :  syoyo@lucillerender.org
---- Stability   :  experimental
---- Portability :  GHC 6.8
----
---- RSL.CodeGenC :  C/C++ code generator for LLVM IR from RSL AST representation.
----
-------------------------------------------------------------------------------

module RSL.CodeGenC where

import Control.Monad.State
import Numeric
import Foreign
import Foreign.C.Types
import Data.IORef
import System.IO.Unsafe           -- A magical module ;-)

import Text.PrettyPrint.Leijen    -- wl-pprint

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

genUniqueID  :: Int
genUniqueID  = unsafePerformIO getCounter

cTy ty = case ty of
  TyVoid        -> "void"
  TyInt         -> "int"
  TyString      -> "char *"
  TyFloat       -> "float"
  TyVector      -> "li_vector_t"
  TyColor       -> "li_vector_t"
  TyPoint       -> "li_vector_t"
  TyNormal      -> "li_vector_t"
  TyMatrix      -> "li_matrix_t"
  TyBool        -> "int"
  TyArray n ty  -> cTy ty ++ "*"

enumTy ty = case ty of
  TyFloat       -> "LI_TYPE_FLOAT"
  TyVector      -> "LI_TYPE_VECTOR"
  TyColor       -> "LI_TYPE_COLOR"
  TyPoint       -> "LI_TYPE_POINT"
  TyNormal      -> "LI_TYPE_NORMAL"
  TyMatrix      -> "LI_TYPE_MATRIX"

getTySize ty = case ty of
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


-- data FormalDecl 
--   = FormalDecl Type (Maybe OutputSpec) String (Maybe Expr)     -- TODO: Allow const expression
--     deriving (Show, Eq, Typeable, Data)
-- 
-- data Func 
--   = ShaderFunc ShaderType String [FormalDecl] [Expr]
--   | UserFunc   Type       String
--   | StructDef             String [FormalDecl]                  -- RSL2
--   | Preprocessor String
--     deriving (Show, Eq, Typeable, Data)
tst = text "muda"


formalDecl :: FormalDecl -> Doc
formalDecl (FormalDecl ty outSpec name _) = text (cTy ty) <+> text name

expr :: Expr -> Doc
expr e = text "bora"

toplevel :: Func -> Doc
toplevel (ShaderFunc ty name decls stmt) = text "void" <+> text name
                                                       <>  encloseSep lparen rparen comma (map formalDecl decls)
                                                       <+> nest 4 (braces (vsep (map expr stmt)))
toplevel (Preprocessor _)                = empty                        -- No codegen for preprocessor line


paramInfo :: String -> [FormalDecl] -> Doc
paramInfo name decls = 
  text "int" <$> text (name ++ "_shader_params_info")
             <>  nest 4 (lparen <$> text "int *ninfos,"
                                <$> text "li_shader_param_info_t **infos"
                                <$> rparen)
             <$> nest 4 (lbrace <$> paramFields decls
                                <$> text "(*ninfos) = sizeof(infolist) / sizeof(li_shader_param_info_t);"
                                <$> text "(*infos)  = infolist;")
             <$> rbrace

  where

    -- TODO: Support arary.
    paramField :: FormalDecl -> Doc
    paramField (FormalDecl ty outSpec name _) = 
      encloseSep lbrace rbrace comma ([ dquotes $ text name
                                      , text $ enumTy ty
                                      , text "0"
                                      , text "1"
                                      , text $ "OFFSET_" ++ name])

    paramFields :: [FormalDecl] -> Doc
    paramFields decls =
      nest 4 (text "static li_shader_param_info_t infolist[] = " <> encloseSep lbrace rbrace comma (map paramField decls) <> semi)


calcOffsetTable :: [FormalDecl] -> [Int]
calcOffsetTable decls = drop 1 $ scanl (\x (FormalDecl ty _ _ _) -> x + getTySize ty) 0 decls

offsetLine (name, offt) = text ("#define OFFSET_" ++ name) <+> parens (text $ show offt)

offsetTables :: [FormalDecl] -> Doc
offsetTables decls = vsep $ map offsetLine (zip (map (\(FormalDecl _ _ name _) -> name) decls) (calcOffsetTable decls))

paramHeader :: Func -> Doc
paramHeader (ShaderFunc ty name decls stmt) = offsetTables decls <$> empty <$> paramInfo name decls
paramHeader _                               = empty


includeHeaderLine = text "#include <li_shader.h>"

-- | Code gen C code from parsed RSL tree.
genCode :: RSLUnit -> String
genCode fs = show $ includeHeaderLine <$> vsep (map paramHeader fs) <$> vsep (map toplevel fs)

