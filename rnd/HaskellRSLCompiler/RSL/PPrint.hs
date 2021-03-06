-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.PPrint
---- Copyright   :  (c) Syoyo Fujita
---- License     :  Modified BSD
----
---- Maintainer  :  syoyo@lucillerender.org
---- Stability   :  experimental
---- Portability :  GHC 6.8
----
---- RSLParser   :  Pretty printer for RSL AST.
----
-------------------------------------------------------------------------------

module RSL.PPrint where

import RSL.AST


indent :: Int -> String
indent n = concat $ replicate (4 * n) " "


class AST a where

  pprint :: Int -> a -> String
  pprintList :: Int -> [a] -> String
  pprintList n = concat . map (pprint n)


instance AST a => AST [a] where

  pprint = pprintList

instance AST ShaderType where

  pprint n ty = case ty of

    Surface       -> "surface"
    Volume        -> "volume"
    Displacement  -> "displacement"
    Imager        -> "imager"


instance AST Type where

  pprint n ty = case ty of

    TyUnknown     -> "uknown"
    TyVoid        -> "void"
    TyFloat       -> "float"
    TyVector      -> "vector"
    TyColor       -> "color"
    TyPoint       -> "point"
    TyNormal      -> "normal"
    TyMatrix      -> "matrix"
    TyString      -> "string"
    TyArray n aty -> pprint n aty

emitOp op = case op of
  OpAdd       -> "+"
  OpSub       -> "-"
  OpMul       -> "*"
  OpDiv       -> "/"
  OpDot       -> "."
  OpNeg       -> "!"
  OpLe        -> "<="
  OpLt        -> "<"
  OpGe        -> ">="
  OpGt        -> ">"
  OpEq        -> "=="
  OpNeq       -> "!="
  OpOr        -> "||"
  OpAnd       -> "&&"
  OpAssign    -> "="
  OpAddAssign -> "+="
  OpSubAssign -> "-="
  OpMulAssign -> "*="
  OpDivAssign -> "/="

instance AST Expr where
  
  pprint n expr = case expr of

    TypeCast _ ty space expr      -> concat
      [ "( ("
      , pprint 0 ty ++ " \"" ++ space ++ "\" "
      , ") "
      , pprint 0 expr
      , " )"
      ]

    Const _ (F val)               -> show val

    Const _ (S val)               -> "\"" ++ val ++ "\""

    Var _  (SymVar name _ _ _ _)  -> name

    UnaryOp _ op expr             -> concat
      [ "( "
      , emitOp op ++ " "
      , pprint 0 expr
      , " )"
      ]

    BinOp _ op e0 e1              -> concat
      [ "( "
      , pprint 0 e0 -- left
      , " " ++ emitOp op ++ " "
      , pprint 0 e1 -- right
      , " )"
      ]


    Def   sym Nothing -> concat 

      [ indent n
      , pprint 0 (getTyOfSym sym) ++ " " ++ (getNameOfSym sym)
      , arrSuffix ++ ";\n"
      ]

      where

        arrSuffix = case getTyOfSym sym of
          (TyArray m ty) -> "[" ++ show m ++ "]"
          _              -> ""


    Def   sym (Just initExpr)  -> concat 

      [ indent n
      , pprint 0 (getTyOfSym sym) ++ " " ++ (getNameOfSym sym)
      , arrSuffix
      , " = "
      , pprint 0 initExpr
      , ";\n"
      ]

      where

        arrSuffix = case getTyOfSym sym of
          (TyArray m ty) -> "[" ++ show m ++ "]"
          _              -> ""


    Assign _ op lexpr rexpr -> concat 
      [ indent n
      , pprint 0 lexpr
      , " " ++ emitOp op ++ " "
      , pprint 0 rexpr
      , ";\n"
      ]


    Call _ (SymFunc name ty _ _ _) args  -> concat 
      [ name
      , "("
      , pprintArgs args
      , ")"
      ]

      where

        pprintArgs []     = ""
        pprintArgs [x]    = pprint 0 x
        pprintArgs (x:xs) = pprint 0 x ++ ", " ++ pprintArgs xs

    Call _ (SymBuiltinFunc name ty _ _) args  -> concat 
      [ name
      , "("
      , pprintArgs args
      , ")"
      ]

      where

        pprintArgs []     = ""
        pprintArgs [x]    = pprint 0 x
        pprintArgs (x:xs) = pprint 0 x ++ ", " ++ pprintArgs xs

    Triple _ exprs                    -> concat
      [ "( "
      , pprint 0 (exprs !! 0)
      , ", "
      , pprint 0 (exprs !! 1)
      , ", "
      , pprint 0 (exprs !! 2)
      , " )"
      ]

    Array _ idxExpr expr              -> concat
      [ pprint 0 expr
      , "["
      , pprint 0 idxExpr
      , "]"
      ]

    EList exprs                           -> concat
      [ "{"
      , pprintEList exprs
      , "}"
      ]

      where

        pprintEList []     = ""
        pprintEList [x]    = pprint 0 x
        pprintEList (x:xs) = pprint 0 x ++ ", " ++ pprintEList xs

    Conditional _ cond thenExpr elseExpr  -> concat
      [ "( " ++ pprint 0 cond ++ " )"
      , " ? "
      , "( " ++ pprint 0 thenExpr ++ " )"
      , " : "
      , "( " ++ pprint 0 thenExpr ++ " )"
      ]

    While cond stms                   -> concat
      [ indent n
      , "while ( "
      , pprint 0 cond
      , " ) {\n"
      , pprint (n+1) stms
      , "\n" ++ indent n ++ "}"
      ]

    For init cond step stms           -> concat
      [ indent n
      , "for ( "
      , pprint 0 init
      , "; "
      , pprint 0 cond
      , "; "
      , pprint 0 step
      , " ) {\n"
      , pprint (n+1) stms
      , "\n" ++ indent n ++ "}"
      ]

    Illuminance position normal angle category stms -> concat
      [ indent n
      , "illuminance ( "
      , pprint 0 position
      , ", "
      , pprint 0 normal
      , ", "
      , pprint 0 angle
      , " ) {\n"
      , pprint (n+1) stms
      , "\n" ++ indent n ++ "}"
      ]

    If cond thenStms (Just elseStms)  -> concat
      [ indent n
      , "if ( "
      , pprint 0 cond 
      , " ) {\n"
      , pprint (n+1) thenStms
      , indent n ++ "} else {\n"
      , pprint (n+1) elseStms
      , indent n ++ "}"
      ]

    If cond thenStms Nothing          -> concat
      [ indent n
      , "if ( "
      , pprint 0 cond 
      , " ) {\n"
      , pprint (n+1) thenStms
      , indent n ++ "}"
      ]

    Return sym expr                   -> concat
      [ indent n ++ "return "
      , pprint 0 expr ++ ";\n"
      ]

    NestedFunc id resTy name decls stms  -> concat
      [ indent n
      , pprint 0 resTy ++ " " ++ name
      , " (" ++ pprint 0 decls ++ ") {\n"
      , pprint (n+1) stms
      , indent n ++ "}"
      ]

    Nil                               -> "null"

instance AST FormalDecl where

  pprint n decl = case decl of

    FormalDecl (TyArray n ty) _ name _  -> pprint n ty ++ " " ++ name ++ "[" ++ show n ++ "]"
    FormalDecl ty _ name _              -> pprint n ty ++ " " ++ name


  pprintList n []     = ""
  pprintList n [x]    = pprint n x
  pprintList n (x:xs) = pprint n x ++ ", " ++ pprint n xs

instance AST Func where

  pprint n f = case f of

    ShaderFunc ty name decls stms -> concat 
      [ pprint n ty
      , " " ++ name
      , "("
      , pprint n decls
      , ") {\n"
      , pprint (n+1) stms
      , "\n}\n"
      ]

    UserFunc ty name -> concat 
      [ pprint n ty
      , " " ++ name
      , "( /* TODO */"
      , ") {\n"
      , "    /* TODO */\n}\n"
      ]

    StructDef name members  -> concat
      [ "struct " ++ name ++ " {\n"
      , pprint (n+1) members
      , "\n"
      , "}\n"
      ]

    Preprocessor s -> ""

