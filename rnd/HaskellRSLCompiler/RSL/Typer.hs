-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.Typer
---- Copyright   :  (c) Syoyo Fujita
---- License     :  BSD-style
----
---- Maintainer  :  syoyo@lucillerender.org
---- Stability   :  experimental
---- Portability :  GHC 6.8
----
---- RSLParser   :  Typing module for untyped RSL AST.
----
-------------------------------------------------------------------------------

module RSL.Typer where

import Control.Monad.State

import RSL.AST
import RSL.Sema

getUniqueName :: State Int String
getUniqueName = do  { n <- get
                    ; let n' = n + 1
                    ; put n'
                    ; return $ "tmp" ++ (show n')
                    } 

class Typer a where
  typing :: a -> State Int a

getReturnTypeOfFunc :: String -> (Maybe Type)
getReturnTypeOfFunc name = case (lookupFunc builtinShaderFunctions name) of
  []  -> Nothing
  [x] -> (Just (getTyOfSym x))
  xs  -> Nothing          -- TODO

instance Typer Expr where
  typing e = case e of
    Var _ sym ->
      do { tmpName <- getUniqueName
         ; let ty = getTyOfSym sym
         ; return (Var (Just (SymVar tmpName ty Uniform KindVariable)) sym)
         }

    Const _ (F fval) ->
      do { tmpName <- getUniqueName
         ; return (Const (Just (SymVar tmpName TyFloat Uniform KindVariable)) (F fval))
         }

    UnaryOp _ op e ->
      do  { e' <- typing e
          ; tmpName <- getUniqueName
          ; let ty  = getTyOfExpr e'
          ; let sym = (SymVar tmpName ty Uniform KindVariable)
          ; return (UnaryOp (Just sym) op e')
          }
    
    BinOp _ op e0 e1 ->
      do  { e0' <- typing e0
          ; e1' <- typing e1
          ; tmpName <- getUniqueName
          ; let ty  = getTyOfExpr e0'
          ; let sym = (SymVar tmpName ty Uniform KindVariable)
          ; return (BinOp (Just sym) op e0' e1')
          }

    Assign _ op lhs rhs ->
      do  { rhs' <- typing rhs
          ; lhs' <- typing lhs
          ; return (Assign Nothing op lhs' rhs')
          }

    Call _ sym exprs   ->
      do  { exprs' <- mapM typing exprs
          ; tmpName <- getUniqueName
          ; case getReturnTypeOfFunc (getNameOfSym sym) of
              Nothing   -> error $ "TODO" ++ (show e)
              (Just ty) -> let sym' = (SymVar tmpName ty Uniform KindVariable) in
                           return (Call (Just sym') sym exprs')
          }

    Def ty name Nothing -> 
      do  { return (Def ty name Nothing) }

    Def ty name (Just initExpr) -> 
      do  { initExpr' <- typing initExpr
          ; return (Def ty name (Just initExpr'))
          }

    _ -> error $ "Typing: TODO: " ++ (show e)
  
instance Typer Func where
  typing f = case f of
    ShaderFunc ty name decls stmt ->
      do { stmt' <- mapM typing stmt
         ; return (ShaderFunc ty name decls stmt') }

typingAST :: [Func] -> [Func]
typingAST []     = []
typingAST [x]    = [evalState (typing x) 0]
typingAST (x:xs) = [evalState (typing x) 0]
