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

getUniqueName :: State Int String
getUniqueName = do  { n <- get
                    ; let n' = n + 1
                    ; put n'
                    ; return $ "tmp" ++ (show n')
                    } 

class Typer a where
  typing :: a -> State Int a


instance Typer Expr where
  typing e = case e of
    Var sym ->
      do { return (Var sym) }

    Const _ (F fval) ->
      do { tmpName <- getUniqueName
         ; return (Const (Just (SymVar tmpName TyFloat Uniform KindVariable)) (F fval))
         }
    
    BinOp _ op (e0:e1) ->
      do { return (BinOp Nothing op (e0:e1)) }

    Assign _ op lhs rhs ->
      do  { rhs' <- typing rhs
          ; lhs' <- typing lhs
          ; return (Assign Nothing op lhs' rhs')
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
