-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.Typer
---- Copyright   :  (c) Syoyo Fujita
---- License     :  Modified BSD
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
import Debug.Trace

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
getReturnTypeOfFunc name = case (lookupBuiltinFunc builtinShaderFunctions name) of
  []  -> Nothing
  [x] -> (Just (getTyOfSym x))
  xs  -> Nothing          -- TODO: Polymorphic function

getReturnTypeOfFuncWithArgumentSignature :: String -> [Type] -> (Maybe Symbol)
getReturnTypeOfFuncWithArgumentSignature name argTys = trace (show argTys) $ case (lookupBuiltinFuncWithArgumentSignature builtinShaderFunctions name argTys) of
  []  -> Nothing
  [x] -> (Just x)         -- Rewrite funcall with proper function signature.
  xs  -> Nothing          -- TODO: Polymorphic function

getArgumentTypeSignature :: [Expr] -> [Type]
getArgumentTypeSignature exprs = map getTyOfExpr exprs


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

    Const _ (S sval) ->
      do { tmpName <- getUniqueName
         ; return (Const (Just (SymVar tmpName TyString Uniform KindVariable)) (S sval))
         }

    TypeCast _ toTy space e ->
      do { e' <- typing e
         ; tmpName <- getUniqueName
         ; return (TypeCast (Just (SymVar tmpName toTy Uniform KindVariable)) toTy space e')
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
          ; case getReturnTypeOfFuncWithArgumentSignature (getNameOfSym sym) (getArgumentTypeSignature exprs') of
              Nothing       -> error $ "[Typer] TODO: " ++ (show e)
              (Just funSym) -> let sym' = (SymVar tmpName (getTyOfSym funSym) Uniform KindVariable) in
                               return (Call (Just sym') funSym exprs')  -- rewrite sym with funSym
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
