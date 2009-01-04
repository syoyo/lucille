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

type TyperState a = State Int a

getUniqueName :: TyperState String
getUniqueName = do  { n <- get
                    ; let n' = n + 1
                    ; put n'
                    ; return $ "tmp" ++ (show n')
                    } 

class Typer a where
  typing :: a -> TyperState a

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


insertFtoV :: Type -> Expr -> TyperState Expr
insertFtoV toTy expr = do { tmpName <- getUniqueName
                          ; let sym = (SymVar tmpName toTy Uniform KindVariable)
                          ; return (TypeCast (Just sym) toTy "" expr)
                          }
--
-- Insert FtoV if required.
--
upcastBinary :: Expr -> Expr -> TyperState (Expr, Expr)
upcastBinary e0 e1 = case (getTyOfExpr e0, getTyOfExpr e1) of
  (TyFloat, TyVector) -> do { e0' <- insertFtoV TyVector e0; return (e0', e1 ) }
  (TyFloat, TyPoint ) -> do { e0' <- insertFtoV TyPoint  e0; return (e0', e1 ) }
  (TyFloat, TyNormal) -> do { e0' <- insertFtoV TyNormal e0; return (e0', e1 ) }
  (TyFloat, TyColor ) -> do { e0' <- insertFtoV TyColor  e0; return (e0', e1 ) }
  (_      , TyVector) -> do {                                return (e0 , e1 ) }
  (_      , TyPoint ) -> do {                                return (e0 , e1 ) }
  (_      , TyNormal) -> do {                                return (e0 , e1 ) }
  (_      , TyColor ) -> do {                                return (e0 , e1 ) }
  (TyVector, TyFloat) -> do { e1' <- insertFtoV TyVector e1; return (e0 , e1') }
  (TyPoint , TyFloat) -> do { e1' <- insertFtoV TyPoint  e1; return (e0 , e1') }
  (TyNormal, TyFloat) -> do { e1' <- insertFtoV TyNormal e1; return (e0 , e1') }
  (TyColor , TyFloat) -> do { e1' <- insertFtoV TyColor  e1; return (e0 , e1') }
  (TyVector, _      ) -> do {                                return (e0 , e1 ) }
  (TyPoint , _      ) -> do {                                return (e0 , e1 ) }
  (TyNormal, _      ) -> do {                                return (e0 , e1 ) }
  (TyColor , _      ) -> do {                                return (e0 , e1 ) }
  _                   -> error $ "[Typer] upcastBinary: TODO: " ++ show e0 ++ " , " ++ show e1

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
          ; (e0'', e1'') <- upcastBinary e0' e1'
          ; tmpName <- getUniqueName
          ; let ty  = getTyOfExpr e0''
          ; let sym = (SymVar tmpName ty Uniform KindVariable)
          ; return (BinOp (Just sym) op e0'' e1'')
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
