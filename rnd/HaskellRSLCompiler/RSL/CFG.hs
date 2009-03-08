-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.CFG
---- Copyright   :  (c) Syoyo Fujita
---- License     :  Modified BSD
----
---- Maintainer  :  syoyo@lucillerender.org
---- Stability   :  experimental
---- Portability :  GHC 6.8
----
---- RSL.CFG     :  Control Flow Graph construction and optimization.
----                Requires fgl, functional graph library package.
----
-------------------------------------------------------------------------------

module RSL.CFG where

-- import Data.Graph
import Data.Graph.Inductive
import Control.Monad.State

import RSL.AST

{-
 - To build a graph, we need unique number for each expression,
 -
 - Given RST.Expr,
 -
 - 1. Assign unique number for each expression node.
 - 2. Build graph from (1).
 -
 -}

type CFGState a = State Int a

getUniqueID :: CFGState Int
getUniqueID = do  { n <- get
                  ; let n' = n + 1
                  ; put n'
                  ; return n'
                  } 

getID :: (Expr, Int, [Int]) -> Int
getID (_, i, _) = i 

getIDMap = map getID

-- Flatten & label each expression node.
numberExpr :: Expr -> CFGState [(Expr, Int, [Int])]
numberExpr e = case e of
  TypeCast _ _ _ expr     -> do { expr' <- numberExpr expr
                                ; id    <- getUniqueID
                                ; return (expr' ++ [(e, id, getIDMap expr')])
                                }

  Const _ _               -> do { id <- getUniqueID
                                ; return [(e, id, [])]
                                }

  Var _ _                 -> do { id <- getUniqueID
                                ; return [(e, id, [])]
                                }

  UnaryOp _ _ expr        -> do { expr' <- numberExpr expr
                                ; id    <- getUniqueID
                                ; return (expr' ++ [(e, id, getIDMap expr')]) 
                                }

  BinOp sym op e0 e1      -> do { e0' <- numberExpr e0
                                ; e1' <- numberExpr e1
                                ; id  <- getUniqueID
                                ; let e' = BinOp sym op Nil Nil
                                ; return (e0' ++ e1' ++ [(e', id, getIDMap e0' ++ getIDMap e1')]) 
                                }

  Def _ Nothing           -> do { id <- getUniqueID
                                ; return [(e, id, [])]
                                }

  Def _ (Just initExpr)   -> do { initExpr' <- numberExpr initExpr
                                ; id        <- getUniqueID
                                ; return (initExpr' ++ [(e, id, getIDMap initExpr')])
                                }

  Assign sym op e0 e1     -> do { e0' <- numberExpr e0
                                ; e1' <- numberExpr e1
                                ; id  <- getUniqueID
                                ; let e' = Assign sym op Nil Nil
                                ; return (e0' ++ e1' ++ [(e', id, getIDMap e0' ++ getIDMap e1')]) 
                                }

  Call _ _ args           -> do { args' <- mapM numberExpr args
                                ; id    <- getUniqueID
                                ; return (concat args' ++ [(e, id, getIDMap (concat args'))]) 
                                }

  _                       -> error $ "[CFG] TODO: " ++ show e

numberExprList :: [Expr] -> CFGState [(Expr, Int, [Int])]
numberExprList []     = return []
numberExprList [x]    = numberExpr x
numberExprList (x:xs) = do { x'  <- numberExpr x
                           ; xs' <- numberExprList xs
                           ; return $ x' ++ xs'
                           }

runCFG :: [Expr] -> [(Expr, Int, [Int])]
runCFG es = evalState (numberExprList es) 0

cfg f = case f of
  ShaderFunc ty name decls stmt -> runCFG stmt


--assignUniqueID :: Expr -> CFGState NumberedExpr
--assignUniqueID expr = evalState (0, expr) 0
-- mkGraphFromExpr :: Expr -> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)
-- mkGraphFromExpr

