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

import RSL.AST

indent :: Int -> String
indent n = concat $ replicate (4 * n) " "

data LLVMState = LLVMState  { globals :: [Symbol]
                            , n       :: Int
                            }

initLLVMState = LLVMState   { globals = []
                            , n       = 0
                            }

class AST a where

  gen :: Int -> a -> String
  genList :: Int -> [a] -> String
  genList n = concat . map (gen n)


instance AST a => AST [a] where

  gen = genList


body = concat
  [ "define void @muda() {"
  , "}"
  ] 

muda :: State LLVMState String
muda = do { x <- get
          ; return "bora"
          }

codeGenLLVM :: (String, LLVMState)
codeGenLLVM = runState muda initLLVMState
