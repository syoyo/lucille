module Main where

import System

import RSL.Parser
import RSL.PPrint

debugPrinter = putStrLn . show

main = do args <- getArgs
          if length args > 0 then runLex program (args !! 0) debugPrinter
                             else error "Needs input file"
