module Main where

import System
import System.FilePath

import RSL.Parser
import RSL.PPrint
import RSL.Typer
import RSL.CodeGenLLVM

debugPrinter ast = do putStrLn $ "// [AST] = " ++ show ast ++ "\n"
                      putStrLn $ pprint 0 ast   -- 0 = initial indent level
                        
                      putStrLn $ "========= LLVM IR ===========" 

                      let globalVariablesString = genGlobal ast
                      let codeString = gen 0 ast
                      putStrLn codeString
                      -- Also write to file.
                      let headerString = genHeader  -- from CodeGenLLVM
                      writeFile "output.ll" (headerString ++ "\n" ++ globalVariablesString ++ codeString)


main = do args <- getArgs
          if length args > 0 then runLex program (args !! 0) debugPrinter
                             else error "Needs input file"

          --putStrLn $ show s

          --  where (s, _) = codeGenLLVM
