module Main where

import Directory
import System
import System.Cmd
import System.FilePath
import System.Console.GetOpt

import Data.Maybe ( fromMaybe )

import RSL.Parser
import RSL.PPrint
import RSL.Typer
import RSL.CFG
import RSL.CodeGenLLVM

--
-- Configurations
--
preprocessorCmd = "mcpp" -- A portable C preprocessor by Kiyoshi Matsui
version         = "0.1"

--
-- Compile flags
--
data RSLFlag
  = Version
  | Output String
  | Input  String
  | Debug
  | Preprocessor
  deriving (Show, Eq) 

options :: [OptDescr RSLFlag]
options =
  [ Option ['o'] ["output"] (OptArg outp "FILE")  "output FILE"
  , Option ['v'] ["version"] (NoArg Version)      "Show version"
  ]

header = "RenderMan Shader Language compiler for lucille, version " ++ version 
usage  = concat
  [ header
  , "\n\n"
  , "Usage: lslc [OPTION...] file.sl"
  ]

outp :: Maybe String -> RSLFlag
outp = Output . fromMaybe "stdout"

parseOption :: [String] -> IO ([RSLFlag], [String])
parseOption argv =
  case getOpt Permute options argv of
    (o, n, []  ) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo usage options))


debugPrinter ast = do putStrLn $ "// [AST] = " ++ show ast ++ "\n"
                      putStrLn $ pprint 0 ast   -- 0 = initial indent level
                        
                      putStrLn $ "========= LLVM IR ===========" 

                      let globalVariablesString = genGlobal ast
                      let staticCodeString  = genStatic 0 ast
                      let dynamicCodeString = genDynamic 0 ast
                      let codeString        = gen 0 ast
                      -- let cfgArr            = cfg $ (ast !! 0)
                      putStrLn codeString
                      -- putStrLn $ show cfgArr
                      -- Also write to file.
                      let headerString = genHeader  -- from CodeGenLLVM
                      writeFile "output.ll" (headerString ++ "\n" ++ globalVariablesString ++ codeString ++ "\n" ++ staticCodeString ++ "\n" ++ dynamicCodeString)


--
-- Execute C preprocessor as an external command.
--
runPreprocessor :: FilePath -> IO FilePath
runPreprocessor filename =
  do  let preprocessedFilename = replaceExtension filename ".i"

      exitCode <- rawSystem preprocessorCmd [filename, "-o", preprocessedFilename]

      case exitCode of 
      
        ExitSuccess   -> return preprocessedFilename
        ExitFailure _ -> error "Failed to execute preprocessor."

        

main = do (flags, args) <- getArgs >>= parseOption
          if length args > 0

            then

              do { preprocessedFilename <- runPreprocessor (args !! 0)
                 ; runLex program preprocessedFilename (args !! 0) debugPrinter
                   -- clean up
                 ; removeFile preprocessedFilename
                 }

            else error (usageInfo usage options)

          --putStrLn $ show s

          --  where (s, _) = codeGenLLVM
