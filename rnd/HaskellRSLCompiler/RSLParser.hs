-------------------------------------------------------------------------------
---- |
---- Module      :  RSLParser
---- Copyright   :  (c) Syoyo Fujita
---- License     :  BSD-style
----
---- Maintainer  :  syoyo@lucillerender.org
---- Stability   :  experimental
---- Portability :  GHC 6.8
----
---- RSLParser   :  A parser for RenderMan SL.
----
-------------------------------------------------------------------------------

module RSLParser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Control.Monad.State

import RSLAST

--
-- Topmost rule
--
program               =     many1 definition

definition            =    try  shader_definition
                      <|>       function_definition

shader_definition     = do  ty    <- shader_type
                            name  <- identifier
                            symbol "("
                            symbol ")"
                            symbol "{"
                            symbol "}"
                            return ("SDR" ++ name)
  
function_definition   = do  ty    <- option ("void") rsltype
                            name  <- identifier
                            symbol "("
                            symbol ")"
                            symbol "{"
                            symbol "}"
                            return (ty ++ name)


shader_type           =   symbol "light"
                      <|> symbol "surface"
                      <|> symbol "volume"
                      <|> symbol "displacement"
                      <|> symbol "imager"

rsltype               =   symbol "float"
                      <|> symbol "string"
                      <|> symbol "color"
                      <|> symbol "point"
                      <|> symbol "vector"
                      


--
-- Parse error reporting routines
--
offt :: Int -> String
offt n = replicate n ' '

showLine :: SourceName -> Int -> Int -> IO ()
showLine name n m =
  do  input <- readFile name

      if (length (lines input)) < n

        then

          if length (lines input) == 0

            then putStrLn ""

          else

            do  { putStrLn $ (lines input) !! ((length (lines input)) - 1)
                ; putStrLn ""
                ; putStrLn $ ((offt (m-1)) ++ "^")
                }

        else

          do  { let l = (lines input) !! (n-1)
              ; putStrLn l
              ; putStrLn $ ((offt (m-1)) ++ "^")
              }


--
-- Parser interface
--
run :: Parser [String] -> FilePath -> IO ()
run p name = do { result <- parseFromFile p name
                ; case (result) of
                    Left err -> do  { putStrLn "Parse err:"
                                    ; showLine name (sourceLine (errorPos err))
(sourceColumn (errorPos err))
                                    ; print err
                                    }
                    Right x  -> do  { print x
                                    }
                }


runLex :: Parser [String] -> FilePath -> IO ()
runLex p name =
  run (do whiteSpace
          x <- p
          return x
      ) name

lexer       = P.makeTokenParser rslDef

whiteSpace  = P.whiteSpace lexer
lexeme      = P.lexeme lexer
symbol      = P.symbol lexer
natural     = P.natural lexer
parens      = P.parens lexer
semi        = P.semi lexer
commaSep    = P.commaSep lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer

rslDef = javaStyle
  { reservedNames = [ "const"
                    , "break", "for", "if", "else"
                    ]
  , reservedOpNames = ["+", "-", "*", "/"]
  }

