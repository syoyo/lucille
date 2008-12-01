-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.Parser
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

module RSL.Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Control.Monad.State

import RSL.AST

--
-- Topmost rule
--
program               =     many1 definition

definition            =    try  shaderDefinition
                      <|>       functionDefinition

shaderDefinition      = do  { ty    <- shaderType
                            ; name  <- identifier
                            ; symbol "("
                            ; formals <- formalDecls
                            ; symbol ")"
                            ; symbol "{"
                            ; symbol "}"
                            ; return (ShaderFunc ty name formals)
                            } 
  
functionDefinition    = do  { ty    <- option (TyVoid) rslType
                            ; name  <- identifier
                            ; symbol "("
                            ; symbol ")"
                            ; symbol "{"
                            ; symbol "}"
                            ; return (UserFunc ty name)
                            }

--
-- TODO: support multiple var def, e.g., float ka = 1, kb = 1;
-- 
formalDecls           = do  { decls <- sepEndBy formalDecl (symbol ";")
                            ; if length decls > 0 then return (Just decls)
                                                  else return Nothing
                            }
                      <?> "formal decl"

formalDecl            = do  { ty  <- rslType
                            ; name <- identifier
                            ; expr <- initFormalDeclExpr
                            ; return (FormalDecl ty name expr)
                            }

mkInt :: String -> Int
mkInt s = read s

mkFloat :: String -> Double
mkFloat s = read s

parseSign :: Parser Char
parseSign =   do  try (char '-')
          <|> do  optional (char '+')
                  return '+'

--
-- TODO: Support more fp value string(e.g. 1.0e+5f)
--
parseFloat :: Parser Double
parseFloat = do { sign  <- parseSign
                ; whole <- many1 digit 
                ; char '.'
                ; fract <- many1 digit
                ; return $ mkFloatVal whole fract
                }
        
                where

                  mkFloatVal :: String -> String -> Double
                  mkFloatVal whole fract = readDouble $ whole ++ "." ++ fract

                  readDouble = read

initFormalDeclExpr    = do  { symbol "="
                            ; val <- parseFloat
                            ; return (Just (F val)) -- FIXME
                            }
                      <|>   return Nothing
                               

shaderType            =   (symbol "light"         >> return Light       )
                      <|> (symbol "surface"       >> return Surface     )
                      <|> (symbol "volume"        >> return Volume      )
                      <|> (symbol "displacement"  >> return Displacement)
                      <|> (symbol "imager"        >> return Imager      )

rslType               =   (symbol "float"         >> return TyFloat     )
                      <|> (symbol "string"        >> return TyString    )
                      <|> (symbol "color"         >> return TyColor     )
                      <|> (symbol "point"         >> return TyPoint     )
                      <|> (symbol "vector"        >> return TyVector    )
                      



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
run :: Parser [Func] -> FilePath -> ([Func] -> IO ()) -> IO ()
run p name proc =
  do  { result <- parseFromFile p name
      ; case (result) of
          Left err -> do  { putStrLn "Parse err:"
                          ; showLine name (sourceLine (errorPos err)) (sourceColumn (errorPos err))
                          ; print err
                          }
          Right x  -> do  { proc x
                          }
      }


runLex :: Parser [Func] -> FilePath -> ([Func] -> IO ()) -> IO ()
runLex p name proc =
  run (do whiteSpace
          x <- p
          return x
      ) name proc

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

