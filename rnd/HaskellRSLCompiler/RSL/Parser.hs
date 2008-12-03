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
                      <?>   "program"

definition            =   shaderDefinition
                      <|> functionDefinition
                      <?>   "definition"

shaderDefinition      = do  { ty    <- shaderType
                            ; name  <- identifier
                            ; symbol "("
                            ; formals <- formalDecls
                            ; symbol ")"
                            ; symbol "{"
                            ; stms <- statements
                            ; symbol "}"
                            ; return (ShaderFunc ty name formals stms)
                            } 
                      <?> "shader definition"
  
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
                            ; return decls
                            }
                      <?> "formal declarations"

formalDecl            = do  { ty    <- rslType
                            ; name  <- identifier
                            ; expr  <- maybeInitFormalDeclExpr
                            ; return (FormalDecl ty name expr)
                            }

statements :: Parser [Expr]
statements =  many statement

statement =   varDefStmt
          <|> assignStmt
          <|> exprStmt
          <?> "statement"


exprStmt = do { e <- expr
              ; symbol ";"
              ; return e
              }

--
-- | Variable definition
--
varDefStmt  = do  { ty    <- rslType
                  ; name  <- identifier
                  ; initE <- try maybeInitExpr
                  ; symbol ";"
                  ; return (Def ty name initE)
                  }
            <?> "variable definition"

assignStmt  = do  { lvalue <- identifier
                  ; symbol "="
                  ; rexpr <- expr
                  ; symbol ";"  <?> "semicolon"
                  ; return (Assign TyUndef (Var TyUndef lvalue) rexpr)
                  }
            <?> "assign stetement"

procedureCall = do  { name <- identifier
                    ; symbol "("
                    ; args <- try procArguments
                    ; symbol ")"
                    ; return (Call TyUndef name args)
                    }

procArguments = sepBy expr (symbol ",") 
                    
-- expr        = choice [
--                   try procedureCall
--                 , varRef 
--                 ]


varRef      = do  { name <- identifier
                  ; return (Var TyUndef name)
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


maybeInitExpr           = do  { symbol "="
                              ; e <- expr
                              ; return (Just e)
                              }
                        <|>   return Nothing
                               

maybeInitFormalDeclExpr = do  { symbol "="
                              ; val <- parseFloat
                              ; return (Just (F val)) -- FIXME
                              }
                        <|>   return Nothing
                               

shaderType            =   (reserved "light"         >> return Light       )
                      <|> (reserved "surface"       >> return Surface     )
                      <|> (reserved "volume"        >> return Volume      )
                      <|> (reserved "displacement"  >> return Displacement)
                      <|> (reserved "imager"        >> return Imager      )
                      <?> "RenderMan shader type"

rslType               =   (reserved "float"         >> return TyFloat     )
                      <|> (reserved "string"        >> return TyString    )
                      <|> (reserved "color"         >> return TyColor     )
                      <|> (reserved "point"         >> return TyPoint     )
                      <|> (reserved "vector"        >> return TyVector    )
                      <|> (reserved "normal"        >> return TyNormal    )
                      <?> "RenderMan type"
                      



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
  run (do { whiteSpace
          ; x <- p
          ; eof
          ; return x
          }
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

expr        ::  Parser Expr
expr        =   buildExpressionParser table primary
           <?> "expression"
-- expr = primary

primary     =   try procedureCall
            <|> parens expr
            <|> varRef

table       =  [  [binOp "*" OpMul AssocLeft, binOp "/" OpDiv AssocLeft]
               ,  [binOp "+" OpAdd AssocLeft, binOp "-" OpSub AssocLeft]
               ]

              where

                binOp s f assoc
                  = Infix ( do { reservedOp s
                               ; return (\x y -> BinOp TyUndef f [x, y])
                               } ) assoc

rslDef = javaStyle
  { reservedNames = [ "const"
                    , "break", "for", "if", "else"
                    , "surface", "volume", "displacement", "imager"
                    ]
  , reservedOpNames = ["+", "-", "*", "/"]
  }

