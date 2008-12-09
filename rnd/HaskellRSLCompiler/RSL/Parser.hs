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

-- |RenderMan Shading Languager parser.
module RSL.Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Control.Monad.State
import Debug.Trace

import RSL.AST
import RSL.Sema

-- | RSL parser state
data RSLState = RSLState  { symbolTable :: SymbolTable }


-- | RSL parser having RSL parser state
type RSLParser a = GenParser Char RSLState a 


-- | Initial state of shader env.
--   Builtin variables are added in global scope.
initRSLState :: RSLState
initRSLState = RSLState {
  symbolTable = [("global", builtinShaderVariables ++ builtinShaderFunctions)] }
  

-- | Push scope into the symbol table
pushScope :: String -> [Symbol] -> RSLState -> RSLState
pushScope scope xs st =
  st { symbolTable = newTable }

    where
    
      -- Add new scope to the first elem of the list.
      newTable = [(scope, xs)] ++ (symbolTable st)

-- | Pop scope from the symbol table
popScope :: RSLState -> RSLState
popScope st =
  st { symbolTable = newTable }

    where

      -- Pop first scope from the scope chain
      newTable = tail (symbolTable st)

-- | Add the symbol to the first scope in the symbol list.
addSymbol :: Symbol -> RSLState -> RSLState
addSymbol sym st = trace ("// Add " ++ (show sym)) $ 
  st { symbolTable = newTable }

    where

      newTable = case (symbolTable st) of
        [(scope, xs)]     -> [(scope, [sym] ++ xs)]
        ((scope, xs):xxs) -> [(scope, [sym] ++ xs)] ++ xxs

--
-- Topmost parsing rule
--
program               = do  { ast <- many1 definition
                            ; return ast
                            }
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

                            -- push scope
                            ; updateState (pushScope name [])

                            ; stms <- statements
                            ; symbol "}"

                            -- pop scope
                            ; updateState (popScope)

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
                            ; updateState (addSymbol (SymVar name ty Uniform KindVariable))
                            ; return (FormalDecl ty name expr)
                            }

statements            =  many statement

statement             =   varDefStmt
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
                  ; updateState (addSymbol (SymVar name ty Uniform KindVariable))
                  ; return (Def ty name initE)
                  }
            <?> "variable definition"

assignStmt  = do  { var <- defined
                  ; symbol "="
                  ; rexpr <- expr
                  ; symbol ";"  <?> "semicolon"
                  ; return (Assign (Var var) rexpr)
                  }
            <?> "assign stetement"

procedureCall = do  { var <- defined
                    ; symbol "("
                    ; args <- try procArguments
                    ; symbol ")"
                    ; return (Call var args)
                    }
              <?> "procedure call"

procArguments = sepBy expr (symbol ",") 
                    

maybeDefinedInScope :: (String, [Symbol]) -> String -> (Maybe Symbol)
maybeDefinedInScope (scope, syms) name = scan syms

  where
    
    scan []     = Nothing
    scan (x:xs) = case (compare symName name) of
                    EQ -> (Just x)
                    _  -> scan xs

                    where
        
                      symName = case x of
                        (SymVar  name _ _ _ ) -> name
                        (SymFunc name _ _ _ ) -> name


maybeDefinedInScopeChain :: SymbolTable -> String -> (Maybe Symbol)
maybeDefinedInScopeChain []     name = Nothing
maybeDefinedInScopeChain [x]    name = maybeDefinedInScope x name
maybeDefinedInScopeChain (x:xs) name = maybeDefinedInScope x name `mplus` maybeDefinedInScopeChain xs name


maybeDefined :: SymbolTable -> String -> (Maybe Symbol)
maybeDefined table name = maybeDefinedInScopeChain table name


--
-- | Check if the identifier trying to parse is defined previously.
--   If the identifier isn't defined in the scope chain, exit with fail.
--
defined = lexeme $ try $
  do  { state <- getState
      ; name  <- identifier
      ; case (maybeDefined (symbolTable state) name) of
          (Just sym) -> return sym
          Nothing    -> unexpected ("undefined symbol " ++ show name)
      } 


--
-- | Expecting identifier and its defined previously.
--
varRef      = do  { var <- defined
                  ; return (Var var)
                  }


mkInt :: String -> Int
mkInt s = read s

mkFloat :: String -> Double
mkFloat s = read s

parseSign :: RSLParser Char
parseSign =   do  try (char '-')
          <|> do  optional (char '+')
                  return '+'

--
-- TODO: Support parising more fp value string(e.g. 1.0e+5f)
--
parseFloat :: RSLParser Double
parseFloat = do { sign  <- parseSign
                ; whole <- many1 digit 
                ; char '.'
                ; fract <- many1 digit
                ; return $ applySign sign $ mkFloatVal whole fract
                }
        
                where

                  mkFloatVal :: String -> String -> Double
                  mkFloatVal whole fract = readDouble $ whole ++ "." ++ fract
                  
                  readDouble = read

                  applySign sign val | sign == '+' = val
                                     | otherwise   = negate val


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
                               

shaderType              =   (reserved "light"         >> return Light       )
                        <|> (reserved "surface"       >> return Surface     )
                        <|> (reserved "volume"        >> return Volume      )
                        <|> (reserved "displacement"  >> return Displacement)
                        <|> (reserved "imager"        >> return Imager      )
                        <?> "RenderMan shader type"

rslType                 =   (reserved "float"         >> return TyFloat     )
                        <|> (reserved "string"        >> return TyString    )
                        <|> (reserved "color"         >> return TyColor     )
                        <|> (reserved "point"         >> return TyPoint     )
                        <|> (reserved "vector"        >> return TyVector    )
                        <|> (reserved "normal"        >> return TyNormal    )
                        <?> "RenderMan type"
                      

--
--
--
initShaderEnv :: SymbolTable
initShaderEnv = [("global", [])]


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
parseRSLFromFile :: RSLParser a -> SourceName -> IO (Either ParseError a)
parseRSLFromFile p fname =
  do { input <- readFile fname
     ; return (runParser p initRSLState fname input)
     }


run :: RSLParser [Func] -> FilePath -> ([Func] -> IO ()) -> IO ()
run p name proc =
  do  { result <- parseRSLFromFile p name
      ; case (result) of
          Left err -> do  { putStrLn "Parse err:"
                          ; showLine name (sourceLine (errorPos err)) (sourceColumn (errorPos err))
                          ; print err
                          }
          Right x  -> do  { proc x
                          }
      }


runLex :: RSLParser [Func] -> FilePath -> ([Func] -> IO ()) -> IO ()
runLex p name proc =
  run (do { x <- p
          ; eof
          ; return x
          }
      ) name proc

--
-- Useful parsing tools
--
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

expr        ::  RSLParser Expr
expr        =   buildExpressionParser table primary
           <?> "expression"

primary     =   try procedureCall   -- Do I really need "try"?
            <|> parens expr
            <|> varRef

table       =  [  [binOp "*" OpMul AssocLeft, binOp "/" OpDiv AssocLeft]
               ,  [binOp "+" OpAdd AssocLeft, binOp "-" OpSub AssocLeft]
               ]

              where

                binOp s f assoc
                  = Infix ( do { reservedOp s
                               ; return (\x y -> BinOp f [x, y])
                               } ) assoc

rslDef = javaStyle
  { reservedNames = [ "const"
                    , "break", "for", "if", "else"
                    , "surface", "volume", "displacement", "imager"
                    -- More is TODO
                    ]
  , reservedOpNames = ["+", "-", "*", "/"] -- More is TODO
  }

