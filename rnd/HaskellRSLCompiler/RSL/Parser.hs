-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.Parser
---- Copyright   :  (c) Syoyo Fujita
---- License     :  Modified BSD
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
import Text.ParserCombinators.Parsec.Error

import Control.Monad.State
import Debug.Trace

import RSL.AST
import RSL.Sema
import RSL.Typer

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
--   TODO: Check duplication of the symbol.
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
program               = do  { ast <- many (spaces >> global)
                            ; return ast
                            }
                       <?>   "program"


global                =   shaderDefinition
                      <|> functionDefinition
                      <|> preprocessor
                      <?>   "top level definition"

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
                            ; return $ concat decls --  [[a]] -> [a]
                            }
                      <?> "formal declarations"

formalDef             = do  { name  <- identifier
                            ; expr  <- maybeInitFormalDeclExpr
                            ; return (name, expr)
                            }

formalDecl            = do  { sc    <- option Nothing maybeRSLStorageClass
                            ; ty    <- rslType
                            ; defs  <- sepBy1 formalDef (symbol ",")
                            ; mapM (updateState . addSymbol) (genSyms ty defs)
                            ; return (genDecls ty defs)
                            }

                            where
                      
                              -- float a, b, c -> [float a, float b, float c]
                              genSyms ty [(name, expr)]   = [(SymVar name ty Uniform KindFormalVariable)]
                              genSyms ty ((name, expr):x) = [(SymVar name ty Uniform KindFormalVariable)] ++ genSyms ty x
                              genDecls ty [(name, expr)]   = [(FormalDecl ty name expr)]
                              genDecls ty ((name, expr):x) = [(FormalDecl ty name expr)] ++ genDecls ty x

statements            =  do { stms <- many statement    -- [[]]
                            ; return $ concat stms      -- []
                            }

statement :: RSLParser [Expr]
statement             =   varDefsStmt
                      -- <|> do { stm <- assignStmt      ; return [stm] }
                      <|> do { stm <- exprStmt        ; return [stm] }
                      <|> do { stm <- whileStmt       ; return [stm] }
                      <|> do { stm <- forStmt         ; return [stm] }
                      <|> do { stm <- illuminanceStmt ; return [stm] }
                      <|> do { stm <- ifStmt          ; return [stm] }
                      <|> do { p <- preprocessor      ; return []    }
                      <?> "statement"


exprStmt = do { e <- expr
              ; symbol ";"
              ; return e
              }

--
-- | Variable definition
--
varDef                = do  { name  <- identifier
                            ; expr  <- maybeInitExpr
                            ; return (name, expr)
                            }

{-
varDefStmt            = do  { ty    <- rslType
                            ; name  <- identifier
                            ; initE <- try maybeInitExpr
                            ; symbol ";"
                            ; updateState (addSymbol (SymVar name ty Uniform KindVariable))
                            ; return (Def ty name initE)
                            }
                      <?> "variable definition"
-}

varDefsStmt           = do  { sc    <- option Nothing maybeRSLStorageClass
                            ; ty    <- rslType
                            ; defs  <- sepBy1 varDef (symbol ",")
                            ; symbol ";"
                            ; mapM (updateState . addSymbol) (genSyms ty defs)
                            ; return (genDefs ty defs)
                            }

                            where

                              -- float a, b, c -> [float a, float b, float c]
                              genSyms ty [(name, expr)]   = [(SymVar name ty Uniform KindVariable)]
                              genSyms ty ((name, expr):x) = [(SymVar name ty Uniform KindVariable)] ++ genSyms ty x
                              genDefs ty [(name, expr)]   = [(Def ty name expr)]
                              genDefs ty ((name, expr):x) = [(Def ty name expr)] ++ genDefs ty x


{-
--
-- Assign statement
--
assignOp              =   (reserved "="  >> return OpAssign)
                      <|> (reserved "+=" >> return OpAddAssign)
                      <|> (reserved "-=" >> return OpSubAssign)
                      <|> (reserved "*=" >> return OpMulAssign)
                      <|> (reserved "/=" >> return OpDivAssign)

assignStmt            = do  { var <- definedSym
                            ; op <- assignOp
                            ; rexpr <- expr
                            ; symbol ";"  <?> "semicolon"
                            ; return (Assign Nothing op (Var Nothing var) rexpr)
                            }
                      <?> "assign stetement"
-}

--
-- while statement
--
whileStmt             = do  { reserved "while"
                            ; symbol "("
                            ; cond <- expr      -- TODO: allow cond expr only.
                            ; symbol ")"
                            ; symbol "{"
                            ; stms <- statements
                            ; symbol "}"
                            ; optional (symbol ";")
                            ; return (While cond stms)
                            }

--
-- for statement
--
forStmt               = do  { reserved "for"
                            ; symbol "("
                            ; init <- expr
                            ; symbol ";"
                            ; cond <- expr      -- TODO: allow cond expr only.
                            ; symbol ";"
                            ; inc  <- expr
                            ; symbol ")"
                            ; symbol "{"
                            ; stms <- statements
                            ; symbol "}"
                            ; optional (symbol ";")
                            ; return (For init cond inc stms)
                            }

-- If not having else clause is TODO
ifStmt                = do  { reserved "if"
                            ; symbol "("
                            ; cond <- expr      -- TODO: allow cond expr only.
                            ; symbol ")"
                            ; thenStmt <- statementBlock
                            ; elseStmt <- option Nothing elseStatement
                            ; optional (symbol ";")
                            ; return (If cond thenStmt elseStmt)
                            }


elseStatement         = do  { reserved "else"
                            ; stmt <- statementBlock
                            ; return (Just stmt)
                            }

statementBlock :: RSLParser [Expr]
statementBlock        = try ( do { s <- statement; return s } )
                      <|>     do { s <- braces (many statement); return $ concat s }
                      <?> "statement"

--
-- Illuminate statement
-- TODO: Parse optional "category" field.
--
-- [13.3]
--
-- illuminance( [string category,] point position )
--     statements
--
-- illuminance( [string category,] point position, vector axis, float angle )
--     statements
--
--
illuminanceStmt       = do  { reserved "illuminance"
                            ; symbol "("
                            ; pos     <- expr -- TODO: allow vector expr only
                            ; symbol ","
                            ; normal  <- expr -- TODO: allow vector expr only
                            ; symbol ","
                            ; angle   <- expr -- TODO: allow float expr only
                            ; symbol ")"
                            ; symbol "{"      -- TODO: brace could be optional.
                            ; stms <- statements
                            ; symbol "}"
                            ; optional (symbol ";")
                            ; return (Illuminance pos normal angle Nothing stms)
                            }
--
-- Expression
--

procedureCall = do  { var <- try definedFunc    -- try is inserted to remove
                                                -- ambiciousness with 'varRef'
                    ; symbol "("
                    ; args <- procArguments
                    ; symbol ")"
                    ; return (Call Nothing var args)
                    }
              <?> "procedure call"

procArguments = sepBy expr (symbol ",") 
              <?> "invalid argument"


triple        = do  { try (symbol "(")        -- try is added to remove
                                              -- ambiciusness with 
                                              -- 'parens expr'
                    ; e0 <- expr
                    ; symbol ","
                    ; e1 <- expr
                    ; symbol ","
                    ; e2 <- expr
                    ; symbol ")"
                    ; return (Triple Nothing [e0, e1, e2])    
                    }
                    

--
-- #line N ...
--
preprocessorLine = do { pos <- getPosition
                      ; string "#line"
                      ; whiteSpace
                      ; n <- natural
                      ; whiteSpace
                      ; s <- manyTill anyChar newline
                      ; -- adjust source pos
                      ; setPosition (setSourceLine pos (fromIntegral n))
                      ; return (Preprocessor s)
                      }

--
-- #pragma ...
--
preprocessorPragma = do { pos <- getPosition
                        ; string "#pragma"
                        ; whiteSpace
                        ; s <- manyTill anyChar newline
                        ; return (Preprocessor s)
                        }


preprocessor      =   try preprocessorLine
                  <|> preprocessorPragma
                  <?> "preprocessor line"

maybeDefinedInScope :: (String, [Symbol]) -> String -> (Maybe Symbol)
maybeDefinedInScope (scope, syms) name = scan syms

  where
    
    scan []     = Nothing
    scan (x:xs) = case (compare symName name) of
                    EQ -> (Just x)
                    _  -> scan xs

                    where
        
                      symName = case x of
                        (SymVar         name _ _ _ ) -> name
                        (SymFunc        name _ _ _ ) -> name
                        (SymBuiltinFunc name _ _ _ ) -> name


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
--defined = lexeme $ try $
--  do  { state <- getState
--      ; name  <- identifier
--      ; case (maybeDefined (symbolTable state) name) of
--          (Just sym) -> return sym
--          Nothing    -> unexpected ("undefined symbol " ++ show name)
--      } 
--  <?> "defined symbol"
definedSym          = do  { state <- getState
                          ; name  <- try identifier
                          ; case (maybeDefined (symbolTable state) name) of
                              (Just sym@(SymVar _ _ _ _ )) -> return sym
                              _    -> unexpected ("undefined symbol " ++ show name)
                          } 
                      <?> "defined symbol"

definedFunc         = do  { state <- getState
                          ; name  <- try identifier
                          ; case (maybeDefined (symbolTable state) name) of
                              (Just sym@(SymBuiltinFunc _ _ _ _ )) -> return sym
                              (Just sym@(SymFunc _ _ _ _ ))        -> return sym
                              _                                    -> unexpected ("undefined symbol " ++ show name)
                          } 
                    <?> "defined symbol"


--
-- | Expecting identifier and its defined previously.
--
varRef      =   do  { var <- try definedSym
                    ; return (Var Nothing var)
                    }
            <?> "defined symbol"


mkInt :: String -> Int
mkInt s   = read s

mkFloat :: String -> Double
mkFloat s = read s

parseSign :: RSLParser Char
parseSign =   do  try (char '-')
          <|> do  optional (char '+')
                  return '+'

fractValue :: RSLParser Double
fractValue              =   do  { char '.'
                                ; fract <- many1 digit
                                ; return $ read ("0." ++ fract)
                                }

toDouble :: Real a => a -> Double
toDouble = fromRational . toRational

floatValue              =   do  { num  <- naturalOrFloat
                                ; return (case num of
                                          Right x -> x
                                          Left  x -> (toDouble x)
                                         )
                                }
                        <|> fractValue

--
-- TODO: Parse more fp value string(e.g. 1.0e+5f)
--

parseFloat :: RSLParser Double
parseFloat = do { sign  <- parseSign
                ; fval  <- floatValue
                ; return $ applySign sign fval 
                }
        
                where

                  mkFloatVal :: String -> String -> Double
                  mkFloatVal whole fract = readDouble $ whole ++ "." ++ fract
                  
                  readDouble = read

                  applySign sign val | sign == '+' = val
                                     | otherwise   = negate val


constString             = do  { s   <- stringLiteral 
                              ; return (Const Nothing (S s))
                              }

-- Number are float value in RSL.
number                  = do  { val <- parseFloat
                              ; return (Const Nothing (F val))
                              }

maybeInitExpr           = do  { symbol "="
                              ; e <- expr
                              ; return (Just e)
                              }
                        <|>   return Nothing
                               

maybeInitFormalDeclExpr = do  { symbol "="
                              ; e <- expr
                              ; return (Just e)
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
                        <|> (reserved "matrix"        >> return TyMatrix    )
                        <?> "RenderMan type"
                      
rslStorageClass         =   (reserved "uniform"       >> return Uniform     )
                        <|> (reserved "varying"       >> return Varying     )
                        <?> "RenderMan storage class"

maybeRSLStorageClass    = do  { sc <- rslStorageClass
                              ; return (Just sc)
                              }

-- typeCastExpr            =   do  { ty  <- rslType
--                                 ; spacety <- option "" stringLiteral
--                                 ; e   <- expr
--                                 ; return (TypeCast Nothing ty spacety e)
--                                 }
                 

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


--
-- Same as done in Error.hs of Parsec, just replace filename to show.
--
showErrorMsg :: ParseError -> FilePath -> String
showErrorMsg err fname =
  show (setSourceName (errorPos err) fname) ++ ":" ++
  showErrorMessages "or" "unknown parse error"
                    "expecting" "unexpected" "end of input"
                   (errorMessages err)

mkMyError :: ParseError -> FilePath -> ParseError
mkMyError err fname = setErrorPos (setSourceName (errorPos err) fname) err


run :: RSLParser [Func] -> FilePath -> FilePath -> ([Func] -> IO ()) -> IO ()
run p prepname name proc =
  do  { result <- parseRSLFromFile p prepname
      ; case (result) of
          Left err -> do  { -- Parse preprocessed file, but print original file
                            -- when reporting error.
                            putStrLn "Parse err:"
                          ; showLine name (sourceLine (errorPos err)) (sourceColumn (errorPos err))
                          --; print (showErrorMsg err name)
                          ; print (mkMyError err name)
                          }
          Right x  -> do  { proc $ typingAST x
                          }
      }


runLex :: RSLParser [Func] -> FilePath -> FilePath -> ([Func] -> IO ()) -> IO ()
runLex p prepname name proc =
  run (do { whiteSpace
          ; x <- p
          ; eof
          ; return x
          }
      ) prepname name proc

--
-- Useful parsing tools
--
lexer           = P.makeTokenParser rslStyle

whiteSpace      = P.whiteSpace lexer
lexeme          = P.lexeme lexer
symbol          = P.symbol lexer
natural         = P.natural lexer
naturalOrFloat  = P.naturalOrFloat lexer
stringLiteral   = P.stringLiteral lexer
float           = P.float lexer
parens          = P.parens lexer
braces          = P.braces lexer
semi            = P.semi lexer
commaSep        = P.commaSep lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer
reservedOp      = P.reservedOp lexer

expr        ::  RSLParser Expr
expr        =   buildExpressionParser table primary
           <?> "expression"

primary     =   try (parens expr)
            <|> triple
            <|> procedureCall   -- Do I really need "try"?
            <|> varRef
            -- <|> procedureCall   -- Do I really need "try"?
            <|> number
            <|> constString
            <?> "primary"

table       =  [
               -- typecast
                  [typecast]
               -- unary
               ,  [prefix "-" OpSub, prefix "!" OpNeg]

               -- binop
               ,  [binOp "."  OpDot AssocLeft]
               ,  [binOp "*"  OpMul AssocLeft, binOp "/"  OpDiv AssocLeft]
               ,  [binOp "+"  OpAdd AssocLeft, binOp "-"  OpSub AssocLeft]

               -- relop
               ,  [binOp ">"  OpGt  AssocLeft, binOp ">=" OpGe  AssocLeft]
               ,  [binOp "<"  OpLt  AssocLeft, binOp "<=" OpLe  AssocLeft]
               ,  [binOp "==" OpEq  AssocLeft, binOp "!=" OpNeq AssocLeft]

               -- logop
               ,  [binOp "&&" OpAnd AssocLeft, binOp "||" OpOr  AssocLeft]

               -- a ? b : c
               ,  [conditional]

               -- assign
               ,  [ assignOp "="  OpAssign    AssocRight
                  , assignOp "+=" OpAddAssign AssocRight
                  , assignOp "-=" OpSubAssign AssocRight
                  , assignOp "*=" OpMulAssign AssocRight
                  , assignOp "/=" OpDivAssign AssocRight
                  ]
  
               ]

              where

                typecast
                  = Prefix ( do { ty  <- rslType
                                ; spacety <- option "" stringLiteral
                                ; return (\e -> TypeCast Nothing ty spacety e)
                                } <?> "typecast" )

                prefix name f
                  = Prefix ( do { reservedOp name
                                ; return (\x -> UnaryOp Nothing f x)
                                } )

                binOp name f assoc
                  = Infix  ( do { reservedOp name
                                ; return (\x y -> BinOp Nothing f x y)
                                } ) assoc

                conditional
                  = Infix  ( do { reservedOp "?"
                                ; thenExpr <- expr
                                ; reservedOp ":"
                                ; return (\condExpr elseExpr -> Conditional Nothing condExpr thenExpr elseExpr)
                                } <?> "conditional" ) AssocRight

                assignOp name f assoc
                  = Infix  ( do { state <- getState
                                ; reservedOp name
                                ; return (\x y -> mkAssign f x y)
                                } <?> "assign" ) assoc

                -- Make Assign node with flattening expression.
                mkAssign :: Op -> Expr -> Expr -> Expr
                mkAssign op x y = case op of
                  OpAssign    -> Assign Nothing OpAssign x y
                  OpAddAssign -> Assign Nothing OpAssign x (BinOp Nothing OpAdd x y)
                  OpSubAssign -> Assign Nothing OpAssign x (BinOp Nothing OpSub x y)
                  OpMulAssign -> Assign Nothing OpAssign x (BinOp Nothing OpMul x y)
                  OpDivAssign -> Assign Nothing OpAssign x (BinOp Nothing OpDiv x y)

rslStyle = javaStyle
  { reservedNames = [ "const"
                    , "break", "continue"
                    , "while", "if", "for", "solar", "illuminate", "illuminance"
                    , "surface", "volume", "displacement", "imager"
                    , "varying", "uniform", "facevarygin", "facevertex"
                    , "output"
                    , "extern"
                    , "color", "vector", "normal", "matrix", "point", "void"
                    -- More is TODO
                    ]
  , reservedOpNames = ["+", "-", "*", "/"] -- More is TODO
  , caseSensitive   = True
  , commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  }

