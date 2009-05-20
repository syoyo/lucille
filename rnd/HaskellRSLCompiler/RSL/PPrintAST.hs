-------------------------------------------------------------------------------
---- |
---- Module      :  RSL.PPrintAST
---- Copyright   :  (c) Syoyo Fujita
---- License     :  Modified BSD
----
---- Maintainer  :  syoyo@lucillerender.org
---- Stability   :  experimental
---- Portability :  GHC 6.8
----
---- PPrintAST   :  Pretty printing AST form.
----
-------------------------------------------------------------------------------

module RSL.PPrintAST where

import Text.PrettyPrint.Leijen      -- wl-pprint

import RSL.AST

pprintAST :: [Func] -> String
pprintAST fs = show $ list $ map func fs

typ :: Type -> Doc
typ ty = text $ show ty

func :: Func -> Doc 
func f = case f of

  ShaderFunc ty name decls exprs
    -> parens (nest 2 (text "ShaderFunc" <+> dquotes (text name)
                       <$> (vcat (map formalDecl decls))
                       <$> (vcat (map expr exprs))))

  UserFunc   ty name             
    -> parens (text "UserFunc" <+> dquotes (text name))

  Preprocessor ss                
    -> empty


formalDecl :: FormalDecl -> Doc
formalDecl decl = text $ show decl
-- (FormalDecl ty specs name exprs) = typ ty <+> dquotes (text name)

expr :: Expr -> Doc
expr e = text $ show e

