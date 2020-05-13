{-# LANGUAGE RecordWildCards #-}

module Language.Hipi.Pretty where

import Data.List
import Language.Hipi.AST

ident :: Int -> String
ident n = replicate n ' '

class PrettyPrint a where
  pretty :: Int -> a -> String

instance PrettyPrint Module where
  pretty _ Module {..} = intercalate "\n" (fmap (pretty 0) declarations)

instance PrettyPrint Declaration where
  pretty _ Definition {lhs = Wildcard, ..} = pretty 0 rhs
  pretty _ Definition {..} = "def " ++ pretty 0 lhs ++ " =\n  " ++ pretty 2 rhs ++ "\n"
  pretty _ DataDeclaration {..} = "data " ++ name ++ "\n" ++ (constructors >>= \c -> "  | " <> pretty 2 c <> "\n")

instance PrettyPrint ConstructorDeclaration where
  pretty _ ConstructorDeclaration {..} = name

instance PrettyPrint Expr where
  pretty _ U = "U"
  pretty n (Pi (var, ty) expr) = "∏ " ++ var ++ " : " ++ pretty n ty ++ ", " ++ pretty n expr
  pretty n (Arr dom cod) = "(" ++ pretty n dom ++ ")" ++ " -> (" ++ pretty n cod ++ ")"
  pretty n (Lam binders body) = "(λ " ++ unwords (fmap (pretty n) binders) ++ ",\n" ++ ident (n + 2) ++ pretty (n + 2) body ++ ")"
  pretty n (App left right) = "(" ++ pretty n left ++ ")(" ++ pretty n right ++ ")"
  pretty _ (Var name) = name
  pretty n (Let binder value next) =
    ("let " ++ pretty n binder ++ " = " ++ pretty n value ++ " in\n")
      ++ ident (n + 2)
      ++ pretty (n + 2) next
  pretty n (Case value cases) =
    "case " ++ pretty n value ++ " of\n"
      ++ intercalate
        "\n"
        ( fmap
            ( \(ConstructorPattern {..}, expr) ->
                ident (n + 2) ++ name ++ " -> " ++ pretty (n + 2) expr
            )
            cases
        )

instance PrettyPrint Binder where
  pretty _ Wildcard = "_"
  pretty _ (Untyped x) = x
  pretty n (Typed x ty) = "(" ++ x ++ " : " ++ pretty n ty ++ ")"

instance PrettyPrint ConstructorPattern where
  pretty _ ConstructorPattern {..} = name
