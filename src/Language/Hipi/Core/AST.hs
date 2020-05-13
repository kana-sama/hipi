module Language.Hipi.Core.AST where

type Name = String

infixl 5 :@

infixr 0 :⇒

data Term
  = U
  | Π (Name, Term) Term
  | Var Name
  | Name :⇒ Term
  | Term :@ Term
  deriving (Eq)
