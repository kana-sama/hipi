{-# LANGUAGE UnicodeSyntax #-}

module Language.Hipi.Core.Check where

import Control.Monad
import Language.Hipi.Core.AST

type Context = [(Name, Term)]

normal :: Term -> Term
normal U = U
normal (Π (x, α) β) = (Π (x, normal α) (normal β))
normal (Var x) = Var x
normal (x :⇒ t) = x :⇒ normal t
normal (Var "the" :@ α :@ τ) = normal τ
normal (t₁ :@ t₂) =
  case (normal t₁, normal t₂) of
    (x :⇒ t, t₂') -> substitute x t₂' t
    (t₁', t₂') -> t₁' :@ t₂'

substitute :: Name -> Term -> Term -> Term
substitute x t₁ U = U
substitute x t₁ (Π (y, α) β) | x == y = Π (y, α) β
substitute x t₁ (Π (y, α) β) = Π (y, substitute x t₁ α) (substitute x t₁ β)
substitute x t₁ (Var y) | x == y = t₁
substitute x t₁ (Var y) = Var y
substitute x t₁ (y :⇒ t₂) | x == y = y :⇒ t₂
substitute x t₁ (y :⇒ t₂) = y :⇒ substitute x t₁ t₂
substitute x t₁ (t₂ :@ t₃) = normal (substitute x t₁ t₂ :@ substitute x t₁ t₃)

infer :: Context -> Term -> Maybe Term
infer г U = pure U
infer г (Var x) = lookup x г
infer г (t₁ :@ t₂) = do
  Π (x, α) β <- infer г t₁
  check г t₂ α
  pure (substitute x t₂ β)
infer _ _ = Nothing

check :: Context -> Term -> Term -> Maybe ()
check г (x :⇒ t) γ@(Π (x', α) β) = do
  check г γ U
  check ((x, α) : г) t β
check г (Π (x, α) β) U = do
  check г α U
  check ((x, α) : г) β U
check г t τ = do
  τ' <- infer г t
  guard (τ == τ')
