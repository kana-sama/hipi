{-# LANGUAGE ViewPatterns #-}

module Language.Hipi.Core.Pretty where

import Language.Hipi.Core.AST

isNat :: Term -> Maybe Int
isNat (Var "zero") = Just 0
isNat (Var "succ" :@ next) = fmap (+ 1) (isNat next)
isNat _ = Nothing

instance Show Term where
  show (isNat -> Just n) = show n
  show U = "U"
  show (Π ("_", α@(Π _ _)) β) = "(" ++ show α ++ ")" ++ " → " ++ show β
  show (Π ("_", α) β) = show α ++ " → " ++ show β
  show (Π (x, α) β) = "(" ++ x ++ " : " ++ show α ++ ") → " ++ show β
  show (Var x) = x
  show (x :⇒ t) = "λ" ++ x ++ ". " ++ show t
  show (t₁ :@ t₂) = parens parens₁ "" (show t₁) " " ++ parens parens₂ "" (show t₂) ""
    where
      parens₁ = case t₁ of _ :⇒ _ -> True; _ -> False
      parens₂ = case t₂ of Var _ -> False; U -> False; _ -> True
      parens True _ x _ = "(" ++ x ++ ")"
      parens False b x a = b ++ x ++ a
