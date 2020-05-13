{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Hipi.Compile where

import Control.Lens
import Data.Generics.Labels
import Data.Generics.Uniplate.Data
import Data.List
import Language.Hipi.AST
import qualified Language.Hipi.Core.AST as Core

dataToValue :: Module -> Module
dataToValue module' = module' & #declarations %~ concatMap \case
  DataDeclaration {name, constructors} ->
    let n = (length constructors)
        typeDeclaration =
          Definition
            { lhs = Typed name U,
              rhs = Pi ("r", U) (foldr Arr (Var "r") (replicate n (Var "r")))
            }
        constructorDeclarations = zipWith (makeConstructor name n) constructors [0 ..]
     in typeDeclaration : constructorDeclarations
  x -> [x]
  where
    makeConstructor typeName n ConstructorDeclaration {..} i =
      Definition
        { lhs = Typed name (Var typeName),
          rhs = Lam (replicate i Wildcard ++ [Untyped "x"] ++ replicate (n - i - 1) Wildcard) (Var "x")
        }

caseToElim :: Module -> Module
caseToElim = transformBi \case
  Case value cases -> foldl App value (fmap snd cases)
  x -> x

defsToLet :: Module -> Module
defsToLet Module {..} =
  Module
    [ Definition
        Wildcard
        ( foldr
            (\Definition {..} next -> Let lhs rhs next)
            (rhs . last $ declarations)
            (init declarations)
        )
    ]

letsToLambdas :: Module -> Module
letsToLambdas = transformBi \case
  Let binder value next -> App (Lam [binder] next) value
  x -> x

toCore :: Module -> Core.Term
toCore (Module [Definition Wildcard value]) = go value
  where
    go U = Core.U
    go (Pi (var, ty) cod) = Core.Π (var, go ty) (go cod)
    go (Arr dom cod) = Core.Π ("_", go dom) (go cod)
    go (Lam binders next) = foldr f (go next) binders
      where
        f Wildcard next = "_" Core.:⇒ next
        f (Typed x _) next = x Core.:⇒ next
        f (Untyped x) next = x Core.:⇒ next
    go (App x y) = (go x) Core.:@ (go y)
    go (Var x) = Core.Var x
    go Let {} = error "impossible"
    go Case {} = error "impossible"
