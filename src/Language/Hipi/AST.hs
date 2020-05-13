{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Hipi.AST where

import Data.Data
import GHC.Generics

type Ident = String

data Module = Module {declarations :: [Declaration]}
  deriving (Generic, Show, Data, Typeable)

data Declaration
  = Definition {lhs :: Binder, rhs :: Expr}
  | DataDeclaration {name :: Ident, constructors :: [ConstructorDeclaration]}
  deriving (Generic, Show, Data, Typeable)

data ConstructorDeclaration = ConstructorDeclaration {name :: String}
  deriving (Generic, Show, Data, Typeable)

data Expr
  = U
  | Pi (Ident, Expr) Expr
  | Arr Expr Expr
  | Lam [Binder] Expr
  | App Expr Expr
  | Var Ident
  | Let Binder Expr Expr
  | Case Expr [(ConstructorPattern, Expr)]
  deriving (Generic, Show, Data, Typeable)

data Binder
  = Wildcard
  | Untyped Ident
  | Typed Ident Expr
  deriving (Generic, Show, Data, Typeable)

data ConstructorPattern = ConstructorPattern {name :: String}
  deriving (Generic, Show, Data, Typeable)
