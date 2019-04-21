{-# LANGUAGE GADTs #-} -- used in testing infrastructure
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module AutomaticDifferentiation where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.List                   -- used in testing infrastructure
import Control.Monad hiding (join) -- used in testing infrastructure
import Control.Exception           -- used in testing infrastructure
import System.IO                   -- used in testing infrastructure

data Expr = IntE Integer
          | PlusE Expr Expr
          | TimesE Expr Expr
          | VarE String
          | LetE String Expr Expr

  deriving (Eq,Ord,Read,Show)

data Value = IntV Integer
  deriving (Eq,Ord,Show)

data Derivative = IntD Integer
  deriving (Eq,Ord,Show)

-- γ^ ∈ env^ ≜ var ⇀ value^
type Env = Map String Value'
