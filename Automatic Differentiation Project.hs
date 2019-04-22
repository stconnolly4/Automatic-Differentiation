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

<<<<<<< HEAD
data Expr = IntE Integer
          | PlusE Expr Expr
          | TimesE Expr Expr
          | VarE String
          | LetE String Expr Expr

  deriving (Eq,Ord,Read,Show)

data Value = IntV Integer
=======
data Value = FloatV Float
>>>>>>> b18dc6c9663696441771a691ed0bb91316687552
  deriving (Eq,Ord,Show)

data Derivative = FloatD Float
  deriving (Eq,Ord,Show)

<<<<<<< HEAD
-- γ^ ∈ env^ ≜ var ⇀ value^
type Env = Map String Value'
=======
data ValueHat = Value Derivative

type EnvHat = Map String ValueHat
>>>>>>> b18dc6c9663696441771a691ed0bb91316687552
