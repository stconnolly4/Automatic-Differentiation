{-# LANGUAGE GADTs #-} -- used in testing infrastructure
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module AutomaticDifferentiation where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List                   -- used in testing infrastructure
import Control.Monad hiding (join) -- used in testing infrastructure
import Control.Exception           -- used in testing infrastructure
import System.IO                   -- used in testing infrastructure

data Variable = Var String
  deriving (Eq,Ord,Show)

data Value = ValueD Double
  deriving (Eq, Ord, Show)

data Derivative = DerivativeD Double
  deriving (Eq,Ord,Show)

data ValueHat = VHat Value Derivative
  deriving (Eq,Ord,Show)

type EnvHat = Map String ValueHat

data Expr = VarE Variable
          | DoubleE Double
          | PlusE Expr Expr
          | TimesE Expr Expr
          | SinE Expr
          | CosE Expr
          | LetE String Expr Expr
  deriving (Eq,Ord,Show)
