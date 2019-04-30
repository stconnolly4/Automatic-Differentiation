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

data Derivative = DerivativeD (Double -> Double)
  deriving (Eq,Ord,Show)

data ValueHat = VHat Value Derivative
  deriving (Eq,Ord,Show)

type EnvHat = Map String ValueHat

data Expr = DoubleE Double
          | SinE Expr
  deriving (Eq,Ord,Show)

--calculateDerivative :: EnvHat -> Derivative -> Derivative

differentiate :: EnvHat -> Expr -> ValueHat -- we should be producing a value wihtout needing parents derivative, and then we\
differentiate env e d = case (e, d) of
  (SinE e1, DerivativeD d1) -> differentiate env e1 (DerivativeD (cos d1))
  (DoubleE e1, _) -> VHat (ValueD e1) d
