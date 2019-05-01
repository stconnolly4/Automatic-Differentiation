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

data FMDerivative = FMDerivativeD Double
  deriving (Eq,Ord,Show)


data ValueHat = VHat Value Derivative

data FMValueHat = FMVHat Value FMDerivative
  deriving (Eq,Ord,Show)

type EnvHat = Map String ValueHat

data Expr = DoubleE Double
          | VarE Variable
          | SinE Expr
  deriving (Eq,Ord,Show)

drive :: ValueHat -> FMValueHat
drive (VHat v (DerivativeD f)) = FMVHat v (FMDerivativeD (f 1))

--Use forward and instead of getting parent derivative use function in that place to send to child

differentiate :: EnvHat -> Expr -> Maybe ValueHat -- we should be producing a value wihtout needing parents derivative, and then we\
differentiate env e = case e of
  DoubleE f -> Just (VHat (ValueD f) (DerivativeD (\0->0)))
  VarE v -> case v of
    Var s -> Map.lookup s env
  SinE e1 -> case differentiate env e1 of
    Just (VHat (ValueD e1v) (DerivativeD e1d)) -> Just (VHat (ValueD (sin e1v)) (DerivativeD (\d -> -d * sin(e1v))))
    Nothing -> Nothing
