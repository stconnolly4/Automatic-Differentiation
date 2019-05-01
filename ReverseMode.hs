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

data Derivative Expr = DerivativeEE Expr Expr
  deriving (Show)

--data Derivative = DerivativeD Double
--  deriving (Eq,Ord,Show)

--data Derivative = DerivativeD (Double -> Double)
--  deriving (Eq,Ord,Show)

--derivative :: (Double -> Double)
--derivative d = undefined

data ValueHat = VHat Value Derivative
  deriving (Eq,Ord,Show)

type EnvHat = Map String ValueHat

data Expr = DoubleE Double
          | SinE Expr
  deriving (Eq,Ord,Show)

--Use forward and instead of getting parent derivative use function in that place to send to child

differentiate :: EnvHat -> Expr -> Maybe ValueHat -- we should be producing a value wihtout needing parents derivative, and then we\
differentiate env e = case e of
  --SinE e1 -> differentiate env e1 (DerivativeD (cos d1))
  --DoubleE e1 -> VHat (ValueD e1) d
  DoubleE f -> Just (VHat (ValueD f) (DerivativeD 0))
  SinE e1 -> case differentiate env e1 of
    Just (VHat (ValueD e1v) (DerivativeD e1d)) -> Just (VHat (ValueD (sin e1v)) (\  -> e1d * cos (e1v)) )
    --Just (VHat (ValueD e1v) (DerivativeD e1d)) -> Just (VHat (ValueD (sin e1v)) (\  -> e1d * cos (e1v)) )
    --(DerivativeD (e1d * (cos e1v))))
    Nothing -> Nothing
