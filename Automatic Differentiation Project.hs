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
  deriving (Eq,Ord,Show)

differentiate :: EnvHat -> Expr -> Maybe ValueHat
differentiate env e = case e of
  DoubleE f -> Just (VHat (ValueD f) (DerivativeD 0))
  VarE v -> case v of
    Var s -> case Map.lookup s env of
      Just (VHat v1 d1) -> Just (VHat v1 d1)
      Nothing -> Just (VHat (ValueD 0) (DerivativeD 0))
  PlusE e1 e2 -> case differentiate env e1 of
    Just (VHat (ValueD e1v) (DerivativeD e1d)) -> case differentiate env e2 of
      Just (VHat (ValueD e2v) (DerivativeD e2d)) -> Just (VHat (ValueD (e1v + e2v)) (DerivativeD (e1d + e2d)))
      Nothing -> Nothing
    Nothing -> Nothing
  TimesE e1 e2 -> case differentiate env e1 of
    Just (VHat (ValueD e1v) (DerivativeD e1d)) -> case differentiate env e2 of
      Just (VHat (ValueD e2v) (DerivativeD e2d)) -> Just (VHat (ValueD (e1v * e2v)) (DerivativeD ((e1d * e2v) + (e1v * e2d))))
      Nothing -> Nothing
    Nothing -> Nothing
  SinE e1 -> case differentiate env e1 of
    Just (VHat (ValueD e1v) (DerivativeD e1d)) -> Just (VHat (ValueD (sin e1v)) (DerivativeD (e1d * (cos e1v))))
    Nothing -> Nothing
  CosE e1 -> case differentiate env e1 of
    Just (VHat (ValueD e1v) (DerivativeD e1d)) -> Just (VHat (ValueD (cos e1v)) (DerivativeD (-e1d * (sin e1v))))
    Nothing -> Nothing
