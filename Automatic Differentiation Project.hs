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

data Value = Value Float
  deriving (Eq, Ord, Show)

data Derivative = FloatD Float
  deriving (Eq,Ord,Show)

data ValueHat = VHat Value Derivative
  deriving (Eq,Ord,Show)

type EnvHat = Map String ValueHat

data Expr = VarE Variable
          | FloatE Float
          | PlusE Expr Expr
          | TimesE Expr Expr
          | SinE Expr
          | CosE Expr
  deriving (Eq,Ord,Show)

differentiate :: EnvHat -> Expr -> Maybe ValueHat
differentiate env e = case e of
  FloatE f -> Just (VHat (Value f) (FloatD 0))
  VarE v -> case v of
    Var s -> case Map.lookup s env of
      Just (VHat v1 d1) -> Just (VHat (Value 1) (FloatD 1))
      Nothing -> Just (VHat (Value 0) (FloatD 0))
  PlusE e1 e2 -> case differentiate env e1 of
    Just (VHat e1v e1d) -> case differentiate env e2 of
      Just (VHat e2v e2d) -> Just (VHat (Value e1v + e2v) (FloatD e1d + e2d))
      Nothing -> Nothing
    Nothing -> Nothing
  _ -> Nothing
