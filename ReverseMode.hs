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
          | CosE Expr
          | PlusE Expr Expr
  deriving (Eq,Ord,Show)

drive ::EnvHat -> Expr -> Maybe FMValueHat
drive env e = case differentiate env e of
  Just (VHat v (DerivativeD f)) -> Just (FMVHat v (FMDerivativeD (f 1)))
  Nothing -> Nothing

differentiate :: EnvHat -> Expr -> Maybe ValueHat
differentiate env e = case e of
  DoubleE f -> Just (VHat (ValueD f) (DerivativeD (\x->0)))
  VarE v -> case v of
    Var s -> Map.lookup s env
  SinE e1 -> case differentiate env e1 of
    Just (VHat (ValueD e1v) (DerivativeD e1d)) -> Just (VHat (ValueD (sin e1v)) (DerivativeD (\d -> (e1d e1v) * cos(e1v))))
    Nothing -> Nothing
  CosE e1 -> case differentiate env e1 of
    Just (VHat (ValueD e1v) (DerivativeD e1d)) -> Just (VHat (ValueD (cos e1v)) (DerivativeD (\d -> -(e1d e1v) * sin(e1v))))
    Nothing -> Nothing
  PlusE e1 e2 -> case differentiate env e1 of
    Just (VHat (ValueD e1v) (DerivativeD e1d)) -> case differentiate env e2 of
      Just (VHat (ValueD e2v) (DerivativeD e2d)) -> Just (VHat (ValueD (e1v + e2v)) (DerivativeD (\d -> (e1d d)+ (e2d d))))
      Nothing -> Nothing
    Nothing -> Nothing



--- manual test cases (run in Terminal) ---

--- drive Map.empty (DoubleE 7)
--- FMVHat (ValueD 7.0) (FMDerivativeD 0.0)

--- drive Map.empty (SinE (DoubleE 7))
--- FMVHat (ValueD 0.6569865987187891) (FMDerivativeD 0.0)

--- drive (Map.fromList [("x", VHat (ValueD 1) (DerivativeD (\x->0)))]) (DoubleE 7)
--- FMVHat (ValueD 7.0) (FMDerivativeD 0.0)

--- drive (Map.fromList [("x", VHat (ValueD 7) (DerivativeD (\x->0)))]) (VarE (Var "x"))
--- FMVHat (ValueD 7.0) (FMDerivativeD 0.0)

--- drive (Map.fromList [("x", VHat (ValueD 7) (DerivativeD (\x->0)))]) (VarE (Var "y"))
--- Nothing

--- drive Map.empty (CosE (DoubleE 7))
--- Just (VHat (ValueD 0.7539022543433046) (DerivativeD (-0.0)))

--- drive (Map.fromList [("x", VHat (ValueD 7) (DerivativeD (\x->0)))]) (SinE(CosE (CosE (VarE (Var "x")))))
-- Just (VHat (ValueD 0.6661415625501989) (DerivativeD 0.0))

-- drive (Map.fromList [("x", VHat (ValueD 10) (DerivativeD (\x->1)))]) (SinE (VarE (Var "x")))
-- Just (VHat (ValueD (-0.5440211108893699)) (DerivativeD (-0.8390715290764524)))

-- drive (Map.fromList [("x", VHat (ValueD 10) (DerivativeD (\x->1))), ("y", VHat (ValueD 20) (DerivativeD (\x->0)))]) (SinE (PlusE (VarE (Var "x")) (VarE (Var "y"))))
-- Just (FMVHat (ValueD (-0.9880316240928618)) (FMDerivativeD 0.15425144988758405))

-- drive (Map.fromList [("x", VHat (ValueD 10) (DerivativeD (\x->1))), ("y", VHat (ValueD 20) (DerivativeD (\x->0)))]) (PlusE (VarE (Var "x")) (VarE (Var "y")))
-- Just (FMVHat (ValueD 30.0) (FMDerivativeD 1.0))
