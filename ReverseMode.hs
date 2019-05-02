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

drive :: Maybe ValueHat -> Maybe FMValueHat
drive mvh = case mvh of
  Just (VHat v (DerivativeD f)) -> Just (FMVHat v (FMDerivativeD (f 1)))
  Nothing -> Nothing

--Use forward and instead of getting parent derivative use function in that place to send to child

differentiate :: EnvHat -> Expr -> Maybe ValueHat -- we should be producing a value wihtout needing parents derivative, and then we\
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

--- drive (differentiate Map.empty (DoubleE 7))
--- FMVHat (ValueD 7.0) (FMDerivativeD 0.0)

--- drive (differentiate Map.empty (SinE (DoubleE 7)))
--- FMVHat (ValueD 0.6569865987187891) (FMDerivativeD 0.0)

--- drive (differentiate (Map.fromList [("x", VHat (ValueD 1) (DerivativeD (\x->0)))]) (DoubleE 7))
--- FMVHat (ValueD 7.0) (FMDerivativeD 0.0)

--- drive (differentiate (Map.fromList [("x", VHat (ValueD 7) (DerivativeD (\x->0)))]) (VarE (Var "x")))
--- FMVHat (ValueD 7.0) (FMDerivativeD 0.0)

--- drive (differentiate (Map.fromList [("x", VHat (ValueD 7) (DerivativeD (\x->0)))]) (VarE (Var "y")))
--- Nothing

--- drive (differentiate Map.empty (CosE (DoubleE 7)))
--- Just (FMVHat (ValueD 0.7539022543433046) (FMDerivativeD (-0.6569865987187891)))
--- from forward: Just (VHat (ValueD 0.7539022543433046) (DerivativeD (-0.0)))

----THESE CASES BELOW DON'T WORK----

--- drive (differentiate (Map.fromList [("x", VHat (ValueD 7) (DerivativeD (\x->0)))]) (SinE(CosE (CosE (VarE (Var "x"))))))
-- Just (VHat (ValueD 0.6661415625501989) (DerivativeD 0.0))

-- drive (differentiate (Map.fromList [("x", VHat (ValueD 10) (DerivativeD (\x->1)))]) (SinE (VarE (Var "x"))))
-- Just (VHat (ValueD (-0.5440211108893699)) (DerivativeD (-0.8390715290764524)))

-- drive (differentiate ((Map.fromList [("x", VHat (ValueD 10) (DerivativeD (\x->1))), ("y", VHat (ValueD 20) (DerivativeD (\x->0))]) (SinE (PlusE (VarE (Var "x")) (VarE (Var "y")))))

-- drive (differentiate (Map.fromList [("x", VHat (ValueD 10) (DerivativeD (\x->1))), ("y", VHat (ValueD 20) (DerivativeD (\x->0)))]) (SinE (PlusE (VarE (Var "x")) (VarE (Var "y")))))



-- differentiateTests :: (Int,String,(EnvHat -> Expr -> Maybe ValueHat),[((EnvHat,Expr),Maybe ValueHat)])
-- differentiateTests =
--   ( 1
--   , "drive"
--   , drive differentiate
--   , [(  (Map.empty,DoubleE 5),
--         Just (VHat (ValueD 5) (FMDerivativeD 0))
--      )
--      -- ,
--      -- (  ((Map.fromList [("x", VHat (ValueD 1) (DerivativeD 1)), ("y", VHat (ValueD 2) (DerivativeD 0))]),(PlusE (TimesE (DoubleE 17.2) (VarE (Var "x"))) (DoubleE 7))),
--      --       Just (VHat (ValueD 24.2) (DerivativeD 17.2))
--      --  )
--      --  ,
--      --  ( ((Map.fromList [("x", VHat (ValueD 9) (DerivativeD 1)), ("y", VHat (ValueD 2) (DerivativeD 0))]), (PlusE (TimesE (DoubleE 17.2) (VarE (Var "x"))) (DoubleE 7))),
--      --        Just (VHat (ValueD 161.79999999999998) (DerivativeD 17.2))
--      --  )
--      --  ,
--      --  ( ((Map.fromList [("x", VHat (ValueD 10) (DerivativeD 1)), ("y", VHat (ValueD 20) (DerivativeD 0))]), (SinE (TimesE (VarE (Var "x")) (VarE (Var "y"))))),
--      --        Just (VHat (ValueD (-0.8732972972139945)) (DerivativeD 9.74375350014012))
--      --  )
--      --  ,
--      --  ( (Map.empty, (LetE "x" (DoubleE 7) (PlusE (DoubleE 5) (VarE ( Var "x"))))),
--      --        Just (VHat (ValueD (12)) (DerivativeD 0))
--      --
--      --  )
--     ]
--   )
--
--
-- ---------------
-- -- ALL TESTS --
-- ---------------
-- allTests :: [Test]
-- allTests =
--   [ Test2 differentiateTests
--   ]
--
-- ----------------------
-- -- MAIN = RUN TESTS --
-- ----------------------
--
-- main :: IO ()
-- main = runTests allTests
--
-- ----------------------------
-- -- TESTING INFRASTRUCTURE --
-- ----------------------------
--
-- mapOn :: [a] -> (a -> b) -> [b]
-- mapOn = flip map
--
-- foldMOn :: (Foldable t,Monad m) => b -> t a -> (b -> a -> m b) -> m b
-- foldMOn i xs f = foldM f i xs
--
-- data Test where
--   Test1 :: (Show a,Eq b,Show b) => (Int,String,a -> b,[(a,b)]) -> Test
--   Test2 :: (Show a,Show b,Eq c,Show c) => (Int,String,a -> b -> c,[((a,b),c)]) -> Test
--   Test3 :: (Show a,Show b,Show c,Eq d,Show d) => (Int,String,a -> b -> c -> d,[((a,b,c),d)]) -> Test
--
-- runTests :: [Test] -> IO ()
-- runTests ts = do
--   rs <- forM ts $ \ t -> do
--     y <- case t of
--       Test1 t -> runTests1 t
--       Test2 t -> runTests2 t
--       Test3 t -> runTests3 t
--     putStrLn ""
--     return y
--   forM_ (zip [1..] rs) $ \ (m,(n,passed,failed)) -> do
--     when (m /= 1) $ putStrLn ""
--     putStrLn $ "++ E" ++ show n ++ " Tests Passed: " ++ show passed
--     putStrLn $ "-- E" ++ show n ++ " Tests Failed: " ++ show failed
--
-- showTestResult :: (Eq a,Show a) => String -> a -> Either String a -> (Int,Int) -> IO (Int,Int)
-- showTestResult fx y y'M (passed,failed) = do
--   let eM = case y'M of
--         Left e -> Just $ "[ERROR]: " ++ e
--         Right y' ->
--           if y' == y
--           then Nothing
--           else Just $ show y'
--   case eM of
--     Nothing -> do
--       putStrLn $ "   [TEST PASSED]: " ++ fx
--       hFlush stdout
--       return (passed+1,failed)
--     Just s -> do
--       putStrLn $ "   [TEST FAILED]:"
--       putStrLn $ "     -- the input"
--       putStrLn $ "     " ++ fx
--       putStrLn $ "   =="
--       putStrLn $ "     -- the output"
--       putStrLn $ "     " ++ s
--       putStrLn $ "   /="
--       putStrLn $ "     -- the expected result"
--       putStrLn $ "     " ++ show y
--       hFlush stdout
--       return (passed,failed+1)
--
-- runTestsN :: (Eq a,Show a) => Int -> String -> [(String,() -> a,a)] -> IO (Int,Int,Int)
-- runTestsN n name tests = do
--   putStrLn $ ">> E" ++ show n ++ " Tests: " ++ name
--   (passed,failed) <- foldMOn (0,0) tests $ \ pf (s,fx,y) -> do
--     y'M <- catch (Right <$> evaluate (fx ())) $ \ (SomeException e) -> return $ Left $ chomp $ unwords $ lines $ show e
--     showTestResult s y y'M pf
--   return (n,passed,failed)
--   where
--     chomp s0 = concat $ mapOn (group s0) $ \ s ->
--       if " " `isPrefixOf` s then " " else s
--
-- runTests1 :: (Eq b,Show a,Show b) => (Int,String,a -> b,[(a,b)]) -> IO (Int,Int,Int)
-- runTests1 (n,name,f,tests) = runTestsN n name $ mapOn tests $ \ (x,y) ->
--   (name ++ " " ++ showsPrec 11 x [],\() -> f x,y)
--
-- runTests2 :: (Eq c,Show a,Show b,Show c) => (Int,String,a -> b -> c,[((a,b),c)]) -> IO (Int,Int,Int)
-- runTests2 (n,name,f,tests) = runTestsN n name $ mapOn tests $ \ ((x,y),z) ->
--   (name ++ " " ++ showsPrec 11 x [] ++ " " ++ showsPrec 11 y [],\() -> f x y,z)
--
-- runTests3 :: (Eq d,Show a,Show b,Show c,Show d) => (Int,String,a -> b -> c -> d,[((a,b,c),d)]) -> IO (Int,Int,Int)
-- runTests3 (n,name,f,tests) = runTestsN n name $ mapOn tests $ \ ((w,x,y),z) ->
--   (name ++ " " ++ showsPrec 11 w [] ++ " " ++ showsPrec 11 x [] ++ " " ++ showsPrec 11 y [],\() -> f w x y,z)
