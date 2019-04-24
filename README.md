# Automatic-Differentiation

So far, we have read various articles, blog posts, and Wikipedia pages regarding Forward Automatic Differentiation. We created a GitHub repository so that we could better collaborate on this project. We created various data structures and one type (for the environment), and began to create the recursive differentiation function. Our next step is to create a testing infrastructure and various tests to see if our code is working as expected. After completing this step, we plan to finish implementing the differentiation function for the data types we have already defined. Afterwards, we plan to expand the data types and the differentiation function to include more complex functions.

differentiate (Map.fromList [("x", VHat (ValueD 1) (DerivativeD 1))]) (PlusE (TimesE (DoubleE 17.2) (VarE (Var "x"))) (DoubleE 7))

Just (VHat (ValueD 24.2) (DerivativeD 17.2))

differentiate (Map.fromList [("x", VHat (ValueD 3) (DerivativeD 1))]) (PlusE (TimesE (DoubleE 17.2) (VarE (Var "x"))) (DoubleE 7))

Just (VHat (ValueD 58.599999999999994) (DerivativeD 17.2))



differentiate (Map.fromList [("x", VHat (ValueD 1) (DerivativeD 1)), ("y", VHat (ValueD 2) (DerivativeD 0))]) (PlusE (TimesE (DoubleE 17.2) (VarE (Var "x"))) (DoubleE 7))

Just (VHat (ValueD 24.2) (DerivativeD 17.2))

differentiate (Map.fromList [("x", VHat (ValueD 9) (DerivativeD 1)), ("y", VHat (ValueD 2) (DerivativeD 0))]) (PlusE (TimesE (DoubleE 17.2) (VarE (Var "x"))) (DoubleE 7))

Just (VHat (ValueD 161.79999999999998) (DerivativeD 17.2))


differentiate (Map.fromList [("x", VHat (ValueD 9) (DerivativeD 1)), ("y", VHat (ValueD 2) (DerivativeD 0))]) (TimesE (VarE (Var "x")) (VarE (Var "y")))

Just (VHat (ValueD 18.0) (DerivativeD 2.0))


differentiate (Map.fromList [("x", VHat (ValueD 9) (DerivativeD 1)), ("y", VHat (ValueD 2) (DerivativeD 0))]) (TimesE (VarE (Var "x")) (VarE (Var "z")))

Just (VHat (ValueD 0.0) (DerivativeD 0.0))