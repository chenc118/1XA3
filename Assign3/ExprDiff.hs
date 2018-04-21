
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-|
Module : ExprDiff
Description: Contains a type class an dinstances for differentiable expressions
Copyright: (c) chenc118 @ 2018
License : WTFPL
Maintainer : no one
Stability : experimental
Portability : MSDOS
-}
module ExprDiff where

import ExprType
import ExprNorm

import qualified Data.Map as Map



{-Class DiffExpr
    Differentiaiable Expression
    --------------------------
    This class has methods over the Expr datatype that assist with construction and evaluation of differentiable expressions
    --------------------------
    Methods:
    eval : Takes a dictionary of variable identifiers and values, and uses it to 
    compute hte Epxr fully
    simplifty : takes a possibly incompletely dictionary and uses it to reduce Expr as much as possible
    partDiff: given an var identifier, differentiate in terms of that identifier
    Default Methods:
        !+,!*,val,var : are function wrappers for Expr constructors that perform additional simplification
    
-}
-- * Section : Diff Expr 

-- | This class operates over the 'Expr' data type
class DiffExpr a where 
    -- | Evaluate an expression fully, values for all variables of the expression must be supplied otherwise or an error will be thrown use 'simplify' if you cannot supply all values
    eval :: Map.Map String a -> Expr a -> a
    -- | Simplifies an expressions replacing variables the values in the given map, and partially evaluating when possible
    simplify :: (Ord a) => Map.Map String a -> Expr a -> Expr a
    -- | Unary simplification, uses an empty map.
    --
    --    Equivalent to
    -- 
    -- > simplify (Map.fromList [])
    usimplify :: (Ord a) => Expr a -> Expr a 
    usimplify = simplify (Map.fromList [])
    -- | performs partial differentiation with respect to the given String identifier for a variable
    partDiff :: String -> Expr a -> Expr a
    {-Default Methods-}
    -- | Equivalent to 'Add' constructor
    (!+) :: Expr a -> Expr a -> Expr a
    e1 !+ e2  = Add e1 e2
    -- | Equivalent to 'Mult' constructor
    (!*) :: Expr a -> Expr a -> Expr a
    e1 !* e2  = Mult e1 e2
    -- | Equivalent to 'Cos' constructor
    exCos :: Expr a -> Expr a
    exCos e1 = Cos e1
    -- | Equivalent to 'Sin' constructor
    exSin :: Expr a -> Expr a
    exSin e1 = Sin e1
    -- | Equivalent to 'NExp' constructor
    exNExp :: Expr a -> Expr a
    exNExp e1 = NExp e1
    -- | Equivalent to 'Ln' constructor
    exLn :: Expr a -> Expr a
    exLn e1 = Ln e1
    -- | Equivalent to 'Exp' constructor
    (!^) :: Expr a -> Expr a -> Expr a
    (!^) e1 e2 = Exp e1 e2
    -- | Equivalent to 'Const' constructor
    val :: a -> Expr a
    val x = Const x
    -- | Equivalent to 'Var' constructor
    var :: String -> Expr a
    var x = Var x


-- | Generic instance for Differential Expressions
-- 
-- Create an instance of 'ShoeHornFloating' to define some of the basic Floating functions used
instance (ShoeHornFloating a) => DiffExpr a where
    eval vrs (Add e1 e2) = eval vrs e1 + eval vrs e2
    eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
    eval vrs (Cos e1)     = shoeHornCos $ eval vrs e1
    eval vrs (Sin e1)     = shoeHornSin $ eval vrs e1
    eval vrs (NExp e1)    = shoeHornNExp $ eval vrs e1
    eval vrs (Ln e1)      = shoeHornLn $ eval vrs e1
    eval vrs (Exp e1 e2)  = shoeHornExp (eval vrs e1) $ eval vrs e2
    eval vrs (Const x) = x
    eval vrs (Var x) = case Map.lookup x vrs of 
                            Just v -> v
                            Nothing -> error "Failed lookup in eval"

    simplify vrs (Add e1 e2)               = let
                                        a1 = addNorm (Add e1 e2)
                                        in case a1 of
                                        (Add e1 e2)  -> let
                                                s1 = simplify vrs $ addNorm e1
                                                s2 = simplify vrs $ addNorm e2
                                            in case (s1,s2) of 
                                                (Const a,Const b)   -> Const (a+b)
                                                (Const 0,se2)       -> se2
                                                (se1,Const 0)       -> se1
                                                (se1,se2)           -> addNorm $ Add se1 se2
                                        e             -> simplify vrs e
    simplify vrs (Mult e1 e2)              = let
                                        m1 = addNorm $ multNorm (Mult e1 e2)
                                        in case m1 of
                                        (Mult e1 e2) -> let
                                                s1 = simplify vrs $ multNorm e1
                                                s2 = simplify vrs $ multNorm e2 
                                            in case (s1,s2) of
                                                (Const a,Const b) -> Const (a*b)
                                                (Const 0,_)       -> Const 0
                                                (_,Const 0)       -> Const 0
                                                (Const 1,se2)     -> se2
                                                (se1,Const 1)     -> se1
                                                (se1,se2)         ->  Mult se1 se2
                                        e            -> simplify vrs e 
    simplify vrs (Cos e1)                  = let
                                            s1 = simplify vrs e1
                                        in case s1 of
                                            (Const a)   -> Const $ eval vrs (Cos e1)
                                            se1         -> Cos se1
    simplify vrs (Sin e1)                  = let
                                            s1 = simplify vrs e1
                                        in case s1 of
                                            (Const a)   -> Const $ eval vrs (Sin e1)
                                            se1         -> Sin se1 
    simplify vrs (NExp e1)                 = let
                                            s1 = simplify vrs e1
                                        in case s1 of
                                            (Const 0)         -> Const 1
                                            (Const a)         -> Const $ eval vrs (NExp s1)
                                            (se1)             -> NExp se1
    simplify vrs (Ln e1)                   = let 
                                            s1 = simplify vrs e1
                                        in case s1 of
                                            (Const 1)         -> Const 0
                                            (Const a)         -> Const $ eval vrs (Ln s1)
                                            (se1)             -> lnNorm $ Ln se1
    simplify vrs (Exp e1_ e2_)               = let
                                            (Exp e1 e2) = expNorm $ (Exp e1_ e2_)
                                            s1 = simplify vrs e1
                                            s2 = simplify vrs e2
                                        in case (s1,s2) of
                                            (_,Const 0)       -> Const 1
                                            (e1,Const 1)      -> e1
                                            (Const a,Const b) -> Const $ eval vrs (Exp s1 s2)
                                            (se1,se2)         -> Exp se1 se2

    simplify vrs (Const a)                 = Const a -- no simplification really needed here
    simplify vrs (Var x)                   = case Map.lookup x vrs of
                                                        Just v -> Const v
                                                        Nothing -> Var x

    partDiff ss (Add e1 e2) = Add (partDiff ss e1) (partDiff ss e2)
    partDiff ss (Mult e1 e2) = Add (Mult e1 (partDiff ss e2)) (Mult e2 (partDiff ss e1))
    partDiff ss (Sin e1) = Mult (Cos e1) $ partDiff ss e1
    partDiff ss (Cos e1) = Mult (Mult (Const (-1)) (Sin e1)) $ partDiff ss e1
    partDiff ss (NExp e1) = Mult (NExp e1) $ partDiff ss e1
    partDiff ss (Ln e1)   = Mult (partDiff ss e1) $ Exp e1  $ Const $ -1
    -- formula =  d/dx( f(x)^g(x) ) = f(x)^g(x) * d/dx( g(x) ) * ln( f(x) ) + f(x)^( g(x)-1 ) * g(x) * d/dx( f(x) ) 
    partDiff ss (Exp e1 e2) = Add (Mult (Exp e1 e2) $ Mult (partDiff ss e2) $ Ln e1) $ Mult (Exp e1 $ Add e2 $ Const $ -1) $ Mult e2 $ partDiff ss e1 -- overused $ cause hate brackets, TODO cleanup
    partDiff ss (Const _) = Const 0
    partDiff ss (Var x) = if x == ss then (Const 1) else (Const 0)


-- ** Shoehorning stuff to get DiffExpr to work with integers

-- You can tell how annoyed I am with this based on the naming
-- | A class to shoehorn the integral types into the floating type (for some function)
class (Num a) => ShoeHornFloating a where
    -- | Cosine function
    shoeHornCos :: a -> a
    -- | Sine function
    shoeHornSin :: a -> a
    -- | Natural Logarithm function, in haskell 'log' 
    shoeHornLn :: a -> a
    -- | Natural Exponentiation functoion, in haskell 'exp' 
    shoeHornNExp :: a -> a
    -- | Exponentiation function, in Haskell '(**)'
    shoeHornExp :: a -> a -> a

-- | basically uses the Floating methods for Double
instance ShoeHornFloating Double where
    shoeHornCos x   = cos x
    shoeHornSin x   = sin x
    shoeHornLn x    = log x
    shoeHornNExp x  = exp x
    shoeHornExp a b = a**b

-- | basically uses the Floating methods for Float
instance ShoeHornFloating Float where
    shoeHornCos x   = cos x
    shoeHornSin x   = sin x
    shoeHornLn x    = log x
    shoeHornNExp x  = exp x
    shoeHornExp a b = a**b

-- | converts to a double to compute the math in Floating then rounds
instance ShoeHornFloating Integer where
    shoeHornCos x   = round $ cos $ fromInteger x
    shoeHornSin x   = round $ sin $ fromInteger x
    shoeHornLn x    = round $ log $ fromInteger x
    shoeHornNExp x  = round $ exp $ fromInteger x
    shoeHornExp a b = round $ (fromIntegral a) ** (fromIntegral b)

-- | converts to a double to compute the math in floating then rounds
instance ShoeHornFloating Int where
    shoeHornCos x   = round $ cos $ fromIntegral x
    shoeHornSin x   = round $ sin $ fromIntegral x
    shoeHornLn x    = round $ log $ fromIntegral x
    shoeHornNExp x  = round $ exp $ fromIntegral x
    shoeHornExp a b = round $ (fromIntegral a) ** (fromIntegral b)
