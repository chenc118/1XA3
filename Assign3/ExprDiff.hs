
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-|
Module : ExprDiff
Description: Contains a type class an dinstances for differentiable expressions
Copyright: (c) no one
License : WTFPL aka do WTF you want with it
Maintainer : no one
Stability : None
Portability : MSDOS
TODO write a longer description of the module, containing some commentary with @some markup@.
-}
module ExprDiff where

import ExprType

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
-- | This class operates over the 'Expr' data type
class DiffExpr a where 
    eval :: Map.Map String a -> Expr a -> a
    simplify :: (Eq a) => Map.Map String a -> Expr a -> Expr a
    partDiff :: String -> Expr a -> Expr a
    {-Default Methods-}
    (!+) :: Expr a -> Expr a -> Expr a
    e1 !+ e2  = Add e1 e2
    (!*) :: Expr a -> Expr a -> Expr a
    e1 !* e2  = Mult e1 e2
    exCos :: Expr a -> Expr a
    exCos e1 = Cos e1
    exSin :: Expr a -> Expr a
    exSin e1 = Sin e1
    exNExp :: Expr a -> Expr a
    exNExp e1 = NExp e1
    val :: a -> Expr a
    val x = Const x
    var :: String -> Expr a
    var x = Var x
{-Most intuative instance of DiffExpr   
    Num instances relies on +,- 
    Methods: 
     eval : ...
     simplify : ...
    partDiff : ...
    
-}

-- YOu can tell how annoyed I am with this based on the naming
class (Num a) => ShoeHornFloating a where
    shoeHornCos :: a -> a
    shoeHornSin :: a -> a
    shoeHornExp :: a -> a -> a

instance ShoeHornFloating Double where
    shoeHornCos x = cos x
    shoeHornSin x = sin x
    shoeHornExp a b = a**b

instance ShoeHornFloating Float where
    shoeHornCos x = cos x
    shoeHornSin x = sin x
    shoeHornExp a b = a**b

instance ShoeHornFloating Integer where
    shoeHornCos x = round $ cos $ fromInteger x
    shoeHornSin x = round $ sin $ fromInteger x
    shoeHornExp a b = round $ (fromIntegral a) ** (fromIntegral b)

instance ShoeHornFloating Int where
    shoeHornCos x = round $ cos $ fromIntegral x
    shoeHornSin x = round $ sin $ fromIntegral x
    shoeHornExp a b = round $ (fromIntegral a) ** (fromIntegral b)

instance (ShoeHornFloating a) => DiffExpr a where
    eval vrs (Add e1 e2) = eval vrs e1 + eval vrs e2
    eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
    --eval vrs (Cos e1)     = cos $ eval vrs e1
    eval vrs (Const x) = x
    eval vrs (Var x) = case Map.lookup x vrs of 
                            Just v -> v
                            Nothing -> error "Failed lookup in eval"

    simplify vrs (Add e1 e2)               = let
                                            s1 = simplify vrs e1
                                            s2 = simplify vrs e2
                                        in case (s1,s2) of 
                                            (Const a,Const b)   -> Const (a+b)
                                            (Const 0,se2)       -> se2
                                            (se1,Const 0)       -> se1
                                            (se1,se2)           -> Add se1 se2
    simplify vrs (Mult e1 e2)              = let
                                            s1 = simplify vrs e1
                                            s2 = simplify vrs e2 
                                        in case (s1,s2) of
                                            (Const a,Const b) -> Const (a*b)
                                            (Const 0,_)       -> Const 0
                                            (_,Const 0)       -> Const 0
                                            (Const 1,se2)     -> se2
                                            (se1,Const 1)     -> se1
                                            (se1,se2)         -> Mult se1 se2
    simplify vrs (Var x)                   = case Map.lookup x vrs of
                                                        Just v -> Const v
                                                        Nothing -> Var x
    simplify _ e = e

    partDiff ss (Add e1 e2) = Add (partDiff ss e1) (partDiff ss e2)
    partDiff ss (Mult e1 e2) = Add (Mult e1 (partDiff ss e2)) (Mult e2 (partDiff ss e1))
    partDiff ss (Sin e1) = Mult (Cos e1) $ partDiff ss e1
    partDiff ss (Cos e1) = Mult (Mult (Const (-1)) (Sin e1)) $ partDiff ss e1
    partDiff ss (NExp e1) = Mult (NExp e1) $ partDiff ss e1
    partDiff ss (Ln e1)   = Exp (partDiff ss e1) (Const $ -1)
    -- formula =  d/dx( f(x)^g(x) ) = f(x)^g(x) * d/dx( g(x) ) * ln( f(x) ) + f(x)^( g(x)-1 ) * g(x) * d/dx( f(x) ) 
    partDiff ss (Exp e1 e2) = Add (Mult (Exp e1 e2) $ Mult (partDiff ss e2) $ Ln e1) $ Mult (Exp e1 $ Add e2 $ Const $ -1) $ Mult e2 $ partDiff ss e1
    partDiff ss (Const _) = Const 0
    partDiff ss (Var x) = if x == ss then (Const 1) else (Const 0)

{-instance (Floating a) => DiffExpr a where
    eval vrs (Cos e1) = cos $ eval vrs e1
    eval vrs (Sin e1) = sin $ eval vrs e1
-}

{-instance  (Integral a) => DiffExpr a where
    eval vrs (Cos e1) = round $ cos $ fromIntegral $ eval vrs e1
    eval vrs (Sin e1) = round $ sin $ fromIntegral $ eval vrs e1-}