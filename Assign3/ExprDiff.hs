{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
    simplify :: Map.Map String a -> Expr a -> Expr a
    partDiff :: String -> Expr a -> Expr a
    {-Default Methods-}
    (!+) :: Expr a -> Expr a -> Expr a
    e1 !+ e2  = Add e1 e2
    (!*) :: Expr a -> Expr a -> Expr a
    e1 !* e2  = Mult e1 e2
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
instance (Num a) => DiffExpr a where
    eval vrs (Add e1 e2) = eval vrs e1 + eval vrs e2
    eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
    eval vrs (Const x) = x
    --eval vrs (Cos e1)  = cos $ eval vrs e1
    eval vrs (Var x) = case Map.lookup x vrs of 
                            Just v -> v
                            Nothing -> error "Failed lookup in eval"

    simplify _ e = e
    partDiff _ e = e
