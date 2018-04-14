module ExprNorm where

import ExprType


{- ExprNorm: Module that contains a bunch of instances and functions
   that help normalize an Expression. Used by the simplify function 
   in ExprDiff.hs
-}

-- * Section : Ordering definition

{- | Basic ordering, wherein things are sorted from least to greatest from most specific to least
    'Const' < 'Var' < 'Cos' < 'Sin' < 'Ln' < 'NExp' < 'Add' < 'Mult' < 'Exp' 
-}
instance (Ord a,Num a) => Ord (Expr a) where
    -- arranged in order of least to greatest, if cons == then test the item that is wrapped
    compare (Const a) (Const b)           = compare a b
    compare (Const _) _                   = LT
    compare _ (Const _)                   = GT
    compare (Var x) (Var y)               = compare x y
    compare (Var _) _                     = LT
    compare _ (Var _)                     = GT
    compare (Cos e1) (Cos e2)             = compare e1 e2
    compare (Cos _) _                     = LT
    compare _ (Cos _)                     = GT
    compare (Sin e1) (Sin e2)             = compare e1 e2
    compare (Sin _) _                     = LT
    compare _ (Sin _)                     = GT
    compare (Ln e1) (Ln e2)               = compare e1 e2
    compare (Ln _) _                      = LT
    compare _ (Ln _)                      = GT
    compare (NExp e1) (NExp e2)           = compare e1 e2
    compare (NExp _) _                    = LT
    compare _ (NExp _)                    = GT
    compare (Mult e11 e12) (Mult e21 e22) = let 
                                            m1 = multNorm (Mult e11 e12)
                                            m2 = multNorm (Mult e21 e22)
                                        in case (m1,m2) of
                                            (Mult e1 _, Mult e2 _) -> compare e1 e2
                                            (e1,e2)                -> compare e1 e2
    compare (Mult _ _) _                  = LT
    compare _ (Mult _ _)                  = GT
    compare (Add e11 e12) (Add e21 e22)   = let 
                                            a1 = addNorm (Add e11 e12)
                                            a2 = addNorm (Add e21 e22)
                                        in case (a1,a2) of
                                            (Add e1 _,Add e2 _)    -> compare e1 e2
                                            (e1,e2)                -> compare e1 e2
                                            _                      -> error "Error in comparing Addition stuff"
    compare (Add _ _) _                   = LT
    compare _ (Add _ _)                   = GT
    compare (Exp e11 e12) (Exp e21 e22)   = let
                                            res = compare e12 e22
                                        in if res == EQ then -- Compare exponent first as that is a better indicator of which is greater
                                                compare e11 e21
                                            else res

{- | Normalize a multiplication Expression
    Always converts a Mult Expression to another Mult Expression in normalized form
-}
multNorm :: (Ord a,Num a)=> Expr a -- ^ An Expression of form ('Mult' e1 e2) any other form will return itself
                        -> Expr a  -- ^ A normalized form of ('Mult' e1 e2) or a Constant if it is a multiplication of constants else the input
multNorm (Mult e1 e2) = case (e1,e2) of 
                        (Mult e11 e12, Mult e21 e22) -> let 
                                                        m1 = multNorm (Mult e11 e12)
                                                        m2 = multNorm (Mult e21 e22)
                                                    in case (m1,m2) of
                                                        (Mult e11 e12, Mult e21 e22) -> let 
                                                                                        res = compare e11 e21
                                                                                    in if res == GT then
                                                                                            (Mult e21 (multNorm $ Mult e11 (Mult e12 e22)))
                                                                                        else if res == LT then
                                                                                            (Mult e11 (multNorm $ Mult e21 (Mult e12 e22)))
                                                                                        else 
                                                                                            multNorm $ Mult (Exp e11 (Const 2)) (Mult e12 e22)
                                                        (m1,m2)                      -> multNorm $ Mult m1 m2
                        (Mult e11 e12, e2)           -> case (multNorm $ Mult e11 e12) of
                                                        (Mult e1' e2') -> let 
                                                                        res = compare e2 e1'
                                                                    in if res == LT then
                                                                        Mult e2 (multNorm $ Mult e1' e2')
                                                                    else if res == GT then
                                                                        Mult e1' (multNorm $ Mult e2 e2')
                                                                    else multNorm (Mult (Exp e2 (Const 2)) e2')
                                                        m1             -> m1
                        (e1, Mult e21 e22)           -> case (multNorm $ Mult e21 e22) of
                                                        (Mult e1' e2') -> let
                                                                        res = compare e1 e1'
                                                                    in if res == LT then
                                                                        Mult e1 (multNorm $ Mult e1' e2')
                                                                    else if res == GT then
                                                                        Mult e1' (multNorm $ Mult e1 e2')
                                                                    else multNorm (Mult (Exp e1 (Const 2)) e2')
                                                        m1             -> m1
                        (Const a, Const b)           -> Const (a*b)
                        (e1, e2)                     -> let
                                                        res = compare e1 e2
                                                    in if res == GT then
                                                        Mult e2 e1
                                                    else if res == EQ then
                                                        Exp (e1) (Const 2)
                                                    else Mult e1 e2
multNorm e            = e -- Do nothing for expressions that are not multiplication 

expNorm :: (Num a,Ord a) => Expr a -> Expr a
exprNorm a = undefined

{- | Normalize an addition Expression
    Always converts a Add Expression to antoher Add expression in normalized form

-}
addNorm :: (Ord a,Num a) => Expr a -> Expr a
addNorm = undefined