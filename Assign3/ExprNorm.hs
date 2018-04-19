module ExprNorm where

import ExprType

import Data.List


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
    -- Exp shoved at the head for special ordering such that the exponent is "near" their term such that they can be grouped easily
    compare (Exp e11 e12) (Exp e21 e22)   = let
                                            res = compare e11 e21
                                        in if res == EQ then 
                                                compare e12 e22
                                            else res
    compare (Exp e11 e12) e2              = case compare e11 e2 of
                                        LT -> LT
                                        GT -> GT
                                        EQ -> GT
    compare  e1 (Exp e21 e22)             = case compare e1 e21 of
                                        LT -> LT
                                        GT -> GT
                                        EQ -> LT
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
                                            (Mult e11 e12, Mult e21 e22) -> let
                                                                res = compare e11 e21
                                                                in if res == EQ then compare e12 e22 else res
                                            (e1,e2)                      -> compare e1 e2
    compare (Mult _ _) _                  = LT
    compare _ (Mult _ _)                  = GT
    compare (Add e11 e12) (Add e21 e22)   = let 
                                            a1 = addNorm (Add e11 e12)
                                            a2 = addNorm (Add e21 e22)
                                        in case (a1,a2) of
                                            (Add e11 e12,Add e21 e22)    -> let
                                                                res = compare e11 e21
                                                                in if res ==EQ then compare e12 e22 else res
                                            (e1,e2)                -> compare e1 e2
    {-compare (Add _ _) _                   = LT
    compare _ (Add _ _)                   = GT-}
    
{- | Normalize a multiplication Expression
    Always converts a Mult Expression to another Mult Expression or Const Expression in normalized form.
    This is a recursive normalization function, there is an alternate multiplication normalization function that uses lists.
-}
multNorm :: (Ord a,Num a)=> Expr a -- ^ An Expression of form ('Mult' e1 e2) any other form will return itself
                        -> Expr a  -- ^ A normalized form of ('Mult' e1 e2) or a Constant if it is a multiplication of constants else the input
{-  Hello person reading source code, welcome to a world of confusion, thanks to how many separate cases there are to consider, when normalizing a multiplication expression
    Just look at the list based normalization and it's basically the same thing but no lists, just constant recursions to itself.
    There are over 50 end points of which about 50% recurse back onto this function.
-}
multNorm (Mult e1 e2) = case (e1,e2) of 
                        (Mult e11 e12, Mult e21 e22) -> let 
                                                        m1 = multNorm (Mult e11 e12)
                                                        m2 = multNorm (Mult e21 e22)
                                                    in case (m1,m2) of
                                                        (Mult e11 e12, Mult e21 e22) -> let 
                                                                                        res = compare e11 e21
                                                                                    in if res == GT then
                                                                                            multNorm $ Mult e21 $ Mult e11 $ Mult e12 e22
                                                                                        else if res == LT then
                                                                                            multNorm $ Mult e11 $ Mult e21 $ Mult e12 e22
                                                                                        else 
                                                                                            multNorm $ Mult (expNorm $ Exp e11 (Const 2)) (Mult e12 e22)
                                                        (m1,m2)                      -> multNorm $ Mult m1 m2
                        (Mult e11 e12, e2)            -> multNorm $ Mult e2 e1 
                        (e1_, Mult e21 e22)           -> case (multNorm $ Mult e21 e22) of
                                                        (Mult e1'_ e2') -> let
                                                                        e1 = expNorm e1_
                                                                        e1' = expNorm e1'_
                                                                        res = compare e1 e1'
                                                                    in if res == LT then
                                                                        case e1' of 
                                                                            (Exp (Mult e111' e112') e12') -> multNorm $ Mult (e2') $ Mult (e1) $ expandExp e1'
                                                                            (Exp e11' e12') -> case e1 of 
                                                                                            (Exp (Mult _ _) e12) -> multNorm $ Mult e2' $ Mult e1' $ expandExp e1
                                                                                            (Exp e11 e12) -> let
                                                                                                        res = compare e11 e11'
                                                                                                        in case res of
                                                                                                            EQ -> multNorm $ Mult (expNorm $ Exp e11' (addNorm $ Add e12' e12)) e2'
                                                                                                            _  -> Mult e1 (multNorm $ Mult e1' e2')
                                                                                            _             -> let
                                                                                                        res = compare e1 e11'
                                                                                                        in case res of 
                                                                                                            EQ -> multNorm $ Mult (expNorm $ Exp e11' (addNorm $ Add e12' $ Const 1)) e2'
                                                                                                            _  -> Mult e1 (multNorm $ Mult e1' e2')
                                                                            (Const a)        -> case e1 of
                                                                                            (Exp (Mult _ _) e12) -> multNorm $ Mult e2' $ Mult e1' $ expandExp e1
                                                                                            (Exp e11 e12) -> let
                                                                                                        res = compare e11 e1'
                                                                                                        in case res of 
                                                                                                            EQ -> multNorm $ Mult (expNorm $ Exp e11 (addNorm $ Add e12 $ Const 1)) e2'
                                                                                                            _  -> Mult e1 (multNorm $ Mult e1' e2')
                                                                                            (Const b)     -> Mult (Const (a*b)) e2'
                                                                                            _             -> Mult e1 (multNorm $ Mult e1' e2')
                                                                            _                -> case e1 of
                                                                                            (Exp (Mult _ _) e12) -> multNorm $ Mult e2' $ Mult e1' $ expandExp e1
                                                                                            (Exp e11 e12) -> let
                                                                                                        res = compare e11 e1'
                                                                                                        in case res of 
                                                                                                            EQ -> multNorm $ Mult (expNorm $ Exp e11 (addNorm $ Add e12 $ Const 1)) e2'
                                                                                                            _  -> Mult e1 (multNorm $ Mult e1' e2')
                                                                                            _             -> Mult e1 (multNorm $ Mult e1' e2')
                                                                    else if res == GT then
                                                                        case e1' of 
                                                                            (Exp (Mult e111' e112') e12') -> multNorm $ Mult (e2') $ Mult (e1) $ expandExp e1'
                                                                            (Exp e11' e12') -> case e1 of
                                                                                            (Exp (Mult _ _) e12) -> multNorm $ Mult e2' $ Mult e1' $ expandExp e1
                                                                                            (Exp e11 e12) -> let
                                                                                                        res = compare e11 e11'
                                                                                                        in case res of
                                                                                                            EQ -> multNorm $ Mult (expNorm $ Exp e11' (addNorm $ Add e12' e12)) e2'
                                                                                                            _  -> Mult e1' (multNorm $ Mult e1 e2')
                                                                                            _             -> let
                                                                                                        res = compare e1 e11'
                                                                                                        in case res of
                                                                                                            EQ -> multNorm $ Mult (expNorm $ Exp e11' (addNorm $ Add e12' $ Const 1)) e2'
                                                                                                            _  -> Mult e1' (multNorm $ Mult e1 e2')
                                                                            (Const a)       -> case e1 of
                                                                                            (Exp (Mult _ _) e12) -> multNorm $ Mult e2' $ Mult e1' $ expandExp e1
                                                                                            (Exp e11 e12) -> let
                                                                                                        res = compare e11 e1'
                                                                                                        in case res of
                                                                                                            EQ  -> multNorm $ Mult (expNorm $ Exp e11 (addNorm $ Add e12 $ Const 1)) e2'
                                                                                                            _   -> Mult e1' (multNorm $ Mult e1 e2')
                                                                                            (Const b)     -> Mult (Const (a*b)) e2'
                                                                                            _             -> Mult e1' (multNorm $ Mult e1 e2')
                                                                            _               -> case e1 of
                                                                                            (Exp (Mult _ _) e12) -> multNorm $ Mult e2' $ Mult e1' $ expandExp e1
                                                                                            (Exp e11 e12) -> let
                                                                                                        res = compare e11 e1'
                                                                                                        in case res of
                                                                                                            EQ  -> multNorm $ Mult (expNorm $ Exp e11 (addNorm $ Add e12 $ Const 1)) e2'
                                                                                                            _   -> Mult e1' (multNorm $ Mult e1 e2')
                                                                                            _             -> Mult e1' (multNorm $ Mult e1 e2')
                                                                    else multNorm (Mult (expNorm $ Exp e1 (Const 2)) e2')
                                                        m1             -> multNorm $ Mult e1 m1 
                        (Const a, Const b)           -> Const (a*b)
                        (e1_, e2_)                     -> let
                                                        e1 = expNorm e1_
                                                        e2 = expNorm e2_
                                                        res = compare e1 e2
                                                    in case res of 
                                                        EQ -> expNorm $ Exp e1 $ Const 2
                                                        e  -> case e1 of 
                                                            (Exp (Mult e111 e112) e12) -> multNorm $ Mult e2 $ expandExp e1
                                                            (Exp e11 e12) -> case e2 of 
                                                                        (Exp (Mult e211 e212) e22) -> multNorm $ Mult e1 $ expandExp e2
                                                                        (Exp e21 e22) -> if compare e11 e21 == EQ then expNorm $ Exp e11 $ addNorm $ Add e12 e22 else if res == GT then Mult e2 e1 else Mult e1 e2
                                                                        _             -> if compare e11 e2 == EQ then expNorm $ Exp e11 $ addNorm $ Add e12 $ Const 1 else if res == GT then Mult e2 e1 else Mult e1 e2
                                                            _             -> case e2 of 
                                                                        (Exp (Mult e211 e212) e22) -> multNorm $ Mult e1 $ expandExp e2
                                                                        (Exp e21 e22) -> if compare e21 e1 == EQ then expNorm $ Exp e21 $ addNorm $ Add e22 $ Const 1 else if res == GT then Mult e2 e1 else Mult e1 e2
                                                                        _             -> if res == GT then Mult e2 e1 else Mult e1 e2
multNorm e            = e -- Do nothing for expressions that are not multiplication 

-- | Expands an exponent with multiplication inside to have multiplication outside
expandExp :: (Num a,Ord a) => Expr a -> Expr a
expandExp (Exp (Mult e11 e12) e2) = Mult (expandExp $ expNorm $ Exp e11 e2) (expandExp $ expNorm $ Exp e12 e2)
expandExp (Exp e1 e2)             = Exp e1 e2
expandExp e                       = e


-- | Converts a list of expressions into a multiplication expression, fails if given an empty list
fromListMult :: [Expr a] -> Expr a
fromListMult (e:[]) = e
fromListMult (e:es) = Mult e $ fromListMult es
fromListMult [] = error "List cannot be empty"

-- | Converts a multiplication Expression 'Mult' into a list of its components
toListMult :: Expr a -> [Expr a]
toListMult (Mult e1 e2) = (toListMult e1) ++ (toListMult e2)
toListMult e            = [e]

{- | Normalize a multiplication Expression
    Always converts a Mult Expression to another Mult Expression or Const Expression in normalized form.
    This is a normalization function using lists, use this to understand tha basics of how this multiplication normalization works 'multNorm' is basically this but w/o lists
    
-}
multNorml :: (Ord a,Num a) => [Expr a] -> [Expr a]
multNorml [] = []
multNorml l = let
            e1_:es = sort l -- seriously why does elm use :: instead of : for list comprehension, literally wrote this section initially using :: instead of : cause of Elm
            e1 = expNorm e1_
            es' = multNorml es
            in case es' of 
                [] -> case e1 of
                    (Exp (Mult _ _) e12) -> multNorml $ (toListMult (expandExp e1))
                    _                    -> [e1]
                (e2_:es) -> let
                    e2 = expNorm e2_
                  in case (e1,e2) of
                    (Const a, Const b)           -> (Const (a*b)):es
                    (Exp (Mult e111 e112) e12,_) -> multNorml $ (toListMult $ expandExp e1)++(e2:es)
                    (Exp e11 e12, Exp e21 e22)   -> case compare e11 e21 of
                                                    EQ -> multNorml $ (expNorm $ Exp e11 $ addNorm $ Add e12 e22):es
                                                    _  -> e1:(multNorml (e2:es))
                    (Exp e11 e12, _)             -> case compare e11 e2 of 
                                                    EQ -> multNorml $ (expNorm $ Exp e11 $ addNorm $ Add e12 $ Const 1):es
                                                    _  -> e1:(multNorml (e2:es))
                    (_,Exp e21 e22)              -> case compare e21 e1 of
                                                    EQ -> multNorml $ (expNorm $ Exp e21 $ addNorm $ Add e22 $ Const 1):es
                                                    _  -> e1:(multNorml (e2:es))
                    (_,_)                        -> case compare e1 e2 of
                                                    EQ -> multNorml $ (expNorm $ Exp e1 $ Const 2):es
                                                    _  -> e1:(multNorml (e2:es))

expNorm :: (Num a,Ord a) => Expr a -> Expr a
expNorm (Exp e1 e2) = case e1 of 
                        (Exp e21 e22)  -> expNorm $ Exp e21 (multNorm $ Mult e2 e22)
                        _              -> Exp e1 e2
expNorm e            = e

{- | Normalize an addition Expression
    Always converts a Add Expression to antoher Add expression in normalized form.
    Note : a pure recursive normalization cannot (at least w/o using a ton of helper functions that are essentially lists) fully normalize an expression
-}
addNorm :: (Ord a,Num a) => Expr a -> Expr a
addNorm (Add e1 e2) = case (e1,e2) of
                        (Add e11 e12, Add e21 e22) -> let
                                                    a1 = addNorm $ Add e11 e12
                                                    a2 = addNorm $ Add e21 e22
                                                    in case (a1,a2) of 
                                                        (Add e11 e12, Add e21 e22) -> let
                                                                                    res = compare e11 e21
                                                                                    in if res == GT then
                                                                                        (Add e21 (addNorm $ Add e11 (Add e12 e22)))
                                                                                    else if res == LT then
                                                                                        (Add e11 (addNorm $ Add e21 (Add e12 e22)))
                                                                                    else 
                                                                                        addNorm $ Add (multNorm $ Mult e11 $ Const 2) (Add e12 e22)
                                                        (a1,a2)                    -> addNorm $ Add a1 a2
                        (Add e11 e12, e2)          -> case (addNorm $ Add e11 e12) of
                                                        (Add e1' e2')  -> let 
                                                                        res = compare e1' e2
                                                                        in if res == LT then
                                                                            Add e2 (addNorm $ Add e1' e2')
                                                                        else if res == GT then
                                                                            Add e1' (addNorm $ Add e2 e2')
                                                                        else addNorm (Add (multNorm $ Mult e2 $ Const 2) e2')
                                                        a1             -> addNorm $ Add e2 a1
                        (e1, Add e21 e22)         -> case (addNorm $ Add e21 e22) of
                                                        (Add e1' e2')  -> let
                                                                        res = compare e1' e1
                                                                        in if res == LT then
                                                                            Add e1 (addNorm $ Add e1' e2')
                                                                        else if res == GT then
                                                                            Add e1' (addNorm $ Add e1 e2')
                                                                        else addNorm (Add (multNorm $ Mult e1 $ Const 2) e2')
                                                        a1             -> addNorm $ Add a1 e1
                        (Const a, Const b)         -> Const (a+b)
                        (e1,e2)                    -> let 
                                                    res = compare e1 e2
                                                    in if res == GT then
                                                        Add e2 e1
                                                    else if res == LT then
                                                        Add e1 e2
                                                    else 
                                                        multNorm $ Mult e1 $ Const 2
addNorm e           = e

-- | Converts a list of expressions into an addition expression, fails if given an empty list
fromListAdd :: [Expr a] -> Expr a
fromListAdd (e:[]) = e
fromListAdd (e:es) = Add e $ fromListAdd es
fromListAdd []     = error "List cannot be empty"

-- | Converts an addition Expression 'Add' into a list of its components
toListAdd :: Expr a -> [Expr a]
toListAdd (Add e1 e2) = (toListAdd e1)++(toListAdd e2)
toListAdd e           = [e]

addNorml :: [Expr a] -> [Expr a]
addNorml = undefined

