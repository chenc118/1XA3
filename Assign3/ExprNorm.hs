{-|
Module : ExprNorm
Description: A class that contains some helper functions to normalize expressions
Copyright: (c) chenc118 @ 2018
License : WTFPL
Stability : experimental
Portability : MSDOS

Constains some helper functions used by 'simplify' in "ExprDiff".

Used to normalize expressions to a constant form that can be further reduced by 'simplify' easily
-}
module ExprNorm where

import ExprType
import ExprUtil

import Data.List

{- ExprNorm: Module that contains a bunch of instances and functions
   that help normalize an Expression. Used by the simplify function 
   in ExprDiff.hs
-}


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

-- ** Multiplication Normalization
{- | Normalize a multiplication Expression
    Always converts a Mult Expression to another Mult Expression or Const Expression in normalized form. (Note this isn't a proper normalized form in terms of 
    rewrite functions but normalizes it to a form that 'simplify' can normalize)
    This is a recursive normalization function, there is an alternate multiplication normalization function that uses lists.
-}
multNorm :: (Ord a,Num a)=> Expr a -- ^ An Expression of form ('Mult' e1 e2) any other form will return itself
                        -> Expr a  -- ^ A normalized form of ('Mult' e1 e2) or a Constant if it is a multiplication of constants else the input
{-  Hello person reading source code, welcome to a world of confusion, thanks to how many separate cases there are to consider, when normalizing a multiplication expression
    Just look at the list based normalization and it's basically the same thing but no lists, just constant recursions to itself.
    There are over 25 end points of which about 50% recurse back onto this function. See also <https://www.xkcd.com/1960/>
-}
multNorm (Mult e1_ e2_) = let 
                    e1 = lnNorm e1_
                    e2 = lnNorm e2_
                    in case (e1,e2) of 
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
multNorml :: (Ord a,Num a) => [Expr a] -- ^ list of expressions multiplied together, use 'toListMult' to convert from an @Expr a@ 
                            -> [Expr a] -- ^ list of expressions multiplied together, use 'fromListMult' to convert to an @Expr a@
multNorml [] = []
multNorml l = let
            e1_:es = sort l -- seriously why does elm use :: instead of : for list comprehension, literally wrote this section initially using :: instead of : cause of Elm
            e1 = lnNorm $ expNorm e1_
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

-- ** Exponent Normalization

-- | Normalize an exponent. Always converts an exponent to another exponent
expNorm :: (Num a,Ord a) => Expr a -- ^ Exp a b expression, any other types are ignored
                        -> Expr a -- ^ Normalized form of Exp expression
expNorm (Exp (Exp e11 e12) e2) = expNorm $ Exp e11 $ multNorm $ Mult e2 e12
expNorm (Exp e1 e2)            = Exp e1 e2
expNorm e                      = e
-- ** Addition Normalization

{- | Normalize an addition Expression
    Always converts a Add Expression to antoher Add expression in normalized form. See 'addNorml'
-}
addNorm :: (Ord a,Num a) => Expr a -> Expr a
addNorm e           = fromListAdd $ addNorml $ toListAdd e

-- | Converts a list of expressions into an addition expression, fails if given an empty list
fromListAdd :: [Expr a] -> Expr a
fromListAdd (e:[]) = e
fromListAdd (e:es) = Add e $ fromListAdd es
fromListAdd []     = error "List cannot be empty"

-- | Converts an addition Expression 'Add' into a list of its components
toListAdd :: Expr a -> [Expr a]
toListAdd (Add e1 e2) = (toListAdd e1)++(toListAdd e2)
toListAdd e           = [e]

{- | Normalize an addition Expression
     Always converts an Add Expression to another Add Expression or Const Expression in normalized form. (Note this isn't a proper normalized form in terms of 
    rewrite functions but normalizes it to a form that 'simplify' can normalize)
    Uses lists as that's the easiest way to normalize for addition.
-}
addNorml :: (Ord a,Num a) => [Expr a] -- ^ list of expressions that will be added together, use 'toListAdd' to convert from @Expr a@ 
                        -> [Expr a] -- ^ list of expressions that will be added together, use 'fromListAdd' to convert to @Expr a@
addNorml [] = []
addNorml l = let 
            addSort a_ b_ =let
                a = multNorm a_
                b = multNorm b_
                in case (a,b) of -- customized sort of the already customized ordering stuff, makes the mult "close" to its term
                            (Mult a1 a2,Mult b1 b2) -> case (a1,b1) of
                                                        (Const a,Const b) -> let 
                                                                            res = compare a2 b2
                                                                            in if res == EQ then compare a b else res
                                                        (Const a,_)       -> let
                                                                            res = compare a2 (Mult b1 b2)
                                                                            in if res == EQ then compare a 1 else res
                                                        (_,Const b)       -> let 
                                                                            res = compare (Mult a1 a2) b2
                                                                            in if res == EQ then compare 1 b else res
                                                        (_,_)             -> compare a b
                            (Mult a1 a2, _)         -> case a1 of
                                                        (Const a)  -> let
                                                                    res = compare a2 b
                                                                    in if res == EQ then GT else res
                                                        _          -> compare a b
                            (_,Mult b1 b2)          -> case b1 of
                                                        (Const b)  -> let
                                                                    res = compare a b2
                                                                    in if res == EQ then LT else res
                                                        _          -> compare a b
                            (_,_)                   -> compare a b

            e1_:es = sortBy addSort l
            e1  = multNorm e1_
            es' = addNorml es
            in case es' of 
                []      -> case e1 of 
                            (Mult _ _)  -> let
                                            (m1:m1s) = expandMult $ toListMult e1
                                            in if length m1s > 0 then addNorml $ (m1:m1s) else [m1]
                            _           -> [e1]
                (e2_:es) -> let 
                        e2 = multNorm e2_
                        in case (e1,e2) of 
                                (Const a,Const b)   -> (Const (a+b)):(addNorml es)
                                (Mult _ _,Mult _ _) -> let
                                                    (m1:m1s) = expandMult $ toListMult e1
                                                    (m2:m2s) = expandMult $ toListMult e2
                                                in if length m1s > 0 || length m2s > 0 then addNorml $ (m1:m1s)++(m2:m2s)++es 
                                                    else if almostEqual m1 m2 then addNorml $ (Mult (Const $ (mCoef m1)+(mCoef m2)) (mTerm m1)):(addNorml es)
                                                        else m1:(addNorml (m2:es))
                                (Mult _ _,_)        -> let
                                                    (m1:m1s) = expandMult $ toListMult e1
                                                    in if length m1s > 0 then addNorml $ (m1:m1s)++(e2:es)
                                                        else if almostEqual m1 e2 then addNorml $ (Mult (Const $ (mCoef m1)+(mCoef e2)) (mTerm m1)):(addNorml es)
                                                            else m1:(addNorml (e2:es))
                                (_ , Mult _ _)      -> let
                                                    (m2:m2s) = expandMult $ toListMult e2
                                                    in if length m2s > 0 then addNorml $ (e1:m2:m2s)++es
                                                        else if almostEqual e1 m2 then addNorml $ (Mult (Const $ (mCoef e1)+(mCoef m2)) (mTerm m2)):(addNorml es)
                                                            else e1:(addNorml (m2:es))
                                (_,_)               -> if almostEqual e1 e2 then addNorml $ (Mult (Const $ (mCoef e1)+(mCoef e2)) (mTerm e1)):(addNorml es) 
                                                       else e1:(addNorml (e2:es))

-- | A function whose name gives you no idea what the heck it's for (you always have to have nonsensical names in your code)
almostEqual :: (Ord a, Num a) => Expr a -> Expr a -> Bool
almostEqual a_ b_ = let
                    a = multNorm a_
                    b = multNorm b_
                    in case (a,b) of 
                        (Mult (Const a) e12,Mult (Const b) e22) -> if compare e12 e22 == EQ then True else False
                        (Mult (Const a) e12,_)                  -> if compare e12 b == EQ then True else False
                        (_,Mult (Const b) e22)                  -> if compare a e22 == EQ then True else False
                        (_,_)                                   -> if compare a b == EQ then True else False

-- | Extract coefficient from multipication term
--
-- For a multiplication expression this is either the constant value or 1.
-- For any other expressions this is 1
--
-- i.e. coef of \[5x\] is 5
--
-- >>> mCoef (Mult (Const 5) (Var "x"))
-- 5
mCoef :: (Ord a, Num a) => Expr a -- ^ Expression to extract coefficient from
                        -> a -- ^ Coefficient
mCoef (Mult e1 e2) = let 
                    m1 = multNorm (Mult e1 e2)
                    in case m1 of
                        (Mult (Const a) e2) -> a
                        _                   -> 1
mCoef _            = 1

-- | Extract Term from a multiplication term
--
-- For a mutliplication expression this is everything except the constants.
-- FOr any other expression this is itself
--
-- i.e. term of \[5x\] is x
--
-- >>> mTerm (Mult (Const 5) (Var "x"))
-- (var "x")
mTerm :: (Ord a, Num a) => Expr a -- ^ Expression to extrac the term from 
                        -> Expr a -- ^ Extracted term
mTerm (Mult e1 e2) = let 
                    m1 = multNorm (Mult e1 e2)
                    in case m1 of 
                        (Mult (Const a) e2) -> e2
                        _                   -> m1
mTerm e             = e


-- | Takes a list of expressions multiplied and returns a list of addition expanding any Add within the list of multiplication
expandMult :: (Eq a)=> [Expr a] -- ^ list of expressions that will be multiplied together, use 'toListMult' to convert from an @Expr a@ 
                    -> [Expr a] -- ^ list of expressions that will be added together, use 'fromListAdd' to convert to an @Expr a@ form
expandMult m  = let
            am x = case x of
                        (Add _ _) -> True
                        _         -> False 
            a  = find am m
            m' = case a of 
                Just a  -> delete a m
                Nothing -> m
            nn l = case l of 
                        [] -> [[]]
                        _  -> l
            in case a of
                Nothing -> case m' of
                            []    -> []
                            _     -> [fromListMult m']
                Just a  -> [ fromListMult (a':m'') | a' <- toListAdd a, m''<-(nn $ fmap toListMult $ expandMult m') ]

-- ** Ln Normalization

-- | Normalizes a Ln expresssion by expanding Multiplication within and bringing down the exponent for exponents within
lnNorm :: Expr a -> Expr a
lnNorm (Ln (Mult e11 e12)) = Add (lnNorm $ Ln e11) (lnNorm $ Ln e12)
lnNorm (Ln (Exp e11 e12))  = Mult (e12) (lnNorm $ Ln e11)
lnNorm (Ln e1)             = Ln e1
lnNorm e                   = e