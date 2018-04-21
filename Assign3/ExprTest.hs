{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module : ExprTest
Description: A class containing various test cases for the code
Copyright: (c) chenc118 @ 2018
License : WTFPL
Stability : experimental
Portability : MSDOS
-}
module ExprTest where

import ExprType
import ExprDiff
import ExprParser
import ExprPretty
import ExprNorm
import ExprUtil

import qualified  Data.Map as Map
import Generic.Random
import GHC.Generics
import Data.List

import Test.QuickCheck

-- | Random sample expression \[x + y\]
sampleExpr :: Expr Double
sampleExpr = (var "x") !+ (var "y")

-- | Generates all possible ways to distribute a list of either a Mult or Add expression (turns out there's a lot of different ways) Approximate complexity formula ~= 2*(n!)
genBranches :: Eq a => (Expr a -> Expr a -> Expr a) -> [Expr a] -> [Expr a]
genBranches expr (e:[])  = [e]
genBranches expr (e:es)  = let
        (a,b) = unzip $ splits (e:es) -- get all possible ways to split a list in 2 non-zero lists
        ab_ = zip [genBranches expr a' | a' <- a] [genBranches expr b' | b' <- b] -- generate all possible ways each sub tree may be arranged
        ba_ = zip [genBranches expr b' | b' <- b] [genBranches expr a' | a' <- a] -- same as above except swap left w/ right
    in nub $ flatMap id [[ expr a''' b''' | a''' <- a'' , b''' <- b'' ] | (a'',b'') <- (ab_++ba_) ] -- construct the list of expressions with all the generated stuff

-- | Verifies that an addition expression will normalize the same way no matter how the binary Add tree may be distributed
verifyAddNormality :: (Show a,Ord a,Num a) => Expr a -> Bool
verifyAddNormality ex = verifyNormality addNorm Add toListAdd ex

-- | Verifies that a multiplication expression will normalize the same way no matter how the binary Mult tree may be distributed
verifyMultNormality ::(Show a, Ord a, Num a) => Expr a -> Bool
verifyMultNormality ex = verifyNormality multNorm Mult toListMult ex

-- | verifies that a multiplication expression will normalize the same way no matter how the binary Mult tree may be distributed
verifyAddNormlality :: (Show a, Ord a, Num a) => Expr a -> Bool
verifyAddNormlality ex = verifyNormality (fromListMult . multNorml . toListMult) Mult toListMult ex

-- | Generic verify that some expression with a binary tree structure will normalize to the same thing
verifyNormality :: (Show a, Ord a, Num a) => (Expr a -> Expr a) -> (Expr a -> Expr a -> Expr a) -> (Expr a -> [Expr a]) -> Expr a -> Bool
verifyNormality norm expr toList test = let
                        normal = (norm test)
                        in vnHelper (genBranches expr (toList test)) norm (norm test)
-- | Helper for the verify normality functions, spits a verbose error detailing edge case if it finds something wrong
vnHelper :: (Show a,Eq a) => [Expr a] ->(Expr a -> Expr a) -> Expr a -> Bool
vnHelper (e:es) norm normal = case (norm e)==normal of
                        True -> vnHelper es norm normal
                        False -> error ("Normality check failed on "++(show e)++" got "++(show $ norm e)++" expected "++(show normal))
vnHelper [] _ _             = True

-- | A question on the 1ZB3 Exam, copied after having to take derivative multiple times in order to verify it (gets very messy)
mathExamQuestion :: Expr Double
mathExamQuestion = (((var "v") !+ (var "u")) !* (Exp ((Var "u") !* (Exp (Var "v") (Const 2.0))) (Const $ -1.0)))


{- | Question = d^2g(u,v)\/dudv where g (u,v) = (u+v) \/ (uv^2) where u = 1, v = 3,
    answer found = 1\/9 (was C or D on version 4) (forgot if it was -1\/9 or 1\/9, w\/e was on the Exam, there was only 1 Answer w\/ 1\/9 as absolute value)
    (Assuming 1\/9 cause that's what this program got) 
    (If I got the values of u and v confused still doesn't matter for some reason swapping the two gets 1\/9 as well)-}
verifyExamQuestion :: Bool
verifyExamQuestion =let 
                    ans = eval (Map.fromList [("u",1.0),("v",3.0)]) $ partDiff "u" $ partDiff "v" mathExamQuestion
                    rep = (1/9)
                    in abs (rep - ans) < 0.00001

-- copied ~ < 1hour after Math Exam
-- eval (Map.fromList [("u",1.0),("v",3.0)]) $ simplify (Map.fromList []) $ partDiff "u" $ partDiff "v" (((var "v") !+ (var "u")) !* (Exp ((Var "u") !* (Exp (Var "v") (Const 2.0))) (Const $ -1.0)))


simplifyProp :: Expr Double -> Bool
simplifyProp expr = let
            s1 = (usimplify $ expr) 
            s2 = (usimplify $ usimplify expr)
            in if isNan s1 then isNan s2 else s1==s2

isNan :: (RealFloat a) => Expr a -> Bool
isNan (Const a)     = isNaN a
isNan (Var _)       = False
isNan (Mult e1 e2)  = isNan e1 || isNan e2
isNan (Add e1 e2)   = isNan e1 || isNan e2
isNan (Exp e1 e2)   = isNan e1 || isNan e2
isNan (Ln e1)       = isNan e1
isNan (NExp e1)     = isNan e1
isNan (Cos e1)      = isNan e1
isNan (Sin e1)      = isNan e1



instance Arbitrary (Expr Double) where 
  arbitrary = genericArbitraryRec uniform `withBaseCase` return (Const 1)

instance Arbitrary (Expr Float) where
    arbitrary = genericArbitraryRec uniform `withBaseCase` return (Const 1)

instance Arbitrary (Expr Integer) where
    arbitrary = genericArbitraryRec uniform `withBaseCase` return (Const 1)

instance Arbitrary (Expr Int) where
    arbitrary = genericArbitraryRec uniform `withBaseCase` return (Const 1)

