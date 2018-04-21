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
Note not a lot of testing has been done due to running out of time, most of the basic cases should be covered, and the code here contains some things to test things further.
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

-- | Sample tests (To make sure any changes don't break basic cases)

-- | Tests that \[x + x\] normalizes to \[2x\]
sampleAddNorm1 :: Bool
sampleAddNorm1 = sampleAdd ((val 2)!*(var "x")) (addNorm $ parseExprI "x!+x")

-- | Tests that \[(x+y)(x+z)\] normalizes to \[x^2 + xy + xz + yz\]
sampleAddNorm2 :: Bool
sampleAddNorm2 = sampleAdd (((Var "x")!^(val 2))!+(((Var "x")!*(Var "y"))!+(((Var "x")!*(Var "z"))!+((Var "y")!*(Var "z"))))) (addNorm $ parseExprI "(x!+y)!*(x!+z)")

-- | Tests that \[2xy + 2x(x+y)\] normalizes to \[5x^2+7xy\]
sampleAddNorm3 :: Bool
sampleAddNorm3 = sampleAdd (((val 5)!*((Var "x")!^(val 2)))!+((val 7)!*((Var "x")!*(Var "y")))) (addNorm $ parseExprI "(2!*x!*y)!+(5!*x!*(x!+y))")

-- | Tests that \[5ln(xy)+5+ln(x)\] normalizes to \[5 + 6ln(x) + 5ln(y)\]
sampleAddNorm4 :: Bool
sampleAddNorm4 = sampleAdd ((Var "x")!+(((val 6)!*(exLn(Var "x")))!+((val 5)!*(exLn(Var "y"))))) (addNorm $ parseExprI "ln(x!*y)!*5!+x!+lnx")

-- | Tests that \[98x(5+i+ln(y)+z)+xln(xy)\] normalizes to \[490x + 98ix + 98xz + xln(x) + 99xln(y)\]
sampleAddNorm5 :: Bool
sampleAddNorm5 = sampleAdd (((val 490)!*(Var "x"))!+(((val 98)!*((Var "i")!*(Var "x")))!+(((val 98)!*((Var "x")!*(Var "z")))!+(((Var "x")!*(exLn(Var "x")))!+((val 99)!*((Var "x")!*(exLn(Var "y")))))))) (addNorm $ parseExprI "98!*x!*(5!+i!+lny!+z)!+x!*ln(x!*y)")

-- | Tests that \[5 * x * cos (5) * x^2 \] normalizes to \[5x^3cos(5)\]
sampleMultNorm1 :: Bool
sampleMultNorm1 = sampleMult ((val 5)!*(((Var "x")!^(val 3))!*(exCos(val 5)))) (multNorm $ parseExprI "5!*x!*cos5!*x!^2")

-- | Tests that \[25xy*cos(x)*(x^{2^{2^{2^{2^2}}}})*(x^32)\] normalizes to \[25x^{65}y cos(x)\]
sampleMultNorm2 :: Bool
sampleMultNorm2 = sampleMult ((val 25)!*(((Var "x")!^(val 65))!*((Var "y")!*(exCos(Var "x"))))) (multNorm $ parseExprI "25!*x!*y!*cosx!*x!^2!^2!^2!^2!^2!*x!^32")

-- | Tests that \[ln(x^2)*4*x^3*x^4\] normalizes to \[8 x^7 ln(x)\]
sampleMultNorm3 :: Bool
sampleMultNorm3 = sampleMult ((val 8)!*(((Var "x")!^(val 7))!*(exLn(Var "x")))) (multNorm $ parseExprI "ln(x!^2)!*4!*x!^3!*x!^4")

-- | Tests all the SampleAddNorms
addNormGauntlet :: Bool
addNormGauntlet = sampleAddNorm1 && sampleAddNorm2 && sampleAddNorm3 && sampleAddNorm4 && sampleAddNorm5

-- | Tests all the SampleMultNorms
multNormGauntlet :: Bool
multNormGauntlet = sampleMultNorm1 && sampleMultNorm2 && sampleMultNorm3
-- * Various Stuff to help test things

-- | Basic testing of a sample add expression tests all possible branches
sampleAdd :: (Show a,Ord a,Num a) => Expr a -> Expr a -> Bool
sampleAdd simp reg = simp == reg && (verifyAddNormality simp)

-- | Basic testing of a sample mult expression tests all possible branches of both list and normal form
sampleMult :: (Show a,Ord a,Num a) => Expr a -> Expr a -> Bool
sampleMult simp reg = simp == reg && (verifyMultNormality simp) && (verifyMultNormlality simp)


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
verifyMultNormlality :: (Show a, Ord a, Num a) => Expr a -> Bool
verifyMultNormlality ex = verifyNormality (fromListMult . multNorml . toListMult) Mult toListMult ex

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

-- | Simplify property, once simplified you cannot simplify any further, Causes frequent stack overflows.
simplifyProp :: Expr Double -> Bool
simplifyProp expr = let
            s1 = (usimplify $ expr) 
            s2 = (usimplify $ usimplify expr)
            in if isNan s1 then isNan s2 else s1==s2

-- | Check if a constant in an expression is NaN, entire function will likely eval to NaN
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


-- | generic arbitrary double
instance Arbitrary (Expr Double) where 
  arbitrary = genericArbitraryRec uniform `withBaseCase` return (Const 1)
-- | generic arbitrary float
instance Arbitrary (Expr Float) where
    arbitrary = genericArbitraryRec uniform `withBaseCase` return (Const 1)
-- | generic arbitrary integer
instance Arbitrary (Expr Integer) where
    arbitrary = genericArbitraryRec uniform `withBaseCase` return (Const 1)
-- | generic arbitrary int
instance Arbitrary (Expr Int) where
    arbitrary = genericArbitraryRec uniform `withBaseCase` return (Const 1)

