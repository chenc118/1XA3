{-# LANGUAGE ParallelListComp #-}
module ExprTest where

import ExprType
import ExprDiff
import ExprParser
import ExprPretty
import ExprNorm

import qualified  Data.Map as Map
import Data.List

import Test.QuickCheck


sampleExpr :: Expr Double
sampleExpr = (var "x") !+ (var "y")

--exprProp :: Expr Double -> Bool

-- | a function to split lists modified slightly from <https://stackoverflow.com/a/22594891>
splits :: [a] -> [([a],[a])]
splits xx = zipWith splitAt [1..((length xx)-1)] (repeat xx)

-- | a function to flat map stuff taken from <https://stackoverflow.com/questions/2986787/haskell-flatmap>
flatMap :: (t -> [a]) -> [t] -> [a]
flatMap _ [] = []
flatMap f (x:xs) = f x ++ flatMap f xs


-- | Generates all possible ways to distribute a list of either a Mult or Add expression (turns out there's a lot of different ways) Approximate complexity formula ~= 2*(n!)
genBranches :: Eq a => (Expr a -> Expr a -> Expr a) -> [Expr a] -> [Expr a]
genBranches expr (e:[])  = [e]
genBranches expr (e:es)  = let
        (a,b) = unzip $ splits (e:es)
        ab_ = zip [genBranches expr a' | a' <- a] [genBranches expr b' | b' <- b]
        ba_ = zip [genBranches expr b' | b' <- b] [genBranches expr a' | a' <- a]
    in nub $ flatMap id [[ expr a''' b''' | a''' <- a'' , b''' <- b'' ] | (a'',b'') <- (ab_++ba_) ]

verifyAddNormality :: (Show a,Ord a,Num a) => Expr a -> Bool
verifyAddNormality ex = verifyNormality addNorm Add toListAdd ex

verifyMultNormality ::(Show a, Ord a, Num a) => Expr a -> Bool
verifyMultNormality ex = verifyNormality multNorm Mult toListMult ex

verifyNormality :: (Show a, Ord a, Num a) => (Expr a -> Expr a) -> (Expr a -> Expr a -> Expr a) -> (Expr a -> [Expr a]) -> Expr a -> Bool
verifyNormality norm expr toList test = let
                        normal = (norm test)
                        in vnHelper (genBranches expr (toList test)) norm (norm test)

vnHelper :: (Show a,Eq a) => [Expr a] ->(Expr a -> Expr a) -> Expr a -> Bool
vnHelper (e:es) norm normal = case (norm e)==normal of
                        True -> vnHelper es norm normal
                        False -> error ("Normality check failed on "++(show e)++" got "++(show $ norm e)++" expected "++(show normal))
vnHelper [] _ _             = True

listToExpr1 :: [Double] -> Expr Double
listToExpr1 xs = undefined

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
--  etc and run quickCheck over those


--instance Arbitrary (Expr a) where
