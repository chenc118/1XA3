module ExprTest where

import ExprType
import ExprDiff
import ExprParser
import ExprPretty

import qualified  Data.Map as Map

import Test.QuickCheck


sampleExpr :: Expr Double
sampleExpr = (var "x") !+ (var "y")

--exprProp :: Expr Double -> Bool

listToExpr1 :: [Double] -> Expr Double
listToExpr1 xs = undefined

--  etc and run quickCheck over those

instance Arbitrary (Expr a) where
