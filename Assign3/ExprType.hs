{-# LANGUAGE DeriveGeneric #-}
{-|
Module : ExprNorm
Description: Declares the 'Expr' type
Copyright: (c) chenc118 @ 2018
License : WTFPL
Stability : experimental
Portability : MSDOS
-}
module ExprType where

import Data.List
import Generic.Random
import GHC.Generics
{-
    Expression data type
    -------------------
    Wraps different Operations in an Expression tree
    Ops:
        Add - Standard binary addition
        Mult - Standard binary multiplication
        Const - Wrapper for simple values
        Var = String identifier for variables
-}
-- * Section: Datatype Declaration
-- | a datatype encoding numeric expressions 
data Expr a = Add (Expr a) (Expr a) -- ^ Binary Addition 
            | Mult (Expr a) (Expr a) -- ^ Binary Multiplication
            | Cos (Expr a)          -- ^ cosine function
            | Sin (Expr a)          -- ^ sine function
            | NExp (Expr a)         -- ^ Natural Exponentiation e^(Expr a)            
            | Ln (Expr a)           -- ^ Natural Logarithm
            | Exp (Expr a) (Expr a) -- ^ Exponentiate a function using where Exp a b = a ^ b
            | Const a               -- ^ Wrap a constant value
            | Var String            -- ^ Wrap a variable identifier
            deriving (Eq,Generic)
--Library provides default values for e and pi 

{-getVars :
        Retrieves variable identifiers from an Expr
-}
-- * Section: Aux values 
-- | Gets the variables within a given expression as a list of strings
getVars :: Expr a-> [String]
getVars (Add e1 e2)  = (getVars e1) `union` (getVars e2)
getVars (Mult e1 e2) = (getVars e1) `union` (getVars e2)
getVars (Cos e1)     = getVars e1
getVars (Sin e1)     = getVars e1
getVars (NExp e1)    = getVars e1
getVars (Ln e1)      = getVars e1
getVars (Exp e1 e2)  = (getVars e1) `union` (getVars e2)
getVars (Const _)    = []
getVars (Var s)      = [s]


