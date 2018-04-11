module ExprType where

import Data.List

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
            | Cos (Expr a)
            | Sin (Expr a)
            | NExp (Expr a)   -- ^ Natural Exponentiation e^(Expr a)            
            | Ln (Expr a) 
            | Exp (Expr a) (a) -- ^ Exponentiate a function using ^  -- really not sure how to approach this, TBD
            | Const a               -- ^ Wrap a constant value
            | Var String            -- ^ Wrap a variable identifier
            deriving Eq
--Library provides default values for e and pi 

{-getVars :
        Retrieves variable identifiers from an Expr
-}
-- * Section: Aux values 
getVars :: Expr a-> [String]
getVars (Add e1 e2)  = (getVars e1) `union` (getVars e2)
getVars (Mult e1 e2) = (getVars e1) `union` (getVars e2)
getVars (Cos e1)     = getVars e1
getVars (Sin e1)     = getVars e1
getVars (NExp e1)    = getVars e1
getVars (Ln e1)      = getVars e1
getVars (Const _)    = []
getVars (Var s)      = [s]


