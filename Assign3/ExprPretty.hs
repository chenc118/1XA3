{-|
Module : ExprNorm
Description: The show instance for 'Expr' types
Copyright: (c) chenc118 @ 2018
License : WTFPL
Stability : experimental
Portability : MSDOS

This class automatically shows expressions in a maybe slightly more readable format. The string shown fully respresents the expression in the DSL established in "ExprDiff".

__Conversions from code format to Show format__

a,b etc represent the show for expressions within


@Add a b  -> ((a)!+(b))@

@Mult a b -> ((a)!*(b))@

@Exp a b  -> ((a)!^(b))@

@Cos a    -> (exCos(a))@

@Sin a    -> (exSin(a))@

@NExp a   -> (e^(a))@

@Ln a     -> (exLn(a))@

@Const a  -> (val a)@

@Var a    -> (var \"a\")@
-}
module ExprPretty () where

import ExprType

{- Instance Show Expr
    Provides a pretty representation of our datatype
    Matcing the DSL provded in DiffExpr
    
-}
parens :: String -> String
parens ss = "("++ss++")"


instance Show a => Show (Expr a) where
    show (Mult e1 e2) = parens $(show e1) ++"!*"++ (show e2)
    show (Add e1 e2) = parens $(show e1) ++"!+"++ (show e2)
    show (Var ss)  = parens $ "Var \""++ ss ++ "\""
    show (Const x) = parens $ "val "++show x
    show (Cos e1)  = parens $ "exCos"++(show e1)
    show (Sin e1)  = parens $ "exSin"++(show e1)
    show (Ln e1)   = parens $ "exLn"++(show e1)
    show (NExp e1) = parens $ "exNExp"++(show e1)
    show (Exp e1 e2) = parens $ (show e1)++"!^"++(show e2)