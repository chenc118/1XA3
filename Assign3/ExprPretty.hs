module ExprPretty () where

import ExprType

{- Instance Show Expr
	Provides a pretty representation of our datatype
	Matcing the DSL provded in DiffExpr
	
-}
parens :: String -> String
parens ss = "("++ss++")"


instance Show a => Show (Expr a) where
	show (Mult e1 e2) = parens $(show e1) ++":*"++ (show e2)
	show (Add e1 e2) = parens $(show e1) ++":+"++ (show e2)
	show (Var ss)  = parens $ "Var \""++ ss ++ "\""
	show (Const x) = parens $ "val "++show x
	show (Cos e1)  = parens $ "cos"++(show e1)
	show (Sin e1)  = parens $ "cos"++(show e1)
	show (Exp e1 a) = parens $ (show e1)"^"(a)