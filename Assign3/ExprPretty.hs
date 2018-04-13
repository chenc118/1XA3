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
    show (NExp e1) = parens $ "e^"++(show e1)
    show (Exp e1 e2) = parens $ (show e1)++"!^"++(show e2)