module Interpreter where
import Lexer 

isValue :: Expr -> Bool
isValue BTrue     = True
isValue BFalse    = True
isValue (Num _)   = True
isValue (Lam _ _ _) = True
isValue _         = False
        
eval :: Expr -> Expr
eval e | isValue e    = e
       | otherwise    = eval (step e)

subst :: String -> Expr -> Expr -> Expr
subst v e BTrue       = BTrue
subst v e BFalse      = BFalse
subst v e (Num x)     = Num x
subst v e (Add e1 e2) = Add (subst v e e1) (subst v e e2)
subst v e (Sub e1 e2) = Sub (subst v e e1) (subst v e e2)
subst v e (Mul e1 e2) = Mul (subst v e e1) (subst v e e2)
subst v e (Div e1 e2) = Div (subst v e e1) (subst v e e2)
subst v e (And e1 e2) = And (subst v e e1) (subst v e e2)
subst v e (Or e1 e2)  = Or (subst v e e1) (subst v e e2)
subst v e (GrT e1 e2) = GrT (subst v e e1) (subst v e e2)
subst v e (LEq e1 e2) = LEq (subst v e e1) (subst v e e2)
subst v e (Not e1)    = Not (subst v e e1)
subst v e (If e1 e2 e3) = If (subst v e e1) (subst v e e2) (subst v e e3)
subst v e (Var x)     = if v == x then e else Var x
subst v e (Lam x t l) = Lam x t (subst v e l)
subst v e (App e1 e2) = App (subst v e e1) (subst v e e2)
subst v e (Paren e1)  = Paren (subst v e e1)
subst v e (Let x e1 e2) = Let x (subst v e e1) (subst v e e2)


step :: Expr -> Expr
step (Add (Num n1) (Num n2))  = Num (n1 + n2)
step (Add (Num n1) e2)        = Add (Num n1) (step e2)
step (Add e1 e2)              = Add (step e1) e2
step (Sub (Num n1) (Num n2))  = Num (n1 - n2)
step (Sub (Num n1) e2)        = Sub (Num n1) (step e2)
step (Sub e1 e2)              = Sub (step e1) e2
step (Mul (Num n1) (Num n2))  = Num (n1 * n2)
step (Mul (Num n1) e2)        = Mul (Num n1) (step e2)
step (Mul e1 e2)              = Mul (step e1) e2
step (Div (Num n1) (Num n2))  = Num (div n1 n2)
step (Div (Num n1) e2)        = Div (Num n1) (step e2)
step (Div e1 e2)              = Div (step e1) e2
step (And BTrue e2)           = e2
step (And BFalse e2)          = BFalse
step (And e1 e2)              = And (step e1) e2  
step (Or BTrue e2)            = BTrue
step (Or BFalse e2)           = e2
step (Or e1 e2)               = Or (step e1) e2
step (GrT (Num n1) (Num n2))  = if n1 > n2 then BTrue else BFalse
step (GrT (Num n1) e2)        = GrT (Num n1) (step e2)
step (GrT e1 e2)              = GrT (step e1) e2
step (LEq (Num n1) (Num n2))  = if n1 <= n2 then BTrue else BFalse
step (LEq (Num n1) e2)        = LEq (Num n1) (step e2)
step (LEq e1 e2)              = LEq (step e1) e2
step (Not BTrue)              = BFalse
step (Not BFalse)             = BTrue
step (Not e1)                 = Not (step e1)
step (If BTrue e2 e3)         = e2
step (If BFalse e2 e3)        = e3
step (If e1 e2 e3)            = If (step e1) e2 e3  
step (App e1@(Lam x t b) e2)  |  isValue e2 = subst x e2 b
                              |  otherwise  = App e1 (step e2)
step (App e1 e2)              = App (step e1) e2
step (Paren e)                = e
step (Let x e1 e2)            = subst x e1 e2