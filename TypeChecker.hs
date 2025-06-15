module TypeChecker where 

import Lexer 

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty 
typeof ctx (Num _) = Just TNum 
typeof ctx BFalse = Just TBool 
typeof ctx BTrue = Just TBool 
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                       (Just TNum, Just TNum)   -> Just TNum 
                       _                        -> Nothing
typeof ctx (Sub e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                       (Just TNum, Just TNum)   -> Just TNum 
                       _                        -> Nothing
typeof ctx (Mul e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                       (Just TNum, Just TNum)   -> Just TNum 
                       _                        -> Nothing
typeof ctx (Div e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                       (Just TNum, Just TNum)   -> Just TNum 
                       _                        -> Nothing
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                       (Just TBool, Just TBool) -> Just TBool 
                       _                        -> Nothing 
typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                       (Just TBool, Just TBool) -> Just TBool 
                       _                        -> Nothing 
typeof ctx (GrT e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                       (Just TNum, Just TNum)   -> Just TBool 
                       _                        -> Nothing 
typeof ctx (LEq e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                       (Just TNum, Just TNum)   -> Just TBool
                       _                        -> Nothing                                                  
typeof ctx (Not e1) = case typeof ctx e1 of 
                       (Just TBool)             -> Just TBool 
                       _                        -> Nothing                  
typeof ctx (If e1 e2 e3) = 
    case typeof ctx e1 of 
      Just TBool -> case (typeof ctx e2, typeof ctx e3) of 
                      (Just t1, Just t2) | t1 == t2  -> Just t1  
                                         | otherwise -> Nothing 
                      _ -> Nothing 
      _ -> Nothing 
typeof ctx (Var x)     = lookup x ctx
typeof ctx (Lam x t1 b) = case typeof ((x, t1) : ctx) b of 
                          Just t2 -> Just (TFun t1 t2 )
                          _       -> Nothing
typeof ctx (App e1 e2) = 
  case typeof ctx e1 of
    Just (TFun t11 t12) -> case typeof ctx e2 of 
                        Just t2 | t11 == t2 -> Just t12
                                | otherwise -> Nothing
                        _ -> Nothing
    _ -> Nothing
typeof ctx (Paren e) = typeof ctx e                         


typecheck :: Expr -> Expr
typecheck e = case typeof [] e of
              Just _ -> e
              _      -> error "ERRO: Erro de tipo!"



