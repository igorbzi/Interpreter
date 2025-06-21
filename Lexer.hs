module Lexer where

import Data.Char ( isAlpha, isDigit, isSpace )

data Expr = BTrue 
          | BFalse
          | Num Int 
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Abs Expr
          | And Expr Expr
          | Or Expr Expr
          | If Expr Expr Expr
          | GrT Expr Expr
          | LEq Expr Expr
          | Eq Expr Expr
          | Not Expr
          | Var String
          | Lam String Ty Expr
          | App Expr Expr
          | Paren Expr
          | Let String Expr Expr
          | Tuple [Expr]
          | Proj Expr Int
        deriving Show 

data Ty = TBool
        | TNum
        | TFun Ty Ty
        | TTuple [Ty]
        deriving (Show, Eq)

data Token = TokenTrue
           | TokenFalse
           | TokenNum Int
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenDiv
           | TokenAbs
           | TokenAnd
           | TokenOr
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenNot
           | TokenGrT
           | TokenLEq
           | TokenEq
           | TokenVar String
           | TokenLam
           | TokenArrow
           | TokenColon
           | TokenTNum
           | TokenTBool
           | TokenLParen
           | TokenRParen
           | TokenLet
           | TokenIn
           | TokenAtt
           | TokenLBrace   
           | TokenRBrace       
           | TokenComma            
           | TokenDot
           deriving Show


lexer :: String -> [Token]
lexer [] = []
lexer ('+' : cs)        = TokenAdd    : lexer cs
lexer ('-':'>':cs)      = TokenArrow  : lexer cs
lexer ('-' : cs)        = TokenSub    : lexer cs
lexer ('*' : cs)        = TokenMul    : lexer cs
lexer ('/' : cs)        = TokenDiv    : lexer cs 
lexer ('\\': cs)        = TokenLam    : lexer cs
lexer (':' : cs)        = TokenColon  : lexer cs
lexer ('(' : cs)        = TokenLParen : lexer cs 
lexer (')' : cs)        = TokenRParen : lexer cs 
lexer ('{' : cs)        = TokenLBrace : lexer cs  --Tokens novos
lexer ('}' : cs)        = TokenRBrace : lexer cs 
lexer (',' : cs)        = TokenComma  : lexer cs  
lexer ('.' : cs)        = TokenDot    : lexer cs  
lexer ('&' : '&' : cs)  = TokenAnd    : lexer cs
lexer ('|' : '|' : cs)  = TokenOr     : lexer cs
lexer ('>' : cs)        = TokenGrT    : lexer cs
lexer ('<' : '=' : cs)  = TokenLEq    : lexer cs
lexer ('=' : '=' : cs)  = TokenEq     : lexer cs
lexer ('=' : cs)        = TokenAtt    : lexer cs
lexer (c : cs) 
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | isAlpha c = lexKW (c:cs)

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of 
            ("true", rest)    -> TokenTrue : lexer rest
            ("false", rest)   -> TokenFalse : lexer rest
            ("if", rest)      -> TokenIf : lexer rest
            ("then", rest)    -> TokenThen : lexer rest
            ("else", rest)    -> TokenElse : lexer rest
            ("abs", rest)      -> TokenAbs : lexer rest
            ("not", rest)     -> TokenNot : lexer rest
            ("int", rest)     -> TokenTNum : lexer rest
            ("bool", rest)    -> TokenTBool : lexer rest
            ("let", rest)     -> TokenLet : lexer rest  --Tokens novos
            ("in", rest)     -> TokenIn : lexer rest
            (var, rest)     ->  TokenVar var : lexer rest    
