{
module Parser where

import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token

    num           { TokenNum $$ }
    true          { TokenTrue }
    false         { TokenFalse }
    '+'           { TokenAdd }
    '-'           { TokenSub }
    '*'           { TokenMul }
    '/'           { TokenDiv }
    "&&"          { TokenAnd }
    "||"          { TokenOr }
    '>'           { TokenGrT }
    "<="          { TokenLEq }
    if            { TokenIf }
    then          { TokenThen }
    else          { TokenElse }
    not           { TokenNot }
    var           { TokenVar $$ }
    '\\'          { TokenLam }
    ':'           { TokenColon }
    "->"          { TokenArrow }
    '('           { TokenLParen }
    ')'           { TokenRParen }
    '='           { TokenAtt }
    int           { TokenTNum }
    bool          { TokenTBool }
    let           { TokenLet }
    in            { TokenIn }


%nonassoc if then else int bool var num true false let in
%nonassoc '\\' "->" '(' ')' ':'
%left '>' "<=" 
%left '+' '-'
%left '*' '/'
%left "&&" "||"

%%

Exp : num                           { Num $1 }
    | true                          { BTrue }
    | false                         { BFalse }
    | var                           { Var $1 }
    | Exp '+' Exp                   { Add $1 $3 }
    | Exp '-' Exp                   { Sub $1 $3 }
    | Exp '*' Exp                   { Mul $1 $3 }
    | Exp '/' Exp                   { Div $1 $3 }
    | Exp "&&" Exp                  { And $1 $3 }
    | Exp "||" Exp                  { Or $1 $3 }
    | Exp '>' Exp                   { GrT $1 $3 }
    | Exp "<=" Exp                  { LEq $1 $3 }
    | if Exp then Exp else Exp      { If $2 $4 $6 }
    | not Exp                       { Not $2}
    | '\\' var ':' Type "->" Exp    { Lam $2 $4 $6 }
    | Exp Exp                       { App $1 $2}
    | '(' Exp ')'                   { Paren $2 }
    | let var '=' Exp in Exp        { Let $2 $4 $6 }

Type  : int                           { TNum }
      | bool                          { TBool }
      | '(' Type "->" Type ')'        { TFun $2 $4}

{
  
parseError :: [Token] -> a
parseError _ = error "ERRO: sintaxe incorreta"

}