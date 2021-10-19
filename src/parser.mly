%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE ANDAND BARBAR
%token LET IN EQ
%token RARROW FUN DFUN
%token REC
%token MATCH WITH NIL APPEND BAR
%token LBOX RBOX SEMI

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  // カリー化されたlet宣言の推論規則です。
  | LET x=ID e=LetParaExpr SEMISEMI { Decl (x, e) }
  // let rec宣言の推論規則です。
  | LET REC x1=ID EQ FUN x2=ID RARROW e=Expr SEMISEMI { RecDecl (x1, x2, e) }

Expr :
    e=OrExpr { e }

OrExpr :
  // ||の推論規則です。最も結合が弱く、また左結合であるためこのようになっています。
    l=OrExpr BARBAR r=AndExpr { BinOp (Barbar, l, r) }
  | e=AndExpr { e }

AndExpr :
  // &&の推論規則です。||の次に結合が弱く、また左結合であるためこのようになっています。
    l=AndExpr ANDAND r=LTExpr { BinOp (Andand, l, r) }
  | e=LTExpr { e }

LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=SyntacticExpr { BinOp (Mult, l, r) }
  | e=SyntacticExpr { e }

SyntacticExpr :
    e=IfExpr { e }
  // カリー化されたlet式、fun式はこの推論規則で導出されます。ただし、カリー化されたlet式は解釈時にfun式の解釈を利用しますので通常のlet式とは区別しています。
  | e=LetCurriedExpr { e }
  | e=LetExpr { e }
  | e=FunCurriedExpr { e }
  | e=DFunExpr { e }
  | e=MatchExpr { e }
  | e=ConsExpr { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

// カリー化されたlet式の推論規則です。元々はfunが連続して並ぶよう記述されるので、それと同様に引数は右結合になっています。
LetCurriedExpr :
    LET x=ID e1=LetParaExpr IN e2=Expr { LetExp (x, e1, e2) }

LetParaExpr :
    x=ID e=LetParaExpr { FunExp (x, e) }
  | x=ID EQ e=Expr { FunExp (x, e) }

LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }
    // let rec式の推論規則です。
  | LET REC x1=ID EQ FUN x2=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x1, x2, e1, e2) }

// カリー化されたfun式の推論規則です。元々の記法に合わせて引数は右結合にしています。
FunCurriedExpr :
    FUN e=FunParaExpr { e }

FunParaExpr :
    x=ID e=FunParaExpr { FunExp (x, e) }
  | x=ID RARROW e=Expr { FunExp (x, e) }

DFunExpr :
    DFUN x=ID RARROW e=Expr { DFunExp (x, e) }

MatchExpr :
    MATCH e1=Expr WITH NIL RARROW e2=Expr BAR x1=ID APPEND x2=ID RARROW e3=Expr { MatchExp (e1, e2, x1, x2, e3) }

ConsExpr :
    i=AppExpr APPEND e=ConsExpr { ConsExp (i, e) }
  | e=AppExpr { e }

AppExpr :
    e1=AppExpr e2=ListStartExpr { AppExp (e1, e2) }
  | e=ListStartExpr { e } 

ListStartExpr :
    LBOX e=ListExpr { e }
  | e=AExpr { e }

ListExpr :
    e1=ListStartExpr SEMI e2=ListExpr { ConsExp (e1, e2) }
  | e=ListStartExpr RBOX { ConsExp (e, NilExp) }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
  | NIL { NilExp }
