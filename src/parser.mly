%{
%}

%token <float> NUMBER
%token <string> SYMBOL
%token TRUE FALSE
%token LPAREN RPAREN
%token DOT
%token EOF

%start expr
%type <Type.t> expr

%%
expr:
    NUMBER { Type.Number($1) }
  | SYMBOL { Type.Symbol($1) }
  | TRUE { Type.Bool true }
  | FALSE { Type.Bool false }
  | LPAREN list RPAREN { $2 }

list:
    expr DOT expr { Type.Cons($1, $3) }
  | expr list { Type.Cons($1, $2) }
  | { Type.Nil }
