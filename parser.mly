/* Ocamlyacc parser for Serene */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN
%token EQ NEQ LT GT AND OR
%token LBRACK RBRACK
%token IF ELSE WHILE INTEGER BOOLEAN CHARACTER
%token FLOAT STRING MULTIPLY DIVIDE MODULUS LEQ GEQ NOT
%token FOR IN
%token NONE
%token FUNCTION ARROW
%token <string> FLIT
%token RETURN COMMA
%token <int> ILIT
%token <string> STLIT
%token <char> CHLIT
%token <bool> BLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program


%nonassoc
%right ASSIGN
%left AND OR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULUS
%right NOT

%nonassoc THEN
%nonassoc ELSE
%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list {  $1 :: $3 }

/* int x */
vdecl:
  typ ID { ($1, $2) }

typ:
    INTEGER { Integer }
  | BOOLEAN { Boolean }
  | FLOAT { Float }
  | STRING { String }
  | CHARACTER { Character }
  | typ LBRACK RBRACK {Arr($1,0)}

/* fdecl */
fvar:
  FUNCTION ID { $2 }

/* fdecl */
fdecl:
  fvar LPAREN formals_opt RPAREN ARROW typ LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=$6;
      fname= $1;
      formals=$3;
      locals=$8;
      body=$9
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1 } else { block2 } */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt %prec THEN  { If($3, $5) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { IfElse($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | FOR LPAREN expr COMMA expr COMMA expr RPAREN stmt { For($3, $5, $7, $9) }
  | RETURN expr SEMI                        { Return $2      }

expr:
    ILIT          { IntegerLit($1)            }
  | BLIT             { BooleanLit($1)            }
  | FLIT { FloatLit($1)}
  | STLIT { StringLit($1) }
  | CHLIT { CharacterLit($1) }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr MULTIPLY  expr { Binop($1, Multiply,   $3)   }
  | expr DIVIDE  expr { Binop($1, Divide,   $3)   }
  | expr MODULUS expr { Binop($1, Modulus,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Greater,  $3)   }
  | expr LEQ expr { Binop($1, LessOrEqual, $3) }
  | expr GEQ expr { Binop($1, GreaterOrEqual, $3) }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | NOT expr { Uniop (Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }
  /* arr functionality */
  | ID LBRACK expr RBRACK { ArrayAccess($1, $3) } /* x[0]; */
  | LBRACK args RBRACK { ArrayLit(List.rev $2) } /* [1,2,3,4,5]; */
  | ID LBRACK expr RBRACK ASSIGN expr { ArrAssign($1,$3,$6) } /* array[0] = 5; */
  /* none */
  | NONE { NonePtr(None) }
  /* var declaration and assignment */
  | ID typ ASSIGN expr { AssignToVar($1,$2,$4) } /* x integer = 5; */



/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
