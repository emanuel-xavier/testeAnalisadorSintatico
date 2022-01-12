// parser.mly

%token                 EOF
%token <int>           LITINT
%token <Symbol.symbol> ID
%token                 PLUS
%token                 LT
%token                 EQ
%token                 COMMA
%token                 LPAREN
%token                 RPAREN
%token                 INT
%token                 BOOL
%token                 IF
%token                 THEN
%token                 ELSE
%token                 LET
%token                 IN

%start <Absyn.lprogram> program

%%

(* write the missing production rules *)

program:
| x=funs EOF { x }

exp:
| x=LITINT                       { $loc , Absyn.IntExp x           }
| x=ID                           { $loc , Absyn.VarExp x           }
| x=exp op=operator y=exp        { $loc , Absyn.OpExp (op, x, y)   }
| IF x=exp THEN y=exp ELSE z=exp { $loc , Absyn.IfExp (x, y, z)    }
| x=ID LPAREN f=exps RPAREN      { $loc , Absyn.CallExp (x, f)     }
| LET i=ID EQ f=exp IN ff=exp    { $loc , Absyn.LetExp (i, f, ff)  }  

exps:
| x=separated_nonempty_list(COMMA, exp) { x }

%inline operator:
| PLUS { Absyn.Plus }
| LT   { Absyn.LT }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc , (x, p, b) }

funs:
| x=fundecs { $loc, Absyn.Program x }

fundecs:
| x=nonempty_list(fundec) { x }

typeid:
| INT x=ID   { (Absyn.Int, x) }
| BOOL x= ID { (Absyn.Bool, x) }

typeids:
| x=separated_nonempty_list(COMMA, typeid) { x }
