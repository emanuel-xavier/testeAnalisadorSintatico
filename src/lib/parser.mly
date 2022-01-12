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

%right ELSE IN
%nonassoc LT
%left PLUS

%%

(* write the missing production rules *)
program:
    | seq=nonempty_list(fundec) EOF { ($loc , Absyn.Program (seq)) }

fundec:
    | t=typeid LPAREN l=typeidlist RPAREN EQ e=exp { $loc , (t, l, e) }

typeid:
    | INT var=ID   { (Absyn.Int, ($loc, var)) }
    | BOOL var= ID { (Absyn.Bool, ($loc, var)) }

typeidlist:
    | l=separated_nonempty_list(COMMA, typeid) { l }

exp:
    | a=LITINT { $loc , Absyn.IntExp a }
    | a=ID { $loc , Absyn.VarExp a }
    | e1=exp op=operator e2=exp { $loc , Absyn.OpExp (op, e1, e2) }
    | IF e1=exp THEN e2=exp ELSE e3=exp { $loc , Absyn.IfExp (e1, e2, e3) }
    | a=ID LPAREN l=explist RPAREN { $loc , Absyn.CallExp (a, l) }
    | LET a=ID EQ e1=exp IN e2=exp { $loc , Absyn.LetExp (a, e1, e2) }

explist:
    | l=separated_nonempty_list(COMMA, exp) { l }

%inline operator:
    | PLUS { Absyn.Plus }
    | LT   { Absyn.LT }