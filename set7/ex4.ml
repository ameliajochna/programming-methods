(*
To the abstract and concrete syntax of the word in LET language from the lecture, add the construction ̨sum with the following concrete syntax:
sum x = n then m in k where sum,toiintokeywords,xtoidentifier,an,mictword ̇enia.
Extend the lexer and parser accordingly.
*)

(* AST: *)

| Sum of ident * expr * expr * expr

(* PARSER: *)

%token SUM
%token TO

expr:
| SUM; x = IDENT; EQ; e1 = expr; TO; e2 = expr; IN; e3 = {Sum(x, e1, e2, e3)}


(* LEXER: *)

| "sum" { SUM }
| "to" { TO }
