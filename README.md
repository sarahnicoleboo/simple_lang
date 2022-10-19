simple expression language we're using:

x is a variable
i is an integer

type ::= int | bool </br >
primary_exp ::= x | i | '(' exp ')' </br >
additive_op ::= + | - </br >
additive_exp ::= primary_exp (additive_op primary_exp)* </br >
comparison_op ::= < | > </br >
comparison_exp ::= additive_exp (comparison_op additive_exp)* </br >
ex_equals_exp ::= comparison_exp ('==' comparison_exp)* </br >
exp ::= ex_equals_exp </br >
vardec ::= type x </br >
stmt ::= vardec = exp; | if (exp) stmt else stmt | { stmt* } </br >
program ::= stmt