simple expression language we're using:

x is a variable
i is an integer

type ::= int | bool
primary_exp ::= x | i | '(' exp ')'
additive_op ::= + | -
additive_exp ::= primary_exp (additive_op primary_exp)*
comparison_op ::= < | >
comparison_exp ::= additive_exp (comparison_op additive_exp)*
ex_equals_exp ::= comparison_exp ('==' comparison_exp)*
exp ::= ex_equals_exp
vardec ::= type x
stmt ::= vardec = exp; | if (exp) stmt else stmt | { stmt* }
program ::= stmt