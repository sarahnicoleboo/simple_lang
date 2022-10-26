x is a variable literal </br >
i is an integer literal </br >
bool is a boolean value (true or false) </br >
str is a string literal </br >

type ::= int | bool | string</br >
primary_exp ::= x | i | bool | str </br >
additive_op ::= + | - </br >
additive_exp ::= primary_exp (additive_op primary_exp)* </br >
comparison_op ::= < | > </br >
comparison_exp ::= additive_exp (comparison_op additive_exp)* </br >
equals_op ::= == | !=
equals_exp ::= comparison_exp (equals_op comparison_exp)* </br >
exp ::= equals_exp </br >
stmt ::= type x = exp; | </br >
	 x = exp; | </br >
	 if (exp) stmt else stmt | </br >
	 while (exp) stmt | </br >
	 print(exp*) | </br >
	 { stmt* } | </br >
	 exp </br >
program ::= stmt