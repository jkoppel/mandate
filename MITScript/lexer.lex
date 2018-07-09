
%{

#include <string>
#include "parser.h"
using std::string;
// You can put additional header files here.

%}

%option reentrant
%option noyywrap
%option never-interactive

int_const [0-9][0-9]*

whitespace   ([ \t\n]*)
%{
// Initial declarations
// In this section of the file, you can define named regular expressions.
// like int_const and whitespace above
//begin_student_code
%}
name	[a-zA-Z_][a-zA-Z0-9_]*


string_const ("\""[^\n\"]*"\"")

Operator     ([\%\/\<\>\;\!\?\*\-\+\,\.\:\[\]\(\)\{\}\=\|\&\^\$])


comment      ("//"[^\n]*)
%{
//end_student_code
%}

%%


{whitespace}   { /* skip */ }

{comment}      { /* skip */ }


{int_const}    { 
		//Rule to identify an integer constant. 
		//The return value indicates the type of token;
		//in this case T_int as defined in parser.yy.
		//The actual value of the constant is returned
		//in the intconst field of yylval (defined in the union
		//type in parser.yy).
			yylval->intconst = atoi(yytext);
			return T_int;
		}

%{
// The rest of your lexical rules go here. 
// rules have the form 
// pattern action
// we have defined a few rules for you above, but you need
// to provide additional lexical rules for string constants, 
// operators, keywords and identifiers.
//begin_student_code
%}





{string_const}  {

			string*  tmp = new string(yytext);
			*tmp = tmp->substr(1, tmp->size() -2);
			yylval->strconst = tmp;
			return T_string;
		}



"None" 		{ return T_none; }
"true" 		{ return T_true; }
"false"		{ return T_false; }
"global"	{ return T_global; }
"if"		{ return T_if; }
"then"	{ return T_then; }
"else"	{ return T_else; }
"while"	{ return T_while; }
"return"	{ return T_return; }
"fun"		{ return T_fun; }
"<=" 		{ return T_leq; }
">="		{ return T_geq; }
"=="		{ return T_eq; }
{Operator} {  return yytext[0]; }

{name} 		{ 
			yylval->strconst = new std::string(yytext);
			return T_ident;
		}

%{
//end_student_code
%}

%%

