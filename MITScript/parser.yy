%code requires{

#include <iostream>
#include <string>
#define YY_DECL int yylex (YYSTYPE* yylval, YYLTYPE * yylloc, yyscan_t yyscanner)
#ifndef FLEX_SCANNER
#include "lexer.h"
#endif

using namespace std;

//The macro below is used by bison for error reporting
//it comes from stacck overflow
//http://stackoverflow.com/questions/656703/how-does-flex-support-bison-location-exactly
#define YY_USER_ACTION \
    yylloc->first_line = yylloc->last_line; \
    yylloc->first_column = yylloc->last_column; \
    for(int i = 0; yytext[i] != '\0'; i++) { \
        if(yytext[i] == '\n') { \
            yylloc->last_line++; \
            yylloc->last_column = 0; \
        } \
        else { \
            yylloc->last_column++; \
        } \
    }


#include "AST.h"
//If you need additional header files, put them here.


}


%define api.pure full
%parse-param {yyscan_t yyscanner} {Statement*& out}
%lex-param {yyscan_t yyscanner}
%locations
%define parse.error verbose

%code provides{
YY_DECL;
int yyerror(YYLTYPE * yylloc, yyscan_t yyscanner, Statement*& out, const char* message);
}



//The union directive defines a union type that will be used to store
//the return values of all the parse rules. We have initialized for you
//with an intconst field that you can use to store an integer, and a
//stmt field with a pointer to a statement. Note that one limitation
//is that you can only use primitive types and pointers in the union.
%union {
	int intconst;
	Statement*   stmt;
//begin_student_code

	std::string* strconst;
	Expression*  expr;
	vector<string>* namelist;
	vector<pair<string, ptr<Expression> > >* fieldlist;
	vector<ptr<Statement> >* slist;
	vector<ptr<Expression> >* elist;
	pair<BinaryOperator, Expression* >* labeledExpression;
//end_student_code
}

//Below is where you define your tokens and their types.
//for example, we have defined for you a T_int token, with type intconst
//the type is the name of a field from the union above
%token<intconst> T_int
//begin_student_code
%token<strconst> T_string
%token<strconst> T_ident
%token T_then
%token T_else
%token T_global
%token T_if
%token T_while
%token T_return
%token T_fun
%token T_geq
%token T_leq
%token T_eq
%token T_none
%token T_true
%token T_false
//end_student_code

//Use the %type directive to specify the types of AST nodes produced by each production.
//For example, you will have a program non-terimnal in your grammar, and it will
//return a Statement*. As with tokens, the name of the type comes
//from the union defined earlier.

%type<stmt> Program
//begin_student_code
%type<slist> StatementList
%type<stmt> Statement
%type<stmt> Global
%type<stmt> Assignment
%type<stmt> CallStatement
%type<stmt> IfStatement
%type<stmt> MaybeElse
%type<stmt> WhileLoop
%type<stmt> Block
%type<stmt> Return
%type<expr> Expression
%type<expr> Function
%type<expr> Boolean
%type<namelist> NameList
%type<expr> BooleanRest
%type<expr> Conjunction
%type<expr> ConjunctionRest
%type<expr> BoolUnit
%type<expr> Predicate
%type<expr> Arithmetic
%type<labeledExpression> ArithmeticRest
%type<expr> Product
%type<labeledExpression> ProductRest
%type<expr> Unit
%type<expr> LHSorConstant
%type<expr> Constant
%type<expr> LHS
%type<expr> Call
%type<elist> ArgList
%type<elist> MaybeMore
%type<expr> Record
%type<fieldlist> FieldList
//end_student_code

%start Program

//You must also define any associativity directives that are necessary
//to resolve ambiguities and properly parse the code.
//begin_student_code
%nonassoc "then"
%nonassoc T_else

//end_student_code

%%

//Your grammar rules should be written here.

Program:
StatementList {
        $$ = // assign a new block to $$.
//begin_student_code
            new sBlock(*$1); delete $1;
//end_student_code
// and make sure the out variable is set, because that is what main is going to read.
out = $$; }

//begin_student_code
StatementList:
  %empty { $$ = new vector<ptr<Statement> >(); }
| Statement StatementList {
		auto list = $2;
		list->insert(list->begin(), shared_ptr<Statement>($1));
		$$ = list;
		}

Statement:
  Assignment
| CallStatement
| Global
| IfStatement
| WhileLoop
| Block
| Return

Global:
 T_global T_ident ';' {
	$$ = new sGlobal(*$2);
	delete $2;
	}

Assignment :
 LHS '=' Expression ';' {
	$$ = new sAssign($1, $3);
	}

CallStatement :
Call ';' {
	$$ = new sExpression($1);
}

IfStatement :
T_if '(' Expression ')' Statement MaybeElse
{
	$$ = new sIfThen($3, $5, $6);
}



MaybeElse:
 %empty %prec "then" { $$ = NULL; }
| T_else Statement   { $$ = $2; }


WhileLoop :
  T_while '(' Expression ')' Statement
{
  $$ = new sWhile($3, $5);

}

Block :
 '{' StatementList '}' { $$ = new sBlock(*$2); delete $2; }

Return :
 T_return Expression ';' {  $$ = new sReturn($2); }



Expression :
   Function
|  Boolean
|  Record


Function :
 T_fun '(' NameList ')' Block {
	$$ = new eFundecl(*$3, $5);
	delete $3;
}

NameList :
 %empty { $$ = new vector<string>(); }
| T_ident NameList {
	auto list = $2;
	list->insert(list->begin(), *$1);
	delete $1;
	$$ = list;
}

Boolean :
 BooleanRest Conjunction {
	if($1 == NULL){
		$$ = $2;
	}else{
		$$ = new eBinary($1, $2, OR);
	}
}

BooleanRest:
  %empty { $$ = NULL; }
|  Boolean  '|' { $$ = $1; }


Conjunction :
 ConjunctionRest BoolUnit {
	if($1 == NULL){
		$$ = $2;
	}else{
		$$ = new eBinary($1, $2, AND);
	}
}

ConjunctionRest:
   %empty { $$ = NULL; }
|   Conjunction '&' { $$ = $1; }


BoolUnit :
Predicate { $$ = $1; }
| '!' Predicate { $$ = new eUnary($2, NOT); }

Predicate:
Arithmetic {$$ = $1; }
| Arithmetic '<' Arithmetic { $$ = new eUnary(new eBinary($1, $3, GTE), NOT); }
| Arithmetic '>' Arithmetic  { $$ = new eBinary($1, $3, GT); }
| Arithmetic T_leq Arithmetic { $$ = new eUnary(new eBinary($1, $3, GT), NOT); }
| Arithmetic T_geq Arithmetic { $$ = new eBinary($1, $3, GTE); }
| Arithmetic T_eq Arithmetic  { $$ = new eBinary($1, $3, EQ); }

Arithmetic :
  ArithmeticRest Product  {
	if($1 == NULL){
		$$ = $2;
	}else{
		$$ = new eBinary($1->second, $2, $1->first);
		delete $1;
	}
}

ArithmeticRest:
  %empty { $$ = NULL; }
|  Arithmetic '+' { $$ = new pair<BinaryOperator, Expression* >(PLUS, $1); }
|  Arithmetic '-' { $$ = new pair<BinaryOperator, Expression* >(MINUS, $1); }

Product :
  ProductRest Unit{
	if($1 == NULL){
		$$ = $2;
	}else{
		$$ = new eBinary($1->second, $2, $1->first);
		delete $1;
	}
}


ProductRest:
  %empty {$$ = NULL; }
|  Product '*' { $$ = new pair<BinaryOperator, Expression* >(TIMES, $1); }
|  Product '/' { $$ = new pair<BinaryOperator, Expression* >(DIV, $1); }

Unit:
'-' LHSorConstant  { $$ = new eUnary($2, UMINUS); }
| LHSorConstant    { $$ = $1; }

LHSorConstant:
LHS
| Constant
| Call
| '(' Boolean ')' { $$ = $2; }

Constant:
T_int { $$ = new eNum($1); }
| T_string { $$ = new eString(*$1); delete $1; }
| T_none { $$ = new eNone(); }
| T_true { $$ = new eBconst(true); }
| T_false { $$ = new eBconst(false); }
LHS :
T_ident { $$ = new eVar(*$1); delete $1; }
| LHS '.' T_ident { $$ = new eField($1, *$3); delete $3; }
| LHS '[' Expression ']' { $$ = new eIndex($1, $3); }



Call :
 LHS '(' ArgList ')' {
	$$ = new eFuncall($1, *$3);
	delete $3;
}

ArgList :
%empty { $$ = new vector<ptr<Expression> >(); }
| Expression MaybeMore{
	auto list = $2;
	list->insert(list->begin(), shared_ptr<Expression>($1));
	$$ = list;
}

MaybeMore:
%empty{ $$ = new vector<ptr<Expression> >(); }
| ',' Expression MaybeMore{
	auto list = $3;
	list->insert(list->begin(), shared_ptr<Expression>($2));
	$$ = list;
}

Record : '{' FieldList '}' {
	$$ = new eRecordConstructor(*$2);
	delete $2;
}
FieldList :
 %empty { $$ = new vector<pair<string, ptr<Expression> > >(); }
|  T_ident ':' Expression ';' FieldList {
	auto fields = $5;
	fields->insert(fields->begin(), make_pair(*$1, shared_ptr<Expression>($3)));
	delete $1;
	$$ = fields;
}


//end_student_code
%%

// Error reporting function. You should not have to modify this.
int yyerror(YYLTYPE * yylloc, void* p, Statement*& out, const char*  msg){

  cout<<"Error in line "<<yylloc->last_line<<", col "<<yylloc->last_column<<": "<<msg;
  return 0;
}

