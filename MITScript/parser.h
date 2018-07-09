/* A Bison parser, made by GNU Bison 3.0.5.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 1 "parser.yy" /* yacc.c:1916  */


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



#line 78 "parser.h" /* yacc.c:1916  */

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    T_int = 258,
    T_string = 259,
    T_ident = 260,
    T_then = 261,
    T_else = 262,
    T_global = 263,
    T_if = 264,
    T_while = 265,
    T_return = 266,
    T_fun = 267,
    T_geq = 268,
    T_leq = 269,
    T_eq = 270,
    T_none = 271,
    T_true = 272,
    T_false = 273
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 54 "parser.yy" /* yacc.c:1916  */

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

#line 124 "parser.h" /* yacc.c:1916  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif



int yyparse (yyscan_t yyscanner, Statement*& out);
/* "%code provides" blocks.  */
#line 42 "parser.yy" /* yacc.c:1916  */

YY_DECL;
int yyerror(YYLTYPE * yylloc, yyscan_t yyscanner, Statement*& out, const char* message);

#line 155 "parser.h" /* yacc.c:1916  */

#endif /* !YY_YY_PARSER_H_INCLUDED  */
