/* A Bison parser, made by GNU Bison 3.0.5.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.5"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 2

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */

#line 67 "parser.cpp" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "parser.h".  */
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
#line 1 "parser.yy" /* yacc.c:355  */


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



#line 131 "parser.cpp" /* yacc.c:355  */

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
#line 54 "parser.yy" /* yacc.c:355  */

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

#line 177 "parser.cpp" /* yacc.c:355  */
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
#line 42 "parser.yy" /* yacc.c:355  */

YY_DECL;
int yyerror(YYLTYPE * yylloc, yyscan_t yyscanner, Statement*& out, const char* message);

#line 208 "parser.cpp" /* yacc.c:355  */

#endif /* !YY_YY_PARSER_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 214 "parser.cpp" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  30
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   100

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  40
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  34
/* YYNRULES -- Number of rules.  */
#define YYNRULES  70
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  115

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   274

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    28,     2,     2,     2,     2,    27,     2,
      22,    23,    33,    31,    38,    32,    35,    34,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    39,    20,
      29,    21,    30,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    36,     2,    37,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    24,    26,    25,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   147,   147,   157,   158,   165,   166,   167,   168,   169,
     170,   171,   174,   180,   185,   190,   198,   199,   203,   210,
     213,   218,   219,   220,   224,   230,   231,   239,   248,   249,
     253,   262,   263,   267,   268,   271,   272,   273,   274,   275,
     276,   279,   289,   290,   291,   294,   305,   306,   307,   310,
     311,   314,   315,   316,   317,   320,   321,   322,   323,   324,
     326,   327,   328,   333,   339,   340,   347,   348,   354,   359,
     360
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "T_int", "T_string", "T_ident", "T_then",
  "T_else", "T_global", "T_if", "T_while", "T_return", "T_fun", "T_geq",
  "T_leq", "T_eq", "T_none", "T_true", "T_false", "\"then\"", "';'", "'='",
  "'('", "')'", "'{'", "'}'", "'|'", "'&'", "'!'", "'<'", "'>'", "'+'",
  "'-'", "'*'", "'/'", "'.'", "'['", "']'", "','", "':'", "$accept",
  "Program", "StatementList", "Statement", "Global", "Assignment",
  "CallStatement", "IfStatement", "MaybeElse", "WhileLoop", "Block",
  "Return", "Expression", "Function", "NameList", "Boolean", "BooleanRest",
  "Conjunction", "ConjunctionRest", "BoolUnit", "Predicate", "Arithmetic",
  "ArithmeticRest", "Product", "ProductRest", "Unit", "LHSorConstant",
  "Constant", "LHS", "Call", "ArgList", "MaybeMore", "Record", "FieldList", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
      59,    61,    40,    41,   123,   125,   124,    38,    33,    60,
      62,    43,    45,    42,    47,    46,    91,    93,    44,    58
};
# endif

#define YYPACT_NINF -65

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-65)))

#define YYTABLE_NINF -65

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int8 yypact[] =
{
      40,   -65,     2,    -6,    14,    10,    40,    38,   -65,    40,
     -65,   -65,   -65,   -65,   -65,   -65,   -65,    31,    36,    41,
      10,    10,    43,    57,    48,   -65,    51,   -65,   -65,    44,
     -65,   -65,    10,    47,    73,    10,   -65,   -65,    56,    58,
      75,    45,    60,   -65,   -65,    55,    59,   -65,    63,    50,
      66,   -65,    49,    40,    40,    75,    67,    10,   -65,   -65,
     -65,   -65,   -65,    -5,   -65,   -65,    10,   -65,   -65,   -65,
      84,   -65,   -65,    68,    74,   -65,   -65,   -65,   -65,   -65,
     -65,   -65,   -65,    -2,     1,    50,    40,   -65,   -65,    57,
      23,    23,    23,    23,    23,   -65,   -65,   -65,   -65,   -65,
     -65,   -65,   -65,    25,   -65,   -65,   -65,    22,   -65,   -65,
     -65,   -65,   -12,   -65,   -65
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       3,    60,     0,     0,     0,    28,     3,     0,     2,     3,
       7,     5,     6,     8,     9,    10,    11,     0,     0,     0,
      28,    28,     0,    69,     0,    21,    22,    31,    23,     0,
       1,     4,    28,    28,     0,    28,    14,    12,     0,     0,
      25,     0,     0,    20,    29,    27,    42,    19,     0,    66,
       0,    61,     0,     0,     0,    25,     0,    28,    68,    32,
      42,    30,    33,    35,    46,    13,    28,    65,    63,    62,
      16,    18,    26,     0,     0,    34,    42,    42,    42,    42,
      42,    43,    44,    41,     0,    66,     0,    15,    24,    69,
      39,    38,    40,    36,    37,    47,    48,    55,    56,    57,
      58,    59,    28,     0,    45,    50,    52,    51,    53,    67,
      17,    70,     0,    49,    54
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -65,   -65,    54,   -51,   -65,   -65,   -65,   -65,   -65,   -65,
      20,   -65,   -20,   -65,    42,    -7,   -65,   -65,   -65,   -65,
      39,    -4,   -65,   -65,   -65,   -65,    -3,   -65,   -64,   -63,
     -65,    11,   -65,     9
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     7,     8,     9,    10,    11,    12,    13,    87,    14,
      15,    16,    24,    25,    56,    26,    27,    45,    46,    61,
      62,    63,    64,    83,    84,   104,   105,   106,    17,    18,
      50,    67,    28,    42
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      38,    39,    70,    71,    97,    98,     1,    19,    76,    77,
      78,   114,    48,    49,    44,    52,    20,    99,   100,   101,
     107,   108,    22,   102,    79,    80,    81,    82,    97,    98,
       1,    95,    96,   103,    23,   110,    21,    74,    30,   107,
     108,    99,   100,   101,    33,     1,    85,   102,     2,     3,
       4,     5,    32,    33,    81,    82,    36,    34,    35,    22,
      29,    37,    41,    31,     6,    40,    34,    35,    43,    47,
     -64,    23,    90,    91,    92,    93,    94,    44,    51,    53,
      55,    54,    59,    65,    57,    58,    69,    60,    66,    68,
      73,    86,     6,    88,    89,   112,   109,    72,   111,    75,
     113
};

static const yytype_uint8 yycheck[] =
{
      20,    21,    53,    54,     3,     4,     5,     5,    13,    14,
      15,    23,    32,    33,    26,    35,    22,    16,    17,    18,
      84,    84,    12,    22,    29,    30,    31,    32,     3,     4,
       5,    33,    34,    32,    24,    86,    22,    57,     0,   103,
     103,    16,    17,    18,    22,     5,    66,    22,     8,     9,
      10,    11,    21,    22,    31,    32,    20,    35,    36,    12,
       6,    20,     5,     9,    24,    22,    35,    36,    20,    25,
      23,    24,    76,    77,    78,    79,    80,    26,     5,    23,
       5,    23,    27,    20,    39,    25,    37,    28,    38,    23,
      23,     7,    24,    73,    20,   102,    85,    55,    89,    60,
     103
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     5,     8,     9,    10,    11,    24,    41,    42,    43,
      44,    45,    46,    47,    49,    50,    51,    68,    69,     5,
      22,    22,    12,    24,    52,    53,    55,    56,    72,    42,
       0,    42,    21,    22,    35,    36,    20,    20,    52,    52,
      22,     5,    73,    20,    26,    57,    58,    25,    52,    52,
      70,     5,    52,    23,    23,     5,    54,    39,    25,    27,
      28,    59,    60,    61,    62,    20,    38,    71,    23,    37,
      43,    43,    54,    23,    52,    60,    13,    14,    15,    29,
      30,    31,    32,    63,    64,    52,     7,    48,    50,    20,
      61,    61,    61,    61,    61,    33,    34,     3,     4,    16,
      17,    18,    22,    32,    65,    66,    67,    68,    69,    71,
      43,    73,    55,    66,    23
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    40,    41,    42,    42,    43,    43,    43,    43,    43,
      43,    43,    44,    45,    46,    47,    48,    48,    49,    50,
      51,    52,    52,    52,    53,    54,    54,    55,    56,    56,
      57,    58,    58,    59,    59,    60,    60,    60,    60,    60,
      60,    61,    62,    62,    62,    63,    64,    64,    64,    65,
      65,    66,    66,    66,    66,    67,    67,    67,    67,    67,
      68,    68,    68,    69,    70,    70,    71,    71,    72,    73,
      73
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     0,     2,     1,     1,     1,     1,     1,
       1,     1,     3,     4,     2,     6,     0,     2,     5,     3,
       3,     1,     1,     1,     5,     0,     2,     2,     0,     2,
       2,     0,     2,     1,     2,     1,     3,     3,     3,     3,
       3,     2,     0,     2,     2,     2,     0,     2,     2,     2,
       1,     1,     1,     1,     3,     1,     1,     1,     1,     1,
       1,     3,     4,     4,     0,     2,     0,     3,     3,     0,
       5
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (&yylloc, yyscanner, out, YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static unsigned
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  unsigned res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, Location, yyscanner, out); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t yyscanner, Statement*& out)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (yylocationp);
  YYUSE (yyscanner);
  YYUSE (out);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t yyscanner, Statement*& out)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp, yyscanner, out);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule, yyscan_t yyscanner, Statement*& out)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       , yyscanner, out);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, yyscanner, out); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, yyscan_t yyscanner, Statement*& out)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  YYUSE (yyscanner);
  YYUSE (out);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/*----------.
| yyparse.  |
`----------*/

int
yyparse (yyscan_t yyscanner, Statement*& out)
{
/* The lookahead symbol.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  yylsp[0] = yylloc;
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yyls1, yysize * sizeof (*yylsp),
                    &yystacksize);

        yyls = yyls1;
        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex (&yylval, &yylloc, yyscanner);
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 147 "parser.yy" /* yacc.c:1663  */
    {
        (yyval.stmt) = // assign a new block to $$.
//begin_student_code
            new sBlock(*(yyvsp[0].slist)); delete (yyvsp[0].slist);
//end_student_code
// and make sure the out variable is set, because that is what main is going to read.
out = (yyval.stmt); }
#line 1486 "parser.cpp" /* yacc.c:1663  */
    break;

  case 3:
#line 157 "parser.yy" /* yacc.c:1663  */
    { (yyval.slist) = new vector<ptr<Statement> >(); }
#line 1492 "parser.cpp" /* yacc.c:1663  */
    break;

  case 4:
#line 158 "parser.yy" /* yacc.c:1663  */
    {
		auto list = (yyvsp[0].slist);
		list->insert(list->begin(), shared_ptr<Statement>((yyvsp[-1].stmt)));
		(yyval.slist) = list;
		}
#line 1502 "parser.cpp" /* yacc.c:1663  */
    break;

  case 12:
#line 174 "parser.yy" /* yacc.c:1663  */
    {
	(yyval.stmt) = new sGlobal(*(yyvsp[-1].strconst));
	delete (yyvsp[-1].strconst);
	}
#line 1511 "parser.cpp" /* yacc.c:1663  */
    break;

  case 13:
#line 180 "parser.yy" /* yacc.c:1663  */
    {
	(yyval.stmt) = new sAssign((yyvsp[-3].expr), (yyvsp[-1].expr));
	}
#line 1519 "parser.cpp" /* yacc.c:1663  */
    break;

  case 14:
#line 185 "parser.yy" /* yacc.c:1663  */
    {
	(yyval.stmt) = new sExpression((yyvsp[-1].expr));
}
#line 1527 "parser.cpp" /* yacc.c:1663  */
    break;

  case 15:
#line 191 "parser.yy" /* yacc.c:1663  */
    {
	(yyval.stmt) = new sIfThen((yyvsp[-3].expr), (yyvsp[-1].stmt), (yyvsp[0].stmt));
}
#line 1535 "parser.cpp" /* yacc.c:1663  */
    break;

  case 16:
#line 198 "parser.yy" /* yacc.c:1663  */
    { (yyval.stmt) = NULL; }
#line 1541 "parser.cpp" /* yacc.c:1663  */
    break;

  case 17:
#line 199 "parser.yy" /* yacc.c:1663  */
    { (yyval.stmt) = (yyvsp[0].stmt); }
#line 1547 "parser.cpp" /* yacc.c:1663  */
    break;

  case 18:
#line 204 "parser.yy" /* yacc.c:1663  */
    {
  (yyval.stmt) = new sWhile((yyvsp[-2].expr), (yyvsp[0].stmt));

}
#line 1556 "parser.cpp" /* yacc.c:1663  */
    break;

  case 19:
#line 210 "parser.yy" /* yacc.c:1663  */
    { (yyval.stmt) = new sBlock(*(yyvsp[-1].slist)); delete (yyvsp[-1].slist); }
#line 1562 "parser.cpp" /* yacc.c:1663  */
    break;

  case 20:
#line 213 "parser.yy" /* yacc.c:1663  */
    {  (yyval.stmt) = new sReturn((yyvsp[-1].expr)); }
#line 1568 "parser.cpp" /* yacc.c:1663  */
    break;

  case 24:
#line 224 "parser.yy" /* yacc.c:1663  */
    {
	(yyval.expr) = new eFundecl(*(yyvsp[-2].namelist), (yyvsp[0].stmt));
	delete (yyvsp[-2].namelist);
}
#line 1577 "parser.cpp" /* yacc.c:1663  */
    break;

  case 25:
#line 230 "parser.yy" /* yacc.c:1663  */
    { (yyval.namelist) = new vector<string>(); }
#line 1583 "parser.cpp" /* yacc.c:1663  */
    break;

  case 26:
#line 231 "parser.yy" /* yacc.c:1663  */
    {
	auto list = (yyvsp[0].namelist);
	list->insert(list->begin(), *(yyvsp[-1].strconst));
	delete (yyvsp[-1].strconst);
	(yyval.namelist) = list;
}
#line 1594 "parser.cpp" /* yacc.c:1663  */
    break;

  case 27:
#line 239 "parser.yy" /* yacc.c:1663  */
    {
	if((yyvsp[-1].expr) == NULL){
		(yyval.expr) = (yyvsp[0].expr);
	}else{
		(yyval.expr) = new eBinary((yyvsp[-1].expr), (yyvsp[0].expr), OR);
	}
}
#line 1606 "parser.cpp" /* yacc.c:1663  */
    break;

  case 28:
#line 248 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = NULL; }
#line 1612 "parser.cpp" /* yacc.c:1663  */
    break;

  case 29:
#line 249 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = (yyvsp[-1].expr); }
#line 1618 "parser.cpp" /* yacc.c:1663  */
    break;

  case 30:
#line 253 "parser.yy" /* yacc.c:1663  */
    {
	if((yyvsp[-1].expr) == NULL){
		(yyval.expr) = (yyvsp[0].expr);
	}else{
		(yyval.expr) = new eBinary((yyvsp[-1].expr), (yyvsp[0].expr), AND);
	}
}
#line 1630 "parser.cpp" /* yacc.c:1663  */
    break;

  case 31:
#line 262 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = NULL; }
#line 1636 "parser.cpp" /* yacc.c:1663  */
    break;

  case 32:
#line 263 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = (yyvsp[-1].expr); }
#line 1642 "parser.cpp" /* yacc.c:1663  */
    break;

  case 33:
#line 267 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = (yyvsp[0].expr); }
#line 1648 "parser.cpp" /* yacc.c:1663  */
    break;

  case 34:
#line 268 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eUnary((yyvsp[0].expr), NOT); }
#line 1654 "parser.cpp" /* yacc.c:1663  */
    break;

  case 35:
#line 271 "parser.yy" /* yacc.c:1663  */
    {(yyval.expr) = (yyvsp[0].expr); }
#line 1660 "parser.cpp" /* yacc.c:1663  */
    break;

  case 36:
#line 272 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eUnary(new eBinary((yyvsp[-2].expr), (yyvsp[0].expr), GTE), NOT); }
#line 1666 "parser.cpp" /* yacc.c:1663  */
    break;

  case 37:
#line 273 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eBinary((yyvsp[-2].expr), (yyvsp[0].expr), GT); }
#line 1672 "parser.cpp" /* yacc.c:1663  */
    break;

  case 38:
#line 274 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eUnary(new eBinary((yyvsp[-2].expr), (yyvsp[0].expr), GT), NOT); }
#line 1678 "parser.cpp" /* yacc.c:1663  */
    break;

  case 39:
#line 275 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eBinary((yyvsp[-2].expr), (yyvsp[0].expr), GTE); }
#line 1684 "parser.cpp" /* yacc.c:1663  */
    break;

  case 40:
#line 276 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eBinary((yyvsp[-2].expr), (yyvsp[0].expr), EQ); }
#line 1690 "parser.cpp" /* yacc.c:1663  */
    break;

  case 41:
#line 279 "parser.yy" /* yacc.c:1663  */
    {
	if((yyvsp[-1].labeledExpression) == NULL){
		(yyval.expr) = (yyvsp[0].expr);
	}else{
		(yyval.expr) = new eBinary((yyvsp[-1].labeledExpression)->second, (yyvsp[0].expr), (yyvsp[-1].labeledExpression)->first);
		delete (yyvsp[-1].labeledExpression);
	}
}
#line 1703 "parser.cpp" /* yacc.c:1663  */
    break;

  case 42:
#line 289 "parser.yy" /* yacc.c:1663  */
    { (yyval.labeledExpression) = NULL; }
#line 1709 "parser.cpp" /* yacc.c:1663  */
    break;

  case 43:
#line 290 "parser.yy" /* yacc.c:1663  */
    { (yyval.labeledExpression) = new pair<BinaryOperator, Expression* >(PLUS, (yyvsp[-1].expr)); }
#line 1715 "parser.cpp" /* yacc.c:1663  */
    break;

  case 44:
#line 291 "parser.yy" /* yacc.c:1663  */
    { (yyval.labeledExpression) = new pair<BinaryOperator, Expression* >(MINUS, (yyvsp[-1].expr)); }
#line 1721 "parser.cpp" /* yacc.c:1663  */
    break;

  case 45:
#line 294 "parser.yy" /* yacc.c:1663  */
    {
	if((yyvsp[-1].labeledExpression) == NULL){
		(yyval.expr) = (yyvsp[0].expr);
	}else{
		(yyval.expr) = new eBinary((yyvsp[-1].labeledExpression)->second, (yyvsp[0].expr), (yyvsp[-1].labeledExpression)->first);
		delete (yyvsp[-1].labeledExpression);
	}
}
#line 1734 "parser.cpp" /* yacc.c:1663  */
    break;

  case 46:
#line 305 "parser.yy" /* yacc.c:1663  */
    {(yyval.labeledExpression) = NULL; }
#line 1740 "parser.cpp" /* yacc.c:1663  */
    break;

  case 47:
#line 306 "parser.yy" /* yacc.c:1663  */
    { (yyval.labeledExpression) = new pair<BinaryOperator, Expression* >(TIMES, (yyvsp[-1].expr)); }
#line 1746 "parser.cpp" /* yacc.c:1663  */
    break;

  case 48:
#line 307 "parser.yy" /* yacc.c:1663  */
    { (yyval.labeledExpression) = new pair<BinaryOperator, Expression* >(DIV, (yyvsp[-1].expr)); }
#line 1752 "parser.cpp" /* yacc.c:1663  */
    break;

  case 49:
#line 310 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eUnary((yyvsp[0].expr), UMINUS); }
#line 1758 "parser.cpp" /* yacc.c:1663  */
    break;

  case 50:
#line 311 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = (yyvsp[0].expr); }
#line 1764 "parser.cpp" /* yacc.c:1663  */
    break;

  case 54:
#line 317 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = (yyvsp[-1].expr); }
#line 1770 "parser.cpp" /* yacc.c:1663  */
    break;

  case 55:
#line 320 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eNum((yyvsp[0].intconst)); }
#line 1776 "parser.cpp" /* yacc.c:1663  */
    break;

  case 56:
#line 321 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eString(*(yyvsp[0].strconst)); delete (yyvsp[0].strconst); }
#line 1782 "parser.cpp" /* yacc.c:1663  */
    break;

  case 57:
#line 322 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eNone(); }
#line 1788 "parser.cpp" /* yacc.c:1663  */
    break;

  case 58:
#line 323 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eBconst(true); }
#line 1794 "parser.cpp" /* yacc.c:1663  */
    break;

  case 59:
#line 324 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eBconst(false); }
#line 1800 "parser.cpp" /* yacc.c:1663  */
    break;

  case 60:
#line 326 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eVar(*(yyvsp[0].strconst)); delete (yyvsp[0].strconst); }
#line 1806 "parser.cpp" /* yacc.c:1663  */
    break;

  case 61:
#line 327 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eField((yyvsp[-2].expr), *(yyvsp[0].strconst)); delete (yyvsp[0].strconst); }
#line 1812 "parser.cpp" /* yacc.c:1663  */
    break;

  case 62:
#line 328 "parser.yy" /* yacc.c:1663  */
    { (yyval.expr) = new eIndex((yyvsp[-3].expr), (yyvsp[-1].expr)); }
#line 1818 "parser.cpp" /* yacc.c:1663  */
    break;

  case 63:
#line 333 "parser.yy" /* yacc.c:1663  */
    {
	(yyval.expr) = new eFuncall((yyvsp[-3].expr), *(yyvsp[-1].elist));
	delete (yyvsp[-1].elist);
}
#line 1827 "parser.cpp" /* yacc.c:1663  */
    break;

  case 64:
#line 339 "parser.yy" /* yacc.c:1663  */
    { (yyval.elist) = new vector<ptr<Expression> >(); }
#line 1833 "parser.cpp" /* yacc.c:1663  */
    break;

  case 65:
#line 340 "parser.yy" /* yacc.c:1663  */
    {
	auto list = (yyvsp[0].elist);
	list->insert(list->begin(), shared_ptr<Expression>((yyvsp[-1].expr)));
	(yyval.elist) = list;
}
#line 1843 "parser.cpp" /* yacc.c:1663  */
    break;

  case 66:
#line 347 "parser.yy" /* yacc.c:1663  */
    { (yyval.elist) = new vector<ptr<Expression> >(); }
#line 1849 "parser.cpp" /* yacc.c:1663  */
    break;

  case 67:
#line 348 "parser.yy" /* yacc.c:1663  */
    {
	auto list = (yyvsp[0].elist);
	list->insert(list->begin(), shared_ptr<Expression>((yyvsp[-1].expr)));
	(yyval.elist) = list;
}
#line 1859 "parser.cpp" /* yacc.c:1663  */
    break;

  case 68:
#line 354 "parser.yy" /* yacc.c:1663  */
    {
	(yyval.expr) = new eRecordConstructor(*(yyvsp[-1].fieldlist));
	delete (yyvsp[-1].fieldlist);
}
#line 1868 "parser.cpp" /* yacc.c:1663  */
    break;

  case 69:
#line 359 "parser.yy" /* yacc.c:1663  */
    { (yyval.fieldlist) = new vector<pair<string, ptr<Expression> > >(); }
#line 1874 "parser.cpp" /* yacc.c:1663  */
    break;

  case 70:
#line 360 "parser.yy" /* yacc.c:1663  */
    {
	auto fields = (yyvsp[0].fieldlist);
	fields->insert(fields->begin(), make_pair(*(yyvsp[-4].strconst), shared_ptr<Expression>((yyvsp[-2].expr))));
	delete (yyvsp[-4].strconst);
	(yyval.fieldlist) = fields;
}
#line 1885 "parser.cpp" /* yacc.c:1663  */
    break;


#line 1889 "parser.cpp" /* yacc.c:1663  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (&yylloc, yyscanner, out, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (&yylloc, yyscanner, out, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc, yyscanner, out);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, yylsp, yyscanner, out);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, yyscanner, out, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, yyscanner, out);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, yylsp, yyscanner, out);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 369 "parser.yy" /* yacc.c:1907  */


// Error reporting function. You should not have to modify this.
int yyerror(YYLTYPE * yylloc, void* p, Statement*& out, const char*  msg){

  cout<<"Error in line "<<yylloc->last_line<<", col "<<yylloc->last_column<<": "<<msg;
  return 0;
}

