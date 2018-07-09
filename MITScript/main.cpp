
#include "parser.h"
#include "lexer.h"
#include "AST.h"
#include "PrettyPrinter.h"
#include "ToHSPrinter.h"
#include <iostream>

using namespace std;




int main(int argc, char** argv){

  void* scanner;
  yylex_init(&scanner);
yyset_in(stdin, scanner);
  Statement* output;
  int rvalue = yyparse(scanner, output);
  if(rvalue == 1){
	cout<<"Parsing failed"<<endl;
	return 1;
  }

  //PrettyPrinter printer;
  ToHSPrinter printer;
  output->accept(printer); 

}
