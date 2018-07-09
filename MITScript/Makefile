BISON="/usr/local/Cellar/bison/3.0.5/bin/bison"


all: parser.cpp lexer.cpp main.cpp PrettyPrinter.h Visitor.h
	g++ -g -std=gnu++11 main.cpp parser.cpp lexer.cpp

interp: parser.cpp lexer.cpp Visitor.h Interpreter.h interpret.cpp InterpreterExceptions.h InterpreterState.h
	g++ -g -std=gnu++11 interpret.cpp parser.cpp lexer.cpp 

parser.cpp: parser.yy
	${BISON} --output=parser.cpp --defines=parser.h -v parser.yy

lexer.cpp: lexer.lex
	flex  --outfile=lexer.cpp --header-file=lexer.h lexer.lex


clean: 
	rm -f lexer.cpp lexer.h parser.cpp parser.h parser.output *.exe


