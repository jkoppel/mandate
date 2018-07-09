#pragma once

#include <memory>

using namespace std;


//begin_student_code
//ptr is an alias for std::shared_ptr, which is a pretty useful class
//to do some automatic memory management for you.
template<class T>
using ptr = std::shared_ptr<T>;

class eBinary;
class eUnary;
class eNum;
class eString;
class eVar;
class eFuncall;
class eFundecl;
class eIndex;
class eField;
class eRecordConstructor;
class sBlock;
class sAssign;
class sIfThen;
class sWhile;
class sExpression;
class sReturn; 
class sGlobal;
class eNone;
class eBconst;
//end_student_code

//You will need to a virtual visitor class with a 
//visit method for each different type of expression and statement
//as defined in AST.h

class Visitor {
public:
// For each AST node, you need a virtual method of the form 
// virtual void visit(EXPRESSION_TYPE& exp)=0;
//begin_student_code
	virtual void visit(eBinary& exp)=0;
	virtual void visit(eUnary& exp) = 0;
	virtual void visit(eNum& exp) = 0;
	virtual void visit(eString& exp) = 0;
	virtual void visit(eVar& exp) = 0;
	virtual void visit(eFuncall& exp) = 0;
	virtual void visit(eFundecl& exp) = 0;
	virtual void visit(eIndex& exp) = 0;
	virtual void visit(eField& exp) = 0;
	virtual void visit(eNone& exp) = 0;
	virtual void visit(sBlock& exp) = 0;
	virtual void visit(sAssign& exp) = 0;
	virtual void visit(sIfThen& exp) = 0;
	virtual void visit(sWhile& exp) = 0;	
	virtual void visit(eRecordConstructor& exp)=0;
	virtual void visit(sExpression& stmt)=0;
	virtual void visit(sReturn& stmt)=0; 
	virtual void visit(sGlobal& stmt)=0;
	virtual void visit(eBconst& exp) = 0;
//end_student_code
};


