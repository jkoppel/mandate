#pragma once

#include <iostream>
#include <memory>
#include <vector>
#include <map>



class SystemException {
	string msg_;
public:
	SystemException(const string& msg) :msg_(msg) {}
};

#define Assert(cond, msg) if(!(cond)){ std::cerr<<msg<<endl; throw SystemException("Bad stuff"); }

using namespace std;


#include "Visitor.h"



class AST_node {

public:
	virtual void accept(Visitor& v) = 0;
};

class Expression : public AST_node {
public:
	//begin_student_code
	typedef enum { BINOP, UNOP, VAR, ICONST, BCONST, NONECONST, SCONST, FREAD, IDX, FCALL, FDECL, RECORD } ExprType;
	ExprType type;
	Expression(Expression::ExprType t) : type(t) {}
	//end_student_code

};

class Statement: public AST_node {

};

//You need to define classes that inherit from Expression and Statement
//for all the different kinds of expressions and statements in the language.

//begin_student_code


typedef enum { PLUS, MINUS, TIMES, DIV, AND, OR, GT, GTE, EQ } BinaryOperator;
typedef enum {UMINUS, NOT} UnaryOperator;

inline string str(BinaryOperator op) {
	switch (op) {
	case PLUS: return "+";
	case MINUS: return "-";
	case TIMES: return "*";
	case DIV: return "/";
	case AND: return "&";
	case OR: return "|";
	case GT: return ">";
	case GTE: return ">=";
	case EQ: return "==";
	default: return "ERROR";
	}
}

inline string str(UnaryOperator op) {
	switch (op) {
	case UMINUS: return "-";
	case NOT: return "!";
	}
}

class eBinary : public Expression {
protected:
	ptr<Expression> left_;
	ptr<Expression> right_;
	BinaryOperator op_;
public:
	eBinary(Expression* left, Expression* right, BinaryOperator op):Expression(Expression::BINOP),left_(left), right_(right), op_(op){}
	const ptr<Expression>& left() {  return left_;  }
	const ptr<Expression>& right() {  return right_; }
	const BinaryOperator op() {  return op_; }
	void accept(Visitor& v){
		 v.visit(*this);
	} 

};


class eUnary : public Expression {
protected:
	ptr<Expression> operand_;
	UnaryOperator op_;
public:
	eUnary(Expression* operand, UnaryOperator op):Expression(Expression::UNOP), operand_(operand), op_(op){}
	const ptr<Expression>& operand() {  return operand_; }
	const UnaryOperator op() {  return op_; }
	void accept(Visitor& v){
		 v.visit(*this);
	} 

};


class eNum : public Expression {
protected:
	int val_;
public:
	eNum(int val):Expression(Expression::ICONST), val_(val){}
	int val() {  return val_;  }
	void accept(Visitor& v){
		 v.visit(*this);
	} 
};

class eBconst : public Expression {
protected:
	int val_;
public:
	eBconst(bool val) :Expression(Expression::BCONST), val_(val) {}
	bool val() { return val_; }
	void accept(Visitor& v) {
		v.visit(*this);
	}
};

class eNone : public Expression {
protected:
	
public:
	eNone() :Expression(Expression::NONECONST) {}
	
	void accept(Visitor& v) {
		v.visit(*this);
	}
};

class eString : public Expression {
protected:
	string val_;
public:
	eString(string& val):Expression(Expression::SCONST), val_(val){}
	string& val() {  return val_; }
	void accept(Visitor& v){
		 v.visit(*this);
	} 
};

class eVar : public Expression {
protected:
	string name_;
public:
	eVar(string& name): Expression(Expression::VAR), name_(name){}
	string& name() {  return name_;  }
	void accept(Visitor& v){
		 v.visit(*this);
	} 
};

class eFuncall : public Expression {
protected:
	ptr<Expression> target_;
	vector<ptr<Expression> > params_;
public:
	eFuncall(Expression* target, vector<ptr<Expression> >& params):Expression(Expression::FCALL), target_(target), params_(params){}
	const ptr<Expression>& target() { return target_;  }
	const vector<ptr<Expression> >& params() {  return params_; } 
	void accept(Visitor& v){
		 v.visit(*this);
	} 
};


class eFundecl : public Expression {
protected:
	vector<string> formals_;
	ptr<Statement> body_;
public:
	eFundecl(vector<string>& formals, Statement* body):Expression(Expression::FDECL), formals_(formals), body_(body){}
	const vector<string>& formals() {  return formals_; }
	const ptr<Statement>& body() {  return body_; }
	void accept(Visitor& v){
		 v.visit(*this);
	} 

};


class eIndex : public Expression {
protected:
	ptr<Expression> base_;
	ptr<Expression> index_;
public:
	eIndex(Expression* base, Expression* index):Expression(Expression::IDX), base_(base), index_(index){}
	const ptr<Expression>& base() {  return base_; }
	const ptr<Expression>& index() {  return index_; };
	void accept(Visitor& v){
		 v.visit(*this);
	} 

};

class eField : public Expression {
protected:
	ptr<Expression> base_;
	string field_;
public:
	eField(Expression* base, string& field):Expression(Expression::FREAD), base_(base), field_(field){}
	const ptr<Expression>& base() {  return base_; }
	const string& field() {  return field_; };
	void accept(Visitor& v){
		 v.visit(*this);
	} 

};


class eRecordConstructor: public Expression{
protected: 
	vector<pair<string, ptr<Expression> > > fields_;
public:
	typedef vector<pair<string, ptr<Expression> > >::const_iterator field_iter;

	eRecordConstructor(vector<pair<string, ptr<Expression> > >& fields):Expression(Expression::RECORD), fields_(fields){}

	const vector<pair<string, ptr<Expression> > >& fields() { return fields_; }

	field_iter begin() { return fields_.begin();  }
	field_iter end() { return fields_.end();  }

	void accept(Visitor& v){
                v.visit(*this);
    }
};


class sBlock : public Statement {
protected:
	vector<ptr<Statement> > body_;
public:
	sBlock(vector<ptr<Statement> >& body): body_(body){}
	const vector<ptr<Statement> >& body() {  return body_; }

	void accept(Visitor& v){
		 v.visit(*this);
	} 

};

class sExpression: public Statement{
protected: 
	ptr<Expression> expr_;
public:
	sExpression(Expression* expr):expr_(expr){}
	const ptr<Expression>& expr() {  return expr_; }
	void accept(Visitor& v){
                 v.visit(*this);
        }
};

class sReturn: public Statement{
protected: 
	ptr<Expression> expr_;
public:
	sReturn(Expression* expr):expr_(expr){}
	const ptr<Expression>& expr() {  return expr_; }
	void accept(Visitor& v){
                 v.visit(*this);
        }
};


class sAssign : public Statement {
protected:
	ptr<Expression> lhs_;
	ptr<Expression> rhs_;
public:
	sAssign(Expression* lhs, Expression* rhs):lhs_(lhs), rhs_(rhs){}
	const ptr<Expression>& lhs() {  return lhs_; }
	const ptr<Expression>& rhs() {  return rhs_; }
	void accept(Visitor& v){
		 v.visit(*this);
	} 

};

class sIfThen : public Statement {
protected:
	ptr<Expression> cond_;
	ptr<Statement> tpart_;
	ptr<Statement> epart_;
	bool hasepart_;
public:
	sIfThen(Expression* cond, Statement* tpart, Statement* epart):cond_(cond),tpart_(tpart),epart_(epart), hasepart_(epart!=NULL){
	
	}
	const ptr<Expression> cond() {  return cond_; }
	const ptr<Statement> tpart() {  return tpart_; }
	const ptr<Statement> epart() {  return epart_; }
	bool hasEpart() { return hasepart_;  }
	void accept(Visitor& v){
		 v.visit(*this);
	} 

};

class sWhile : public Statement {
protected:
	ptr<Expression> cond_;
	ptr<Statement> body_;
public:
	sWhile(Expression* cond, Statement* body):cond_(cond), body_(body){ }
	const ptr<Expression> cond() { return cond_; }
	const ptr<Statement> body() {  return body_; }
	void accept(Visitor& v){
		 v.visit(*this);
	} 

};




class sGlobal : public Statement{
protected: 
	string name_;
public: 
	sGlobal(string& name): name_(name){}
	string& name(){ return name_; }
	void accept(Visitor& v){
                 v.visit(*this);
    }
};

//end_student_code


