#pragma once

#include "AST.h"
#include <string>
#include <iostream>

using namespace std;

//This is where you get to define your pretty printer class, which should be
//a subtype of visitor.
class PrettyPrinter : public Visitor {
//begin_student_code
	int spaces;
	void indent() {
		spaces++;
	}
	void unindent() {
		spaces--;
	}
	void newline() {
		cout << endl;
		for (auto i = 0; i < spaces; ++i) {
			cout << "\t";
		}
	}
	void doNode(ptr<AST_node> node) {
		node->accept(*this);
	}
public:
	PrettyPrinter() :spaces(0) { }
	void visit(eBinary& exp) {
		cout << "(";
		doNode(exp.left());
		cout << str(exp.op());
		doNode(exp.right());
		cout << ")";
	}
	void visit(eUnary& exp) {
		cout << "(";
		cout << str(exp.op());
		doNode(exp.operand());
		cout << ")";
	}
	void visit(eNone& exp){
		cout << "None";
	}
	void visit(eNum& exp) {
		cout << exp.val();
	}
	void visit(eString& exp) {
		cout << "\"" << exp.val() << "\"";
	}
	void visit(eVar& exp) {
		cout << exp.name();
	}
	void visit(eFuncall& exp) {
		doNode(exp.target());
		cout << "(";
		auto params = exp.params();
		for (auto it = params.begin(); it != params.end(); ++it) {
			if (it != params.begin())
				cout << ", ";
			doNode(*it);
		}
		cout << ")";
	}
	void visit(eFundecl& exp) {
		cout << "fun(";
		auto formals = exp.formals();
		for (auto it = formals.begin(); it != formals.end(); ++it) {
			cout << " " << *it;
		}
		cout << ")";
		doNode(exp.body());

	}

	void visit(eIndex& exp) {
		doNode(exp.base());
		cout << "[";
		doNode(exp.index());
		cout << "]";
	}

	void visit(eField& exp) {
		doNode(exp.base());
		cout << "." << exp.field();
	}
	void visit(sBlock& exp) {
		cout << "{";
		indent();
		newline();
		for (auto it = exp.body().begin(); it != exp.body().end(); ++it) {
			doNode(*it);
		}
		unindent();
		newline();
		cout << "}";
	}
	void visit(sAssign& exp) {
		doNode(exp.lhs());
		cout << " = ";
		doNode(exp.rhs());
		cout << ";";
		newline();
	}
	void visit(sIfThen& exp) {
		cout << "if(";
		doNode(exp.cond());
		cout << ")";
		doNode(exp.tpart());
		if (exp.hasEpart()) {
			cout << " else ";
			doNode(exp.epart());
		}
	}
	void visit(sWhile& exp) {
		cout << "while(";
		doNode(exp.cond());
		cout << ")";
		doNode(exp.body());
	}
	void visit(eBconst& exp) {
		if (exp.val()) {
			cout << "true";
		}
		else {
			cout << "false";
		}
	}
	void visit(eRecordConstructor& exp) {
		cout << "{";
		indent();
		newline();
		for (auto it = exp.begin(); it != exp.end(); ++it) {
			cout << it->first << ":";
			it->second->accept(*this);
			cout << ";";
			newline();
		}
		unindent();
		newline();
		cout << "}";
	}
	void visit(sExpression& stmt) {
		stmt.expr()->accept(*this);
		cout << ";";
		newline();
	}
	void visit(sReturn& stmt) {
		cout << "return ";
		stmt.expr()->accept(*this);
		cout << ";";
		newline();
	}
	void visit(sGlobal& stmt) {
		cout << "global " << stmt.name() << ";";
		newline();
	}
//end_student_code
};
