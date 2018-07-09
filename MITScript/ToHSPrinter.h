#pragma once

#include "AST.h"
#include <string>
#include <iostream>

using namespace std;



inline string hsStr(BinaryOperator op) {
    switch (op) {
        case PLUS: return "PLUS";
        case MINUS: return "MINUS";
        case TIMES: return "TIMES";
        case DIV: return "DIV";
        case AND: return "AND";
        case OR: return "OR";
        case GT: return "GT";
        case GTE: return "GTE";
        case EQ: return "EQ";
        default: return "ERROR";
    }
}

inline string hsSTr(UnaryOperator op) {
    switch (op) {
        case UMINUS: return "UMINUS";
        case NOT: return "NOT";
    }
}

class ToHSPrinter : public Visitor {
    void doNode(ptr<AST_node> node) {
        node->accept(*this);
    }
public:
    ToHSPrinter()  { }
    void visit(eBinary& exp) {
        cout << "(BinExp ";
        doNode(exp.left());
        cout << " ";
        cout << str(exp.op());
        cout << " ";
        doNode(exp.right());
        cout << ")";
    }
    void visit(eUnary& exp) {
        cout << "(UnExp ";
        cout << str(exp.op());
        cout << " ";
        doNode(exp.operand());
        cout << ")";
    }
    void visit(eNone& exp){
        cout << "None";
    }
    void visit(eNum& exp) {
        cout << "(NumConst ";
        cout << exp.val();
        cout << ")";
    }
    void visit(eString& exp) {
        cout << "(Str ";
        cout << "\"" << exp.val() << "\"";
        cout << ")";
    }
    void visit(eVar& exp) {
        cout << "(Var (Name ";
        cout << exp.name();
        cout << "))";
    }
    void visit(eFuncall& exp) {
        cout << "(FunCall ";
        doNode(exp.target());
        cout << " [";
        auto params = exp.params();
        for (auto it = params.begin(); it != params.end(); ++it) {
            if (it != params.begin())
                cout << ", ";
            doNode(*it);
        }
        cout << "])";
    }
    void visit(eFundecl& exp) {
        cout << "(FunDecl [";
        auto formals = exp.formals();
        for (auto it = formals.begin(); it != formals.end(); ++it) {
            if (it != formals.begin())
                cout << ", ";
            cout << "(Name \"" << *it << "\")";
        }
        cout << "] ";
        doNode(exp.body());
        cout << ")";
    }

    void visit(eIndex& exp) {
        cout << "(Index ";
        doNode(exp.base());
        cout << " ";
        doNode(exp.index());
        cout << ")";
    }

    void visit(eField& exp) {
        cout << ("(Field ");
        doNode(exp.base());
        cout << " ";
        cout << exp.field();
        cout << ")";
    }
    void visit(sBlock& exp) {
        cout << "(Block [";
        for (auto it = exp.body().begin(); it != exp.body().end(); ++it) {
            if (it != exp.body().begin())
                cout << ", ";

            doNode(*it);
        }
        cout << "])";
    }
    void visit(sAssign& exp) {
        cout << "(Assign ";
        doNode(exp.lhs());
        cout << " ";
        doNode(exp.rhs());
        cout << ")";
    }
    void visit(sIfThen& exp) {
        cout << "(If ";
        doNode(exp.cond());
        cout << " ";
        doNode(exp.tpart());
        cout << " ";
        if (exp.hasEpart()) {
            doNode(exp.epart());
        } else {
            cout << "(Block [])";
        }
        cout << ")";
    }
    void visit(sWhile& exp) {
        cout << "(While";
        doNode(exp.cond());
        cout << " ";
        doNode(exp.body());
        cout << ")";
    }
    void visit(eBconst& exp) {
        if (exp.val()) {
            cout << "(BConst True)";
        } else {
            cout << "(BConst False)";
        }
    }
    void visit(eRecordConstructor& exp) {
        cout << "(Record [";
        for (auto it = exp.begin(); it != exp.end(); ++it) {
            if (it != exp.begin())
                cout << ", ";

            cout << "(RecordPair \"" << it->first << "\" ";
            it->second->accept(*this);
            cout << ")";
        }
        cout << "])";
    }
    void visit(sExpression& stmt) {
        cout << "(ExpStmt ";
        doNode(stmt.expr());
        cout << ")";
    }
    void visit(sReturn& stmt) {
        cout << "(Return ";
        doNode(stmt.expr());
        cout << ")";
    }
    void visit(sGlobal& stmt) {
        cout << "(Global (Name " << stmt.name() << "))";
    }
};
