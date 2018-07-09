#pragma once


#include "AST.h"
#include "InterpreterState.h"
#include <stack>


class PopulateNewFrame : public Visitor {
	State* state_;
	
public:
	PopulateNewFrame(State* state) :state_(state) {
		
	}
	virtual void visit(eBinary& exp) {}
	virtual void visit(eUnary& exp) {}
	virtual void visit(eNum& exp) {}
	virtual void visit(eString& exp) {}
	virtual void visit(eVar& exp) {}
	virtual void visit(eNone& exp){}
	virtual void visit(eFuncall& exp) {}
	virtual void visit(eFundecl& exp) {}
	virtual void visit(eIndex& exp) {}
	virtual void visit(eField& exp) {}
	virtual void visit(eBconst& exp) {}

	virtual void visit(sBlock& exp) {
		auto stmts = exp.body();
		for (auto it = stmts.begin(); it != stmts.end(); ++it) {
			(*it)->accept(*this);
		}
	}
	virtual void visit(sAssign& exp) {
		if (exp.lhs()->type == Expression::VAR) {
			eVar* ev = (eVar*) &*exp.lhs();
			if (state_->globals.count(ev->name()) == 0) {
				state_->vars[ev->name()] = NONE;
			}
		}
	}
	virtual void visit(sIfThen& exp) {
		exp.tpart()->accept(*this);
		if (exp.hasEpart()) {
			exp.epart()->accept(*this);
		}

	}
	virtual void visit(sWhile& exp) {
		exp.body()->accept(*this);
	}
	virtual void visit(eRecordConstructor& exp) {}
	virtual void visit(sExpression& stmt) {}
	virtual void visit(sReturn& stmt) {}
	virtual void visit(sGlobal& stmt) {
		state_->globals.insert(stmt.name());
		auto it = state_->vars.find(stmt.name());
		if (it != state_->vars.end()) {
			state_->vars.erase(it);
		}
	}
};

class Interpreter: public Visitor {
	stack<ptr<State> > callstack;
	ptr<State> state;
	ptr<State> global;
	

	ptr<Value> funReturn;
	bool hasReturned;

	ptr<Value> rval;
	ptr<Value> eval(ptr<Expression> exp) {
		exp->accept(*this);
		return rval;
	}

	void debug(const string& msg) {
		//cout << msg << endl;
	}

	void helperAssign(ptr<Expression> lhs, ptr<Value> val) {
		switch(lhs->type) {
			case Expression::VAR: {
				eVar* ev = (eVar*) &*lhs;
				state->assign(ev->name(), global, val);
				return;
			}
			case Expression::FREAD:{
				eField* ev = (eField*)&*lhs;
				ptr<Value> base = eval(ev->base());
				Record* rec = dynamic_cast<Record*>(&*base);
				Assert(rec != NULL, "Field dereference but not a record");
				rec->setField(ev->field(), val);
				return;
			}
			case Expression::IDX:{
				eIndex* ev = (eIndex*)&*lhs;
				ptr<Value> base = eval(ev->base());
				Record* rec = dynamic_cast<Record*>(&*base);
				Assert(rec != NULL, "Field dereference but not a record");
				rec->setField(stringCast(eval(ev->index())), val);
				return;
			}
			default:
				Assert("false", "You should not be here!");
		}
	}

	string stringCast(ptr<Value> val) {
		//There should be default to-string methods for everything. 
		//In the case of records,  though, if the record has a _str function field, 
		//we call that function and expect it to return a string.
		return val->str();
	}
	ptr<Value> valuePlus(ptr<Value> left, ptr<Value> right) {
		//We should have + methods for everything.
		//In the case of records, if the record has a _plus function field,
		//we call that function.
		int vleft;
		if (left->isInt(vleft)) {
			int vright;
			if (right->isInt(vright)) {
				return shared_ptr<Value>(new Number(vleft + vright));
			}
		}
		if (left->isString() || right->isString()) {
			string sleft = stringCast(left);
			sleft += stringCast(right);
			return shared_ptr<Value>(new String(sleft));
		}
		throw IllegalCastException("IllegalCastException");
	}
	ptr<Value> valueEq(ptr<Value> left, ptr<Value> right) {
		//We should have == methods for everything.
		if (left == right) {
			return shared_ptr<Value>(new Boolean(true));
		}
		int lval;
		if (left->isInt(lval)) {
			int rval;
			if (right->isInt(rval)) {
				return shared_ptr<Value>( new Boolean(lval == rval) );
			}
		}
		if (left->isString() && right->isString()) {
			return shared_ptr<Value>(new Boolean(left->str() == right->str()));
		}
		if (left == NONE && right == NONE) {
			return shared_ptr<Value>(new Boolean(true));
		}
		

		return shared_ptr<Value>(new Boolean(false));
	}
	ptr<eFundecl> printFunBody;
	ptr<eFundecl> inputFunBody;
	ptr<eFundecl> intcastFunBody;

	ptr<Function> printFun;
	ptr<Function> inputFun;
	ptr<Function> intcastFun;

	void nativePrint(const vector<ptr<Expression> >& params) {
		Assert(params.size() == 1, "Print should have one argument");
		cout <<stringCast(eval(params[0])) << endl;
	}

	void nativeIntcast(const vector<ptr<Expression> >& params) {
		Assert(params.size() == 1, "Print should have one argument");
		string s = stringCast(eval(params[0]));
		int ival = atoi(s.c_str());
		rval = shared_ptr<Value>(new Number(ival));
	}

	void nativeInput() {
		string val;
		std::getline(cin, val);
		rval = shared_ptr<Value>(new String(val));
	}

public:	
	Interpreter() {
		state = std::shared_ptr<State>(new State());
		global = state;
		callstack.push(state);
		{
			vector<string> pars;
			pars.push_back("in");
			printFunBody = std::shared_ptr<eFundecl>(new eFundecl(pars, NULL));
			printFun = shared_ptr<Function>(new Function(global, *printFunBody));
			global->vars["print"] = std::shared_ptr<Value>(printFun);
		}
		{
			vector<string> pars;			
			inputFunBody = std::shared_ptr<eFundecl>(new eFundecl(pars, NULL));
			inputFun = shared_ptr<Function>(new Function(global, *inputFunBody));
			global->vars["input"] = std::shared_ptr<Value>(inputFun);
		}
		{
			vector<string> pars;
			pars.push_back("in");
			intcastFunBody = std::shared_ptr<eFundecl>(new eFundecl(pars, NULL));
			intcastFun = shared_ptr<Function>(new Function(global, *intcastFunBody));
			global->vars["intcast"] = std::shared_ptr<Value>(intcastFun);
		}

		funReturn = NULL;
		hasReturned = false;
	}


	//begin_student_code
	void visit(eBinary& exp) {
		switch (exp.op()) {
			case PLUS: 
				rval = valuePlus(eval(exp.left()), eval(exp.right()));
				return;
			case MINUS:
				rval = shared_ptr<Value>(new Number(eval(exp.left())->getInt() - eval(exp.right())->getInt()));
				return;
			case TIMES:
				rval = shared_ptr<Value>(new Number(eval(exp.left())->getInt() * eval(exp.right())->getInt()));
				return;
			case DIV: {
				int denom = eval(exp.right())->getInt();
				if (denom == 0) { throw IllegalArithmeticException("Division by zero"); }
				rval = shared_ptr<Value>(new Number(eval(exp.left())->getInt() / denom));
				return;
			}
			case AND: 
				rval = shared_ptr<Value>(new Boolean((eval(exp.left())->getBool() && eval(exp.right())->getBool())));
				return;
			case OR: 
				rval = shared_ptr<Value>(new Boolean((eval(exp.left())->getBool() || eval(exp.right())->getBool())));
				return;
			case GT: 
				rval = shared_ptr<Value>(new Boolean(eval(exp.left())->getInt() > eval(exp.right())->getInt()));
				return;
			case GTE: 
				rval = shared_ptr<Value>(new Boolean(eval(exp.left())->getInt() >= eval(exp.right())->getInt()));
				return;
			case EQ:
				rval = valueEq(eval(exp.left()), eval(exp.right()));
				return;
		}
	}
	virtual void visit(eUnary& exp) {
		switch (exp.op()) {
		case NOT:
			rval = shared_ptr<Value>(new Boolean(! eval(exp.operand())->getBool()));
			return;
		case UMINUS:
			rval = shared_ptr<Value>(new Number( - eval(exp.operand())->getInt()));
			return;
		}
	}
	virtual void visit(eNum& exp) {
		rval = shared_ptr<Value>(new Number(exp.val()));
	}
	virtual void visit(eString& exp) {
		rval = shared_ptr<Value>(new String(exp.val()));
	}
	virtual void visit(eVar& exp) {
		rval = state->getValue(exp.name(), global);
	}

	virtual void visit(eNone& exp) {
		rval = NONE;
	}

	virtual void visit(eBconst& exp) {
		rval = exp.val() ? TRUE : FALSE;
	}

	State* createStackframe(Function& f) {
		State* newstate = new State();
		PopulateNewFrame populate(newstate);
		f.fun.body()->accept(populate);
		newstate->parent = f.closure;
		return newstate;
	}



	virtual void visit(eFuncall& exp) {
		debug("Calling function");
		ptr<Value> val = eval(exp.target());
		Function* fval = val->getFun();

		if (fval == printFun.get()) {
			nativePrint(exp.params());
		} else if (fval == inputFun.get()) {
			nativeInput();
		}else if (fval == intcastFun.get()) {
			nativeIntcast(exp.params());
		} else {
			hasReturned = false;
			funReturn = NONE;
			State* newstate = createStackframe(*fval);
			auto formals = fval->fun.formals();
			auto params = exp.params();
			int i = 0;
			if (params.size() > formals.size()) {
				throw RuntimeException("Too many parameters");
			}
			for (auto it = formals.begin(); it != formals.end(); ++it, ++i) {
				if (i < params.size()) {
					newstate->vars[*it] = eval((params[i]));
				}
				else {
					newstate->vars[*it] = NONE;
				}
			}

			
			state = std::shared_ptr<State>(newstate);
			callstack.push(state);

			fval->fun.body()->accept(*this);
			callstack.pop();
			state = callstack.top();
			rval = funReturn;
			hasReturned = false;

		}

		

	}
	virtual void visit(eFundecl& exp) {		
		rval = shared_ptr<Value>(new Function(state, exp));
	}

	virtual void visit(eIndex& exp) {
		ptr<Value> base = eval(exp.base());
		Record* r = base->getRecord();
		ptr<Value> index = eval(exp.index());
		rval = r->getField( this->stringCast(index) );
	}

	virtual void visit(eField& exp) {
		ptr<Value> base = eval(exp.base());
		Record* r = base->getRecord();
		rval =  r->getField(exp.field());	
	}
	virtual void visit(sBlock& exp) {
		debug("Entering Block");
		auto stmts = exp.body();
		for (auto it = stmts.begin(); it != stmts.end(); ++it) {
			(*it)->accept(*this);
			if (hasReturned) { return;  }
		}
	}
	virtual void visit(sAssign& exp) {
		debug("Assignment");
		ptr<Value> v = eval(exp.rhs());
		helperAssign(exp.lhs(), v);

	}
	virtual void visit(sIfThen& exp) {
		ptr<Value> v = eval(exp.cond());
		if (v->getBool()) {
			exp.tpart()->accept(*this);
		} else {
			if (exp.hasEpart()) {
				exp.epart()->accept(*this);
			}
		}
	}
	virtual void visit(sWhile& exp) {
		ptr<Value> v = eval(exp.cond());
		while (v->getBool()) {
			exp.body()->accept(*this);
			if (hasReturned) { return;  }
			v = eval(exp.cond());
		}
	}
	virtual void visit(eRecordConstructor& exp) {
		Record* r = new Record();
		for (auto field = exp.begin(); field != exp.end(); ++field) {
			r->setField(field->first, eval(field->second));
		}
		rval = shared_ptr<Value>(r);		
	}
	virtual void visit(sExpression& stmt) {
		eval(stmt.expr());
	}
	virtual void visit(sReturn& stmt){
		funReturn = eval(stmt.expr());
		hasReturned = true;
	}
	virtual void visit(sGlobal& stmt) {
		//Global declarations are handled by PopulateNewFrame.
	}
	//end_student_code
};
