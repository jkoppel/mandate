#pragma once

#include "InterpreterExceptions.h"
#include "AST.h"
#include <set>
#include <sstream>


class Function;
class Record;

class Value {
public:
	virtual bool isValid() = 0;
	virtual bool isInt(int& out) = 0;
	virtual bool isString() = 0;
	virtual int getInt() = 0;
	virtual bool getBool() = 0;
	virtual Record* getRecord() = 0;
	virtual Function* getFun() = 0;	
	virtual string str() = 0;
};

class None: public Value {
	virtual bool isValid() { return false; }
	virtual bool isInt(int& out) {
		return false;
	}
	virtual int getInt() { throw IllegalCastException("Uninitialized to int"); }
	virtual Function* getFun() { 
		throw IllegalCastException("None to fun"); 
	}
	virtual Record* getRecord() { throw IllegalCastException("Uninitialized to Record"); }
	virtual bool getBool() { throw IllegalCastException("Uninitialized to Bool"); }
	virtual string str() { return "None";  }
	virtual bool isString() {
		return false;
	};
};

ptr<Value> NONE = shared_ptr<Value>(new None());

class String : public Value {
public:
	const string val;
	String(const string& v) : val(v) {}
	virtual bool isValid() { return true; }
	virtual int getInt() { throw IllegalCastException("String to int"); }
	virtual Function* getFun(){ throw IllegalCastException("String to fun"); }
	virtual Record* getRecord() { throw IllegalCastException("String to Record"); }
	virtual bool getBool() { throw IllegalCastException("String to Bool"); }
	virtual string str() { 
		return val;
	}
	virtual bool isInt(int& out) {
		return false;
	}
	virtual bool isString() {
		return true;
	};
};

class Number : public Value {
public:
	const int val;
	Number(int v) :val(v) {}
	virtual bool isValid() { return true; }
	virtual int getInt() { return val; }
	virtual Function* getFun() { throw IllegalCastException("int to fun"); }
	virtual Record* getRecord() { throw IllegalCastException("int to Record"); }
	virtual bool getBool() { throw IllegalCastException("Uninitialized to Bool"); }
	virtual string str() {
		stringstream s;
		s << val;
		return s.str();
	}
	virtual bool isInt(int& out) {
		out = val;
		return true;
	}
	virtual bool isString() {
		return false;
	};
};

class Boolean : public Value {
public:
	const bool val;
	Boolean(bool v) :val(v) {}
	virtual bool isValid() { return true; }
	virtual int getInt() { throw IllegalCastException("bool to int"); }
	virtual Function* getFun() { throw IllegalCastException("bool to fun"); }
	virtual Record* getRecord() { throw IllegalCastException("bool to Record"); }
	virtual bool getBool() { return val; }
	virtual string str() {
		return val ? "True" : "False";
	}
	virtual bool isInt(int& out) {		
		return false;
	}
	virtual bool isString() {
		return false;
	};
};

ptr<Value> TRUE = shared_ptr<Value>(new Boolean(true));
ptr<Value> FALSE = shared_ptr<Value>(new Boolean(false));

class Record : public Value {
protected:
	map<string, ptr<Value> > fields;
public:
	virtual bool isValid() { return true; }
	virtual int getInt() { throw IllegalCastException("Record to int"); }
	virtual Function* getFun() { throw IllegalCastException("Record to fun"); }
	virtual bool getBool() { throw IllegalCastException("Record to Bool"); }
	virtual Record* getRecord() { return this; }
	void setField(const string& name, ptr<Value> val) {
		fields[name] = val;
	}
	ptr<Value>& getField(const string& name) {
		if (fields.count(name) > 0)
			return fields[name];
		else
			return NONE;
	}
	virtual string str() {
		stringstream s;
		for (auto it = fields.begin(); it != fields.end(); ++it) {
			s << it->first << ": " << it->second->str() << endl;
		}
		return s.str();
	}
	virtual bool isInt(int& out) {
		return false;
	}
	virtual bool isString() {
		return false;
	};
};

class State;

class Function : public Value {
public:
	const ptr<State> closure;
	eFundecl& fun;
	Function(ptr<State> s, eFundecl& f) : closure(s), fun(f) {}
	virtual int getInt() { throw IllegalCastException("Function to int"); }
	virtual Record* getRecord() { throw IllegalCastException("Function to Record"); }
	virtual bool getBool() { throw IllegalCastException("Function to Bool"); }
	virtual Function* getFun() { return this; }
	virtual bool isValid() { return true; }
	virtual string str() {
		return "FUNCTION";
	}
	virtual bool isInt(int& out) {
		return false;
	}
	virtual bool isString() {
		return false;
	};
};




class State {
public:
	map<string, ptr<Value> > vars;
	ptr<State> parent;
	set<string> globals;
	State():parent(NULL) {

	}
	void assign(string& name, ptr<State>& globalScope, ptr<Value> value) {
		if (globals.count(name) > 0) {
			globalScope->vars[name] = value;
		}else {
			vars[name] = value;
		}
		
	}
	ptr<Value> getValue(string& name, ptr<State>& globalScope) {
		if (globals.count(name) > 0) {
			//name was declared as global in this scope.
			//that means we should get it from the global scope.
			Assert(globalScope->vars.count(name) > 0, "Variable declared as global but not declared in globals.");
			return globalScope->vars[name];
		}
		auto valueIt = vars.find(name);
		if (valueIt != vars.end()) {			
			return valueIt->second;
		}else{
			if (parent == NULL) {
				throw UninitializedVariableException(name);
			}
			//Not in this scope, try the previous one.
			return parent->getValue(name, globalScope);
		}
	}

};

