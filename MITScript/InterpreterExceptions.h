#pragma once


class InterpreterException {
protected:
	string name_;
public:
	InterpreterException(const string& name) :name_(name) {}
	virtual string message() = 0;
};

class UninitializedVariableException: public InterpreterException {
	
public:
	UninitializedVariableException(string& name) :InterpreterException(name) {

	}
	string message() { return "UninitializedVariableException: " + name_;  }
};

class IllegalCastException : public InterpreterException {
	
public:
	IllegalCastException(const string& msg) :InterpreterException(msg) {}
	string message() { return "IllegalCastException: " + name_; }
};

class IllegalArithmeticException : public InterpreterException {
	
public:
	IllegalArithmeticException(const string& msg) :InterpreterException(msg) {}
	string message() { return "IllegalArithmeticException: " + name_; }
};

class RuntimeException : public InterpreterException {
	
public:
	RuntimeException(const string& msg) :InterpreterException(msg) {}
	string message() { return "RuntimeException: " + name_; }
};
