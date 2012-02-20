# -*- coding: utf-8 -*-

import tokenizer
import functions
import special_operators
from tokenizer import nextToken

SYNTAX_ERROR = 0
INT = 1
FLOAT = 2
DOUBLE = 3
STRING = 4;
OPENING_PARENTHESIS = 5
LIST = 5 # OPENING_PARENTHESIS and LIST have purposely the same value!
CLOSING_PARENTHESIS = 6
SYMBOL = 7
RATIO = 8
QUOTE = 9
BACKQUOTE = 10
COMMA_AT = 11
COMMA = 12
HASH = 13
EOF = 14

FUN_OBJ = 30
		
class BadInputException(Exception):
	def __init__(self, msg):
		self.msg = msg

class Env:	
	def __init__(self, variables, funDict):
		self.variables = variables
		self.funDict = funDict

class Environment:
	def __init__(self, globalEnv, lexicalEnv):
		self.globalEnv = globalEnv
		self.lexicalEnv = lexicalEnv
		
	def getVariable(self, varName):
		val = self.lexicalEnv.variables.get(varName)
		if val:
			return val
		return self.globalEnv.variables.get(varName)
		
	def getFunction(self, fnName):
		val = self.lexicalEnv.funDict.get(fnName)
		if val:
			return val
		return self.globalEnv.funDict.get(fnName)
		
def getNil():
	return Symbol("NIL")
	
def getEmptyList():
	return List()

class Interpreter:
	
	funDict = {
		"+": functions.Plus(),
		"-": functions.Minus(),
		"*": functions.Mul(),
		"=": functions.Equal()
	}
	
	operatorsDict = {
		"if": special_operators.IfOperator(),
		"let": special_operators.Let(),
		"progn": special_operators.Progn(),
		"setq" : special_operators.Setq(),
		"quote" : special_operators.Quote(),
		"'" : special_operators.Quote(),
		#"list" : special_operators.List(),
		"car" : special_operators.Car(),
		"cdr" : special_operators.Cdr(),
		"eval" : special_operators.Eval(),
		"atom" : special_operators.Atom(),
		"`" : special_operators.Backquote(),
		"," : special_operators.Comma(),
		",@" : special_operators.Comma(),
		"lambda" : special_operators.Lambda(),
		"defun" : special_operators.Defun(),
		"#" : special_operators.Hash(),
		"funcall" : special_operators.Funcall(),
		"cons" : special_operators.Cons(),
		"eval" : special_operators.Eval(),
		"defmacro" : special_operators.Defmacro()
	}
	
	def tokenizerToInterpreterCons(self, tokenizerCons):
		if tokenizerCons==tokenizer.INT:
			return INT
		
	
	def read(self, text):
		form = List()
		text = self.readForm(form, text, False)[1]
		if tokenizer.nextToken(text)[0].tokenId != tokenizer.EOF:
			raise BadInputException("Syntax error")
		return form.children[0]

	# maximum x means that after loading x element function returns
	# useful eg. when implementing "'" bevaviour, "'a b" should translate to "(quote a) b". So after loading quote we want load just one element nevertheless there is no ")" in original string
	# maximum -1 means we want return from readForm onlu if there is EOF or ")" appears in original string
	def readForm(self, parent, text, openedParenthesis, maximum=-1):
		(token, text) = tokenizer.nextToken(text)
		while token.tokenId != tokenizer.EOF:
			if token.tokenId == tokenizer.SYNTAX_ERROR:
				raise BadInputException("Syntax error")
			
			if token.tokenId == tokenizer.QUOTE or token.tokenId == tokenizer.BACKQUOTE or token.tokenId == tokenizer.COMMA or token.tokenId == tokenizer.HASH or token.tokenId == COMMA_AT:
				newForm = List()
				newForm.children.append(Symbol(token.value))
				text = self.readForm(newForm, text, False, 1)[1]
				parent.children.append(newForm)
			else:
				newForm = LispForm.createLispForm(token.tokenId, token.value)
				
				if token.tokenId == tokenizer.OPENING_PARENTHESIS:
					text = self.readForm(newForm, text, True)[1]
					parent.children.append(newForm)
				elif token.tokenId == tokenizer.CLOSING_PARENTHESIS:
					if not(openedParenthesis):
						raise BadInputException("Unexpected ')'")
					return (newForm, text)
				elif token.tokenId == tokenizer.INT:
					newForm.value = int(newForm.value)
					parent.children.append(newForm)
				else:
					parent.children.append(newForm)
			
			maximum -= 1
			if maximum == 0:
				return (newForm, text)
			(token, text) = tokenizer.nextToken(text)	
		
		if openedParenthesis:
			raise BadInputException("Unexpected end of file")
		return (newForm, text)

	def evalExpr(self, text, variables):
		form = self.read(text)
		return form.evaluate(Environment(Env(variables, self.funDict), Env({}, {})))
			
	def evalExpression(self, text):
		return self.evalExpr(text, {"NIL" : getNil(), "T" : Symbol("T")})

class LispForm(object):
	operatorsDict = Interpreter.operatorsDict
	def __init__(self, value):
		self.children = []
		self.value = value
		
	@staticmethod
	def createLispForm(type, value):
		if type == SYMBOL:
			return Symbol(value)
		elif type == LIST:
			return List()
		elif type == INT:
			return Number(value)
		elif type == STRING:
			return String(value)
		
	def evaluate(self, env, **rest):
		return LispForm(self.value)
			
	def evaluateIfComma(self, env, **rest):
		return [LispForm(self.value)]
	
	def getValue(self):
		return self.value
		
	def call(self, params, env, **rest):
		raise BadInputException("The first element of list should be a symbol\n")
		
	def isNil(self):
		# all cases when it's true are implemented in children classes
		return False
	
def evaluateParams(params, env, **rest):
	evaluated = []
	for param in params:
		evaluated.append(param.evaluate(env, **rest))
	return evaluated
		
class List(LispForm):
	def __init__(self):
		super(List, self).__init__("(")
	
	def evaluate(self, env, **rest):
		if len(self.children) == 0:
			return getNil()
			
		if self.children[0].getType() == tokenizer.OPENING_PARENTHESIS:
			firstChild = self.children[0].evaluate(env, **rest)
		else:
			firstChild = self.children[0]

		if firstChild.getType() == tokenizer.SYMBOL:
			if firstChild.value in self.operatorsDict:
				return self.operatorsDict[firstChild.value].evaluate(self.children[1:], env, **rest)
			else:
				fun = env.getFunction(firstChild.value)
				if fun:
					return fun.funcall(evaluateParams(self.children[1:], env, **rest))
				else:
					print firstChild.value
					raise BadInputException("The function " + firstChild.value + " is undefined")
		elif firstChild.getType() == FUN_OBJ:
			return firstChild.funcall(evaluateParams(self.children[1:], env, **rest))
		else:
			raise BadInputException("The first element of list should be a symbol\n")
			
	def evaluateIfComma(self, env, **rest):
		print "list.evaluateIfComma"
		if self.children[0].getType() == tokenizer.SYMBOL and self.children[0].value == ",":
			return [self.operatorsDict[self.children[0].value].evaluate(self.children[1:], env, **rest)]
		elif self.children[0].getType() == tokenizer.SYMBOL and self.children[0].value == ",@":
			res = self.operatorsDict[self.children[0].value].evaluate(self.children[1:], env, **rest)
			return res.children
		else:
			res = List()
			for ch in self.children:
				res.children[len(res.children):] = (ch.evaluateIfComma(env, **rest))
			return [res]
			
	def getValue(self):
		if len(self.children) == 0:
			return "NIL"
		
		res = self.value
		for ch in self.children:
			if ch == self.children[0]:
				res += str(ch.getValue())
			else:
				res += " " + str(ch.getValue())
				
		res += ")"
		return res
	
	def isNil(self):
		return len(self.children) == 0
		
	def getType(self):
		return LIST
	
class Number(LispForm):
	def __init__(self, value):
		super(Number, self).__init__(value)
	
	def evaluate(self, env, **rest):
		return Number(self.value)
	
	def evaluateIfComma(self, env, **rest):
		return [Number(self.value)]
		
	def getType(self):
		return INT
	
class String(LispForm):
	def __init__(self, value):
		super(String, self).__init__(value)
	
	def evaluate(self, env, **rest):
		return String(self.value)
	
	def evaluateIfComma(self, env, **rest):
		return [String(self.value)]
		
	def getType(self):
		return STRING
		
class Symbol(LispForm):
	def evaluate(self, env, **rest):
		val = env.getVariable(self.value)
		if val:
			return val
		
		raise BadInputException("The variable " + self.value + " is unbound")
		
	def evaluateIfComma(self, env, **rest):
		if self.value == "," or self.value == ",@":
			return [self.operatorsDict[self.value].evaluateIfComma(self.children[1:], env, **rest)]
		return [Symbol(self.value)]
	
	def isNil(self):
		return self.value == "NIL"
	
	def getType(self):
		return SYMBOL

class Callable(LispForm):
	def __init__(self, name):
		super(Callable, self).__init__(name)
		
	def funcall(self, params):
		raise NotImplementedError( "Should have implemented this" )
		
class Function(Callable):
	def __init__(self, argNames, body, env, name):
		
		#searching for &rest
		self.restPresent = False
		for i in xrange(len(argNames)):
			if argNames[i] == "&rest":
				if i == len(argNames)-2:
					self.restPresent = True
					break
				elif i < len(argNames)-2:
					raise BadInputException("too many variables are following &rest")
				else:
					raise BadInputException("&rest with no rest variable")
				
		self.argNames = argNames
		if self.restPresent:
			self.argNames[len(self.argNames)-2] = self.argNames[len(self.argNames)-1]
			self.argNames.pop() # it cannot be done in one line
		self.body = body
		self.env = env
		super(Function, self).__init__(name)
		
	def call(self, env, params, **rest):
		return self.funcall(params)
	
	# It's a huge difference between Function.funcall and Function.evaluate.
	# Function.funcall is called when lambda is found in context like this: ((lambda (x) x) 3)
	# The most important thing to notice is two opening parenthesis followed by lambda(the second right after the first). Function.funcall actually call the lambda function.
	# Function.evaluate is called is called otherwise. It just returns Function objects.
	def funcall(self, params):
		if self.restPresent:
			if len(params) < len(self.argNames)-1:
				raise BadInputException("invalid number of arguments: " + str(len(params)))
			variables = dict(zip(self.argNames[0:len(self.argNames)-1], params[0:len(self.argNames)-1]))
			variables[self.argNames[len(self.argNames)-1]] = List()
			variables[self.argNames[len(self.argNames)-1]].children = params[len(self.argNames)-1:]
		else:
			if len(params) != len(self.argNames):
				raise BadInputException("invalid number of arguments: " + str(len(params)))
			variables = dict(zip(self.argNames, params))
		newVariables = dict(self.env.lexicalEnv.variables.items() + variables.items()) #order is important - in the case of the same keys the values from variables will be taken
		newEnv = Environment(self.env.globalEnv, Env(newVariables, self.env.lexicalEnv.funDict))
		return self.body.evaluate(newEnv)
	
	def evaluate(self, params, **rest):
		return self
	
	def getValue(self):
		return self.value
	
	def getType(self):
		return FUN_OBJ
	