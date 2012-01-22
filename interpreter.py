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
COMMA = 11
HASH = 12
EOF = 13

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
		"list" : special_operators.List(),
		"car" : special_operators.Car(),
		"cdr" : special_operators.Cdr(),
		"eval" : special_operators.Eval(),
		"atom" : special_operators.Atom(),
		"`" : special_operators.Backquote(),
		"," : special_operators.Comma(),
		"lambda" : special_operators.Lambda(),
		"defun" : special_operators.Defun(),
		"#" : special_operators.Hash(),
		"funcall" : special_operators.Funcall()
	}
	
	def tokenizerToInterpreterCons(self, tokenizerCons):
		if tokenizerCons==tokenizer.INT:
			return INT
		
	
	def read(self, text):
		form = LispForm(LIST, "")
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
			
			if token.tokenId == tokenizer.QUOTE or token.tokenId == tokenizer.BACKQUOTE or token.tokenId == tokenizer.COMMA or token.tokenId == tokenizer.HASH:
				newForm = LispForm(LIST, "(")
				newForm.children.append(LispForm(SYMBOL, token.value))
				text = self.readForm(newForm, text, False, 1)[1]
				parent.children.append(newForm)
			else:
				newForm = LispForm(token.tokenId, token.value)
				
				if token.tokenId == tokenizer.OPENING_PARENTHESIS:
					text = self.readForm(newForm, text, True)[1]
					parent.children.append(newForm)
				elif token.tokenId == tokenizer.CLOSING_PARENTHESIS:
					if not(openedParenthesis):
						raise BadInputException("Unexpected ')'")
					return (newForm, text)
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
		return self.evalExpr(text, {"NIL" : LispForm(SYMBOL, "NIL"), "T" : LispForm(SYMBOL, "T")})


class LispForm(object):
	operatorsDict = Interpreter.operatorsDict
	def __init__(self, type, value):
		self.children = []
		self.type = type
		self.value = value
		
	def evaluate(self, env, **rest):
		if self.type == tokenizer.OPENING_PARENTHESIS:
			if self.children[0].type == tokenizer.OPENING_PARENTHESIS:
				firstChild = self.children[0].evaluate(env, **rest)
			else:
				firstChild = self.children[0]
				
			if firstChild.type == tokenizer.SYMBOL:
				if firstChild.value in self.operatorsDict:
					return self.operatorsDict[firstChild.value].evaluate(self.children[1:], env, **rest)
				else:
					fun = env.getFunction(firstChild.value)
					if fun:
						params = []
						for i in xrange(1, len(self.children)):
							params.append(self.children[i].evaluate(env, **rest))
						return fun.funcall(params)
					else:
						print firstChild.value
						raise BadInputException("The function " + firstChild.value + " is undefined")
			elif firstChild.type == FUN_OBJ:
				params = []
				for i in xrange(1, len(self.children)):
					params.append(self.children[i].evaluate(env, **rest))
				return firstChild.funcall(params)
			else:
				raise BadInputException("The first element of list should be a symbol\n")
		elif self.type == tokenizer.INT:
			return LispForm(self.type, int(self.value))
		elif self.type == tokenizer.SYMBOL:
			val = env.getVariable(self.value)
			if val:
				return val
			print "+++++WARNING"
			print self.value
			raise BadInputException("The variable " + self.value + " is unbound")
		else:
			return LispForm(self.type, self.value)
		
	def evaluateIfComma(self, env, **rest):
		if self.type == tokenizer.SYMBOL and self.value == ",":
			return self.operatorsDict[self.value].evaluateIfComma(self.children[1:], env, rest)
		elif self.type == tokenizer.OPENING_PARENTHESIS:
			if self.children[0].type == tokenizer.SYMBOL and self.children[0].value == ",":
				return self.operatorsDict[self.children[0].value].evaluate(self.children[1:], env, **rest)
			else:
				res = LispForm(self.type, self.value)
				for ch in self.children:
					res.children.append(ch.evaluateIfComma(env, **rest))
				return res
		elif self.type == tokenizer.INT or self.type == tokenizer.SYMBOL:
			return LispForm(self.type, self.value)
				
	
	def getValue(self):
		res = self.value
		for ch in self.children:
			if self.type == tokenizer.OPENING_PARENTHESIS and ch == self.children[0]:
				res += str(ch.getValue())
			else:
				res += " " + str(ch.getValue())
		if self.type == tokenizer.OPENING_PARENTHESIS:
			res += ")"
		return res
		
	def getType(self):
		return self.type
		
class Function(LispForm):
	def __init__(self, argNames, body, env, name):
		self.argNames = argNames
		self.body = body
		self.env = env
		super(Function, self).__init__(FUN_OBJ, name)
	
	# It's a huge difference between Function.funcall and Function.evaluate.
	# Function.funcall is called when lambda is found in context like this: ((lambda (x) x) 3)
	# The most important thing to notice is two opening parenthesis followed by lambda(the second right after the first). Function.funcall actually call the lambda function.
	# Function.evaluate is called is called otherwise. It just returns Function objects.
	def funcall(self, params):
		variables = dict(zip(self.argNames, params))
		newVariables = dict(self.env.lexicalEnv.variables.items() + variables.items()) #order is important - in the case of the same keys the values from variables will be taken
		newEnv = Environment(self.env.globalEnv, Env(newVariables, self.env.lexicalEnv.funDict))
		return self.body.evaluate(newEnv)
	
	def evaluate(self, params, **rest):
		return self
	
	def getValue(self):
		return self.value
