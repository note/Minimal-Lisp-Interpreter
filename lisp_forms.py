# -*- coding: utf-8 -*-

from interpreter_exceptions import InterpreterException, BadInputException
from interpreter_consts import *
import special_operators
from environment import *
from helpers import *

class LispForm(object):
	operatorsDict = special_operators.operatorsDict
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
		
	def isNil(self):
		# all cases when it's true are implemented in children classes
		return False
			
class List(LispForm):
	def __init__(self):
		super(List, self).__init__("(")
		
	def evaluateParams(self, params, env, **rest):
		evaluated = []
		for param in params:
			evaluated.append(param.evaluate(env, **rest))
		return evaluated
		
	def callObject(self, obj, params, env, **rest):
		if obj.getType() == FUN_OBJ:
			return obj.funcall(self.evaluateParams(params, env, **rest))
		elif obj.getType() == MACRO_OBJ:
			return obj.funcall(params).evaluate(env, **rest)
		else:
			raise InterpreterException("Parameter to callObject should be Function or Macro")
		
	
	def evaluate(self, env, **rest):
		if len(self.children) == 0:
			return getNil()
			
		if self.children[0].getType() == OPENING_PARENTHESIS:
			firstChild = self.children[0].evaluate(env, **rest)
		else:
			firstChild = self.children[0]

		if firstChild.getType() == SYMBOL:
			if firstChild.value in self.operatorsDict:
				return self.operatorsDict[firstChild.value].evaluate(self.children[1:], env, **rest)
			else:
				fun = env.getFunction(firstChild.value)
				if fun:
					return self.callObject(fun, self.children[1:], env, **rest)					
				else:
					raise BadInputException("The function " + firstChild.value + " is undefined")
		elif firstChild.getType() == FUN_OBJ:
			return self.callObject(firstChild, self.children[1:], env, **rest)
		else:
			print str(self.children[0].value) + ";;;;"
			raise BadInputException("The first element of list should be a symbol\n")
			
	def evaluateIfComma(self, env, **rest):
		if len(self.children) and self.children[0].getType() == SYMBOL and self.children[0].value == ",":
			return [self.operatorsDict[self.children[0].value].evaluate(self.children[1:], env, **rest)]
		elif len(self.children) and self.children[0].getType() == SYMBOL and self.children[0].value == ",@":
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
		
class Function(LispForm):
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
		self.env = env.getCopy()
		super(Function, self).__init__(name)
	
	# It's a huge difference between Function.funcall and Function.evaluate.
	# Function.funcall is called when lambda is found in context like this: ((lambda (x) x) 3)
	# The most important thing to notice is two opening parenthesis followed by lambda(the second right after the first). Function.funcall actually call the lambda function.
	# Function.evaluate is called is called otherwise. It just returns Function objects.
	def funcall(self, params):
		env = self.env.getCopy()
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
		
		env.overwriteLexicalVariables(variables)
		res = self.body.evaluate(env)
		return res
	
	def evaluate(self, params, **rest):
		return self
	
	def getValue(self):
		return self.value
	
	def getType(self):
		return FUN_OBJ

class Macro(Function):
	def __init__(self, argNames, body, env, name):
		super(Macro, self).__init__(argNames, body, env, name)
		
	def getType(self):
		return MACRO_OBJ