# -*- coding: utf-8 -*-

import functions
import lisp_forms
from interpreter_exceptions import BadInputException, InterpreterException
from environment import *
from helpers import *
from interpreter_consts import *

def checkNumberOfParams(expected, actual, operatorName):
	if expected != actual:
		raise BadInputException("Special operator " + operatorName + " expects " + str(expected) + " arguments (got " + str(actual) + " arguments")
	return True	

class IfOperator:
	def evaluate(self, params, env, **rest):
		if len(params) == 2 or len(params) == 3:
			res = params[0].evaluate(env)
			if res.getType() == SYMBOL and res.value == "NIL":
				if len(params) == 3:
					return params[2].evaluate(env)
				else:
					return getEmptyList()
			else:
				return params[1].evaluate(env)
		else:
			raise BadInputException("Special operator 'if' expects 2 or 3 arguments (got " + str(len(params)) + " arguments)")
		
class Let:
	def evaluate(self, params, env, **rest):
		if len(params) < 1:
			raise BadInputException("let expects at least one argument (got " + str(len(params)) + " arguments)")
		
		newVariables = {}
		for initExpr in params[0].children:
			if len(initExpr.children) > 0:
				varName = initExpr.children[0]
				value = initExpr.children[1].evaluate(env)
			else:
				varName = initExpr
				value = getNil()
			if varName.getType() != SYMBOL:
				raise BadInputException("Variable name " + str(varName.value) + " is not a symbol")
			newVariables[varName.value] = value
		
		newEnv = env.getCopy()	
		newEnv.overwriteLexicalVariables(newVariables)
		res = getNil()
		for param in params[1:]:
			res = param.evaluate(newEnv)
		return res
	
class Progn:
	def evaluate(self, params, env, **rest):
		while len(params)>1:
			params.pop(0).evaluate(env)
		
		if len(params)>0:
			return params.pop(0).evaluate(env)
		else:
			return getNil()
		
class Setq:
	def evaluate(self, params, env, **rest):
		if len(params) % 2 == 1:
			raise BadInputException("Invalid number of arguments for setq")
		
		key = None
		while len(params)>0:
			key = params.pop(0)
			if key.getType() != SYMBOL:
				raise BadInputException("Variable name " + key.value + " is not a symbol")
			env.setLexicalVariable(key.value, params.pop(0).evaluate(env))
		
		if key:
			return env.getVariable(key.value)
		else:
			return intr.getNil()
		
class Quote:
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(1, len(params), "quote"):
			return params[0]
	
class List:
	def evaluate(self, params, env, **rest):
		form = lisp_forms.List()
		for param in params:
			form.children.append(param.evaluate(env))
		return form
		
class Cons:
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(2, len(params), "cons"):
			res = params[1].evaluate(env)
			if res.isNil():
				res = getEmptyList()
				
			if res.getType() != LIST:
				raise BadInputException("second parameter of cons is expected to be a list")
			
			res.children.insert(0, params[0].evaluate(env))
			return res
	
class Car:
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(1, len(params), "car"):
			res = params[0].evaluate(env)
			
			if res.isNil():
				res = getEmptyList()
			
			if res.getType() != LIST: 
				raise BadInputException("The value " + str(res.value) + " is not a list")
			
			if len(res.children) == 0:
				return getNil()
			return res.children[0]
	
class Cdr:
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(1, len(params), "cdr"):
			res = params[0].evaluate(env)
			
			if res.getType() != LIST:
				raise BadInputException("The value " + str(res.value) + " is not a list")
			
			if len(res.children) < 2:
				return getNil()
				
			form = lisp_forms.List()
			form.children = res.children[1:]
			return form
	
class Eval:
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(1, len(params), "eval"):
			expr = params[0].evaluate(env)
			return expr.evaluate(env)
	
class Atom:
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(1, len(params), "atom"):
			res = params[0].evaluate(env)
			if res.getType() == LIST:
				if len(res.children) == 0:
					return lisp_forms.Symbol("T")
				else:
					return getNil()
					
			return lisp_forms.Symbol("T")
	
class Backquote:
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(1, len(params), "backquote"):
			if params[0].getType() != LIST:
				return params[0]
			else:
				res = lisp_forms.List()
				
				for ch in params[0].children:
					res.children[len(res.children):] = ch.evaluateIfComma(env, **rest)
				return res
		
class Comma:
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(1, len(params), "comma"):
			if "calledByBackquote" in rest:
				raise BadInputException("comma must occurs inside backquote block")
			
			return params[0].evaluate(env)
		
	def evaluateIfComma(self, params, env, **rest):
		return self.evaluate(params, env, **rest)
		
class Lambda:
	def evaluate(self, params, env, fnName="#<FUNCTION LAMBDA>"):
		if checkNumberOfParams(2, len(params), "lambda"):
			if params[0].getType() != LIST:
				raise BadInputException("missing lambda list")
			
			argNames = []
			for argName in params[0].children:
				if argName.getType() != SYMBOL:
					raise BadInputException("each element of lambda list is expected to be a symbol")
				argNames.append(argName.value)
				
			return lisp_forms.Function(argNames, params[1], env, fnName)
	
class Defun:
	def evaluate(self, params, env):
		if checkNumberOfParams(3, len(params), "defun"):
			if params[0].getType() != SYMBOL:
				raise BadInputException("first element of defun is expected to be a symbol")
			
			env.globalEnv.funDict[params[0].value] = Lambda().evaluate(params[1:], env, "#<FUNCTION " + params[0].value + ">") # making copy is undesirable because defun change the global environment
			
			return lisp_forms.Symbol(params[0].value)

class Defmacro:
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(3, len(params), "defmacro"):
			if params[0].getType() != SYMBOL:
				raise BadInputException("first element of defun is expected to be a symbol")
				
			if params[1].getType() != LIST:
				raise BadInputException("missing lambda list")
			
			argNames = []
			for argName in params[1].children:
				if argName.getType() != SYMBOL:
					raise BadInputException("each element of lambda list is expected to be a symbol")
				argNames.append(argName.value)
			
			env.globalEnv.funDict[params[0].value] = lisp_forms.Macro(argNames, params[2], env, params[0].value) # making copy is undesirable because defmacro change the global environment
			
			return lisp_forms.Symbol(params[0].value)

class Hash:
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(1, len(params), "hash"):
			tmp = params[0].evaluate(env)
			if tmp.getType() != SYMBOL:
				raise BadInputException("first parameter of # should evaluate to function name")
			
			fn = env.getFunction(tmp.value)
			if not(fn):
				raise BadInputException("first parameter of # should evaluate to function name")
			return fn.evaluate(env)

class Funcall:
	def evaluate(self, params, env, **rest):
		if len(params) < 1:
			raise BadInputException("funcall expects at least one parameter")
				
		if params[0].getType() != SYMBOL:
			fn = params[0].evaluate(env)
			if fn.getType() != FUN_OBJ:
				raise BadInputException("first parameter of funcall should evaluate to function")
		else:
			fn = env.getVariable(params[0].value)
			if not(fn):
				fn = env.getFunction(params[0].value)
				if not(fn):
					raise BadInputException("cannot find function " + params[0].value)
	
		args = []
		for param in params[1:]:			
			args.append(param.evaluate(env))
			
		return fn.funcall(args)
		
class Eval:
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(1, len(params), "cdr"):
			tmp = params[0].evaluate(env)
			return tmp.evaluate(env)
			
class Gensym:
	i = 0
	def evaluate(self, params, env, **rest):
		if checkNumberOfParams(0, len(params), "gensym"):
			res = lisp_forms.Symbol("__##G" + str(Gensym.i))
			Gensym.i += 1
			return res
			

operatorsDict = {
    "if": IfOperator(),
    "let": Let(),
    "progn": Progn(),
    "setq" : Setq(),
    "quote" : Quote(),
    "'" : Quote(),
    #"list" : List(),
    "car" : Car(),
    "cdr" : Cdr(),
    "eval" : Eval(),
    "atom" : Atom(),
    "`" : Backquote(),
    "," : Comma(),
    ",@" : Comma(),
    "lambda" : Lambda(),
    "defun" : Defun(),
    "#" : Hash(),
    "funcall" : Funcall(),
    "cons" : Cons(),
    "eval" : Eval(),
    "defmacro" : Defmacro(),
    "gensym" : Gensym()
}	
	