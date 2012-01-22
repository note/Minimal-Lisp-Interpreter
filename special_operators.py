# -*- coding: utf-8 -*-
import interpreter
import functions

class IfOperator:
	def evaluate(self, params, env, **rest):
		if len(params) == 2 or len(params) == 3:
			res = params[0].evaluate(env)
			if res.type == interpreter.SYMBOL and res.value == "NIL":
				return params[2].evaluate(env)
			else:
				return params[1].evaluate(env)
		else:
			raise interpreter.BadInputException("Special operator 'if' expects 2 or 3 arguments (got " + str(len(params)) + " arguments)")
		
class Let:
	def evaluate(self, params, env, **rest):
		tmp = {}
		for initExpr in params[0].children:
			if len(initExpr.children) > 0:
				varName = initExpr.children[0]
				value = initExpr.children[1].evaluate(env)
			else:
				varName = initExpr
				value = interpreter.LispForm(interpreter.SYMBOL, "NIL")
			if varName.type != interpreter.SYMBOL:
				raise interpreter.BadInputException("Variable name " + varName.value + " is not a symbol")
			tmp[varName.value] = value
			
		newVariables = dict(env.lexicalEnv.variables.items() + tmp.items()) #order is important - in the case of the same keys the values from tmp will be taken
		newEnv = interpreter.Environment(env.globalEnv, interpreter.Env(newVariables, env.lexicalEnv.funDict))
		
		return params[1].evaluate(newEnv)
	
class Progn:
	def evaluate(self, params, env, **rest):
		while len(params)>1:
			params.pop(0).evaluate(env)
		
		if len(params)>0:
			return params.pop(0).evaluate(env)
		else:
			return interpreter.LispForm(interpreter.SYMBOL, "NIL")
		
class Setq:
	def evaluate(self, params, env, **rest):
		if len(params) % 2 == 1:
			raise interpreter.BadInputException("Invalid number of arguments for setq")
		
		key = None
		while len(params)>0:
			key = params.pop(0)
			if key.type != interpreter.SYMBOL:
				raise interpreter.BadInputException("Variable name " + key.value + " is not a symbol")
			env.lexicalEnv.variables[key.value] = params.pop(0).evaluate(env)
		
		if key:
			return env.lexicalEnv.variables[key.value]
		else:
			return intr.LispForm(interpreter.SYMBOL, "NIL")
		
class Quote:
	def evaluate(self, params, env, **rest):
		if len(params) == 1:
			return params[0]
		else:
			raise interpreter.BadInputException("Special operator 'quote' expects 1 argument (got " + str(len(params)) + " arguments)")
	
class List:
	def evaluate(self, params, env, **rest):
		form = interpreter.LispForm(interpreter.LIST, "(")
		for param in params:
			form.children.append(param.evaluate(env))
		return form
	
class Car:
	def evaluate(self, params, env, **rest):
		if len(params) != 1:
			raise interpreter.BadInputException("car expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
		
		res = params[0].evaluate(env)
		
		if res.type != interpreter.LIST: 
			raise interpreter.BadInputException("The value " + str(res.value) + " is not a list")
		
		if len(res.children) == 0:
			return interpreter.LispForm(interpreter.SYMBOL, "NIL")
		return res.children[0]
	
class Cdr:
	def evaluate(self, params, env, **rest):
		if len(params) != 1:
			raise interpreter.BadInputException("cdr expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
		
		res = params[0].evaluate(env)
		
		if res.type != interpreter.LIST:
			raise interpreter.BadInputException("The value " + str(res.value) + " is not a list")
		
		if len(res.children) < 2:
			return interpreter.LispForm(interpreter.SYMBOL, "NIL")
			
		form = interpreter.LispForm(interpreter.LIST, "(")
		form.children = res.children[1:]
		return form
	
class Eval:
	def evaluate(self, params, env, **rest):
		if len(params) != 1:
			raise interpreter.BadInputException("eval expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
		
		expr = params[0].evaluate(env)
		return expr.evaluate(env)
	
class Atom:
	def evaluate(self, params, env, **rest):
		if len(params) != 1:
			raise interpreter.BadInputException("atom expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
		
		res = params[0].evaluate(env)
		if res.type == interpreter.LIST:
			if len(res.children) == 0:
				return interpreter.LispForm(interpreter.SYMBOL, "T")
			else:
				return interpreter.LispForm(interpreter.SYMBOL, "NIL")
				
		return interpreter.LispForm(interpreter.SYMBOL, "T")
	
class Backquote:
	def evaluate(self, params, env, **rest):
		# this should never occur because backquote operator is implicitely added by interpreter. But it still can be useful for debugging purposes
		if len(params) != 1:
			raise interpreter.BadInputException("backquote expects " + str(1) + " argument (got " + str(len(params)) + " arguments)")
		
		if params[0].type != interpreter.LIST:
			return params[0]
		else:
			res = interpreter.LispForm(interpreter.LIST, "(")
			for ch in params[0].children:
				print ch.getValue()
			
			for ch in params[0].children:
				res.children.append(ch.evaluateIfComma(env, **rest))
			return res
		
class Comma:
	def evaluate(self, params, env, **rest):
		# this should never occur because comma operator is implicitely added by interpreter. But it still can be useful for debugging purposes
		if len(params) != 1:
			raise interpreter.BadInputException("comma expects " + str(1) + " argument (got " + str(len(params)) + " arguments)")
		
		if "calledByBackquote" in rest:
			raise interpreter.BadInputException("comma must occurs inside backquote block")
		
		return params[0].evaluate(env)
		
	def evaluateIfComma(self, params, env, **rest):
		print "EVALUATEIFCOMMA!!!!!"
		return self.evaluate(params, env, **rest)
		
class Lambda:
	def evaluate(self, params, env, fnName="#<FUNCTION LAMBDA>"):
		if len(params) != 2:
			raise interpreter.BadInputException("lambda expects " + str(2) + " arguments (got " + str(len(params)) + " arguments)")
		
		if params[0].type != interpreter.LIST:
			raise interpreter.BadInputException("missing lambda list")
		
		argNames = []
		for argName in params[0].children:
			if argName.type != interpreter.SYMBOL:
				raise interpreter.BadInputException("each element of lambda list is expected to be a symbol")
			
			argNames.append(argName.value)
		return interpreter.Function(argNames, params[1], env, fnName)
		
class Defun:
	def evaluate(self, params, env):
		if len(params) != 3:
			raise interpreter.BadInputException("defun expects " + str(3) + " arguments (got " + str(len(params)) + " arguments)")
		
		if params[0].type != interpreter.SYMBOL:
			raise interpreter.BadInputException("first element of defun is expected to be a symbol")
		
		env.globalEnv.funDict[params[0].value] = Lambda().evaluate(params[1:], env, "#<FUNCTION " + params[0].value + ">") # making copy is undesirable because defun change the global environment
		
		return interpreter.LispForm(interpreter.SYMBOL, params[0].value)
		
class Hash:
	def evaluate(self, params, env, **rest):
		if len(params) != 1:
			raise interpreter.BadInputException("# expects " + str(1) + " argument (got " + str(len(params)) + " arguments)")
		
		tmp = params[0].evaluate(env)
		if tmp.type != interpreter.SYMBOL:
			raise interpreter.BadInputException("first parameter of # should evaluate to function name")
		
		fn = env.getFunction(tmp.value)
		if not(fn):
			raise interpreter.BadInputException("first parameter of # should evaluate to function name")
		return fn.evaluate(env)

class Funcall:
	def evaluate(self, params, env, **rest):
		if len(params) < 1:
			raise interpreter.BadInputException("funcall expects at least one parameter")
				
		if params[0].type != interpreter.SYMBOL:
			fn = params[0].evaluate(env)
			if fn.type != interpreter.FUN_OBJ:
				raise interpreter.BadInputException("first parameter of funcall should evaluate to function")
		else:
			fn = env.getVariable(params[0].value)
			if not(fn):
				fn = env.getFunction(params[0].value)
				if not(fn):
					raise interpreter.BadInputException("cannot find function " + params[0].value)
	
		args = []
		for param in params[1:]:
			args.append(param.evaluate(env))
			
		return fn.funcall(args)