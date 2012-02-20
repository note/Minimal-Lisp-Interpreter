# -*- coding: utf-8 -*-
import interpreter
import functions

class IfOperator:
	def evaluate(self, params, env, **rest):
		if len(params) == 2 or len(params) == 3:
			res = params[0].evaluate(env)
			if res.getType() == interpreter.SYMBOL and res.value == "NIL":
				if len(params) == 3:
					return params[2].evaluate(env)
				else:
					return interpreter.getEmptyList()
			else:
				return params[1].evaluate(env)
		else:
			raise interpreter.BadInputException("Special operator 'if' expects 2 or 3 arguments (got " + str(len(params)) + " arguments)")
		
class Let:
	def evaluate(self, params, env, **rest):
		if len(params) < 1:
			raise interpreter.BadInputException("let expects at least one argument (got " + str(len(params)) + " arguments)")
		
		tmp = {}
		for initExpr in params[0].children:
			if len(initExpr.children) > 0:
				varName = initExpr.children[0]
				value = initExpr.children[1].evaluate(env)
			else:
				varName = initExpr
				value = interpreter.getNil()
			if varName.getType() != interpreter.SYMBOL:
				raise interpreter.BadInputException("Variable name " + varName.value + " is not a symbol")
			tmp[varName.value] = value
			
		newVariables = dict(env.lexicalEnv.variables.items() + tmp.items()) #order is important - in the case of the same keys the values from tmp will be taken
		newEnv = interpreter.Environment(env.globalEnv, interpreter.Env(newVariables, env.lexicalEnv.funDict))
		
		res = interpreter.getNil()
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
			return interpreter.getNil()
		
class Setq:
	def evaluate(self, params, env, **rest):
		if len(params) % 2 == 1:
			raise interpreter.BadInputException("Invalid number of arguments for setq")
		
		key = None
		while len(params)>0:
			key = params.pop(0)
			if key.getType() != interpreter.SYMBOL:
				raise interpreter.BadInputException("Variable name " + key.value + " is not a symbol")
			env.lexicalEnv.variables[key.value] = params.pop(0).evaluate(env)
		
		if key:
			return env.lexicalEnv.variables[key.value]
		else:
			return intr.getNil()
		
class Quote:
	def evaluate(self, params, env, **rest):
		if len(params) == 1:
			return params[0]
		else:
			raise interpreter.BadInputException("Special operator 'quote' expects 1 argument (got " + str(len(params)) + " arguments)")
	
class List:
	def evaluate(self, params, env, **rest):
		form = interpreter.List()
		for param in params:
			form.children.append(param.evaluate(env))
		return form
		
class Cons:
	def evaluate(self, params, env, **rest):
		if len(params) != 2:
			raise interpreter.BadInputException("cons expects 2 arguments (got " + str(len(params)) + " arguments)")
		
		res = params[1].evaluate(env)
		if res.isNil():
			res = interpreter.getEmptyList()
			
		if res.getType() != interpreter.LIST:
			raise interpreter.BadInputException("second parameter of cons is expected to be a list")
		
		res.children.insert(0, params[0].evaluate(env))
		return res
		
	
class Car:
	def evaluate(self, params, env, **rest):
		if len(params) != 1:
			raise interpreter.BadInputException("car expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
		
		res = params[0].evaluate(env)
		
		if res.isNil():
			res = interpreter.getEmptyList()
		
		if res.getType() != interpreter.LIST: 
			raise interpreter.BadInputException("The value " + str(res.value) + " is not a list")
		
		if len(res.children) == 0:
			return interpreter.getNil()
		return res.children[0]
	
class Cdr:
	def evaluate(self, params, env, **rest):
		if len(params) != 1:
			raise interpreter.BadInputException("cdr expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
		
		res = params[0].evaluate(env)
		
		if res.getType() != interpreter.LIST:
			raise interpreter.BadInputException("The value " + str(res.value) + " is not a list")
		
		if len(res.children) < 2:
			return interpreter.getNil()
			
		form = interpreter.List()
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
		if res.getType() == interpreter.LIST:
			if len(res.children) == 0:
				return interpreter.Symbol("T")
			else:
				return interpreter.getNil()
				
		return interpreter.Symbol("T")
	
class Backquote:
	def evaluate(self, params, env, **rest):
		# this should never occur because backquote operator is implicitely added by interpreter. But it still can be useful for debugging purposes
		if len(params) != 1:
			raise interpreter.BadInputException("backquote expects " + str(1) + " argument (got " + str(len(params)) + " arguments)")
		
		if params[0].getType() != interpreter.LIST:
			return params[0]
		else:
			res = interpreter.List()
			
			for ch in params[0].children:
				res.children[len(res.children):] = ch.evaluateIfComma(env, **rest)
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
		return self.evaluate(params, env, **rest)
		
class Lambda:
	def evaluate(self, params, env, fnName="#<FUNCTION LAMBDA>"):
		if len(params) != 2:
			raise interpreter.BadInputException("lambda expects " + str(2) + " arguments (got " + str(len(params)) + " arguments)")
		
		if params[0].getType() != interpreter.LIST:
			raise interpreter.BadInputException("missing lambda list")
		
		argNames = []
		for argName in params[0].children:
			if argName.getType() != interpreter.SYMBOL:
				raise interpreter.BadInputException("each element of lambda list is expected to be a symbol")
			argNames.append(argName.value)
			
		return interpreter.Function(argNames, params[1], env, fnName)
		
class Defun:
	def evaluate(self, params, env):
		if len(params) != 3:
			raise interpreter.BadInputException("defun expects " + str(3) + " arguments (got " + str(len(params)) + " arguments)")
		
		if params[0].getType() != interpreter.SYMBOL:
			raise interpreter.BadInputException("first element of defun is expected to be a symbol")
		
		env.globalEnv.funDict[params[0].value] = Lambda().evaluate(params[1:], env, "#<FUNCTION " + params[0].value + ">") # making copy is undesirable because defun change the global environment
		
		return interpreter.Symbol(params[0].value)

class Defmacro:
	def evaluate(self, params, env, **rest):
		if len(params) != 3:
			raise interpreter.BadInputException("defmacro expects three arguments(got " + str(len(params)) + " arguments)")
			
		if params[0].getType() != interpreter.SYMBOL:
			raise interpreter.BadInputException("first element of defun is expected to be a symbol")
			
		if params[1].getType() != interpreter.LIST:
			raise interpreter.BadInputException("missing lambda list")
		
		argNames = []
		for argName in params[1].children:
			if argName.getType() != interpreter.SYMBOL:
				raise interpreter.BadInputException("each element of lambda list is expected to be a symbol")
			argNames.append(argName.value)
		
		env.globalEnv.funDict[params[0].value] = Lambda().evaluate(params[1:], env, "#<MACRO " + params[0].value + ">") # making copy is undesirable because defmacro change the global environment
		
		return interpreter.Symbol(params[0].value)	

class Hash:
	def evaluate(self, params, env, **rest):
		if len(params) != 1:
			raise interpreter.BadInputException("# expects " + str(1) + " argument (got " + str(len(params)) + " arguments)")
		
		tmp = params[0].evaluate(env)
		if tmp.getType() != interpreter.SYMBOL:
			raise interpreter.BadInputException("first parameter of # should evaluate to function name")
		
		fn = env.getFunction(tmp.value)
		if not(fn):
			raise interpreter.BadInputException("first parameter of # should evaluate to function name")
		return fn.evaluate(env)

class Funcall:
	def evaluate(self, params, env, **rest):
		if len(params) < 1:
			raise interpreter.BadInputException("funcall expects at least one parameter")
				
		if params[0].getType() != interpreter.SYMBOL:
			fn = params[0].evaluate(env)
			if fn.getType() != interpreter.FUN_OBJ:
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
		
class Eval:
	def evaluate(self, params, env, **rest):
		if len(params) != 1:
			raise interpreter.BadInputException("eval expects one argument (got " + str(len(params)) + " arguments)")
		
		tmp = params[0].evaluate(env)
		print "EVAL"
		print tmp.getValue()
		return tmp.evaluate(env)
	
	