# -*- coding: utf-8 -*-
import interpreter
		
def ifOperator(params, env):
	if len(params) == 2 or len(params) == 3:
		res = params[0].evaluate(env)
		if res.type == interpreter.SYMBOL and res.value == "NIL":
			return params[2].evaluate(env)
		else:
			return params[1].evaluate(env)
	else:
		raise interpreter.BadInputException("Special operator 'if' expects 2 or 3 arguments (got " + str(len(params)) + " arguments)")
		
def let(params, env):
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
		
	newVariables = dict(env.variables.items() + tmp.items())
	newEnv = interpreter.Environment(newVariables, env.funDict)
	
	return params[1].evaluate(newEnv)
	
def progn(params, env):
	while len(params)>1:
		params.pop(0).evaluate(env)
	
	if len(params)>0:
		return params.pop(0).evaluate(env)
	else:
		return interpreter.LispForm(interpreter.SYMBOL, "NIL")
		
def setq(params, env):
	if len(params) % 2 == 1:
		raise interpreter.BadInputException("Invalid number of arguments for setq")
	
	key = None
	while len(params)>0:
		key = params.pop(0)
		if key.type != interpreter.SYMBOL:
			raise interpreter.BadInputException("Variable name " + key.value + " is not a symbol")
		env.variables[key.value] = params.pop(0).evaluate(env)
	
	if key:
		return env.variables[key.value]
	else:
		return intr.LispForm(interpreter.SYMBOL, "NIL")
		
def quote(params, env):
	if len(params) == 1:
		return params[0]
	else:
		raise interpreter.BadInputException("Special operator 'quote' expects 1 argument (got " + str(len(params)) + " arguments)")
	
def list(params, env):
	form = interpreter.LispForm(interpreter.LIST, "(")
	for param in params:
		form.children.append(param.evaluate(env))
	return form
	
def car(params, env):
	if len(params) != 1:
		raise interpreter.BadInputException("car expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
	
	res = params[0].evaluate(env)
	
	if res.type != interpreter.LIST: 
		raise interpreter.BadInputException("The value " + str(res.value) + " is not a list")
	
	if len(res.children) == 0:
		return interpreter.LispForm(interpreter.SYMBOL, "NIL")
	return res.children[0]
	
def cdr(params, env):
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
	
def eval(params, env):
	if len(params) != 1:
		raise interpreter.BadInputException("eval expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
	
	expr = params[0].evaluate(env)
	return expr.evaluate(env)
	
def atom(params, env):
	if len(params) != 1:
		raise interpreter.BadInputException("atom expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
	
	res = params[0].evaluate(env)
	if res.type == interpreter.LIST:
		if len(res.children) == 0:
			return interpreter.LispForm(interpreter.SYMBOL, "T")
		else:
			return interpreter.LispForm(interpreter.SYMBOL, "NIL")
			
	return interpreter.LispForm(interpreter.SYMBOL, "T")
	
def backquote(params, env):
	# this should never occur because backquote operator is implicitely added by interpreter. But it still can be useful for debugging purposes
	if len(params) != 1:
		raise interpreter.BadInputException("backquote expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
	
	print "BACKQUOTE"
	if params[0].type != interpreter.LIST:
		return params[0]
	else:
		res = interpreter.LispForm(interpreter.LIST, "(")
		print "!! Lista"
		for ch in params[0].children:
			print ch.getValue()
		
		print "////"
		for ch in params[0].children:
			if ch.type == interpreter.SYMBOL and ch.value == ",":
				res.children.append(ch.evaluate(env, True))
			else:
				res.children.append(ch)
		return res
		
def comma(params, env, calledByBackquote=False):
	# this should never occur because comma operator is implicitely added by interpreter. But it still can be useful for debugging purposes
	if len(params) != 1:
		raise interpreter.BadInputException("comma expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
	print "here"
	if not(calledByBackquote):
		raise interpreter.BadInputeException("comma must occurs inside backquote block")
	
	return params[0].evaluate(env)