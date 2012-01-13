# -*- coding: utf-8 -*-
import interpreter

def ifOperator(params, env, funDict, operatorsDict):
	if len(params) == 2 or len(params) == 3:
		res = params[0].evaluate(env, funDict, operatorsDict)
		if res.type == interpreter.SYMBOL and res.value == "NIL":
			return params[2].evaluate(env, funDict, operatorsDict)
		else:
			return params[1].evaluate(env, funDict, operatorsDict)
	else:
		raise interpreter.BadInputException("Special operator 'if' expects 2 or 3 arguments (got " + str(len(params)) + " arguments)")
		
def let(params, env, funDict, operatorsDict):
	tmp = {}
	for initExpr in params[0].children:
		if len(initExpr.children) > 0:
			varName = initExpr.children[0]
			value = initExpr.children[1].evaluate(env, funDict, operatorsDict)
		else:
			varName = initExpr
			value = interpreter.LispForm(interpreter.SYMBOL, "NIL")
		if varName.type != interpreter.SYMBOL:
			raise interpreter.BadInputException("Variable name " + varName.value + " is not a symbol")
		tmp[varName.value] = value
		
	newEnv = dict(env.items() + tmp.items())
	
	return params[1].evaluate(newEnv, funDict, operatorsDict)
	
def progn(params, env, funDict, operatorsDict):
	while len(params)>1:
		params.pop(0).evaluate(env, funDict, operatorsDict)
	
	if len(params)>0:
		return params.pop(0).evaluate(env, funDict, operatorsDict)
	else:
		return interpreter.LispForm(interpreter.SYMBOL, "NIL")
		
def setq(params, env, funDict, operatorsDict):
	if len(params) % 2 == 1:
		raise interpreter.BadInputException("Invalid number of arguments for setq")
	
	key = None
	while len(params)>0:
		key = params.pop(0)
		if key.type != interpreter.SYMBOL:
			raise interpreter.BadInputException("Variable name " + key.value + " is not a symbol")
		env[key.value] = params.pop(0).evaluate(env, funDict, operatorsDict)
	
	if key:
		return env[key.value]
	else:
		return intr.LispForm(interpreter.SYMBOL, "NIL")
		
def quote(params, env, funDict, operatorsDict):
	if len(params) == 1:
		return params[0]
	else:
		raise interpreter.BadInputException("Special operator 'quote' expects 1 argument (got " + str(len(params)) + " arguments)")
	
def list(params, env, funDict, operatorsDict):
	form = interpreter.LispForm(interpreter.LIST, "(")
	for param in params:
		form.children.append(param.evaluate(env, funDict, operatorsDict))
	return form
	
def car(params, env, funDict, operatorsDict):
	if len(params) != 1:
		raise interpreter.BadInputException("car expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
	
	res = params[0].evaluate(env, funDict, operatorsDict)
	
	if res.type != interpreter.LIST: 
		raise interpreter.BadInputException("The value " + str(res.value) + " is not a list")
	
	if len(res.children) == 0:
		return interpreter.LispForm(interpreter.SYMBOL, "NIL")
	return res.children[0]
	
def cdr(params, env, funDict, operatorsDict):
	if len(params) != 1:
		raise interpreter.BadInputException("cdr expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
	
	res = params[0].evaluate(env, funDict, operatorsDict)
	
	if res.type != interpreter.LIST:
		raise interpreter.BadInputException("The value " + str(res.value) + " is not a list")
	
	if len(res.children) < 2:
		return interpreter.LispForm(interpreter.SYMBOL, "NIL")
		
	form = interpreter.LispForm(interpreter.LIST, "(")
	form.children = res.children[1:]
	return form
	
def eval(params, env, funDict, operatorsDict):
	if len(params) != 1:
		raise interpreter.BadInputException("eval expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
	
	expr = params[0].evaluate(env, funDict, operatorsDict)
	return expr.evaluate(env, funDict, operatorsDict)
	
def atom(params, env, funDict, operatorsDict):
	if len(params) != 1:
		raise interpreter.BadInputException("atom expects " + 1 + " argument (got " + str(len(params)) + " arguments)")
	
	res = params[0].evaluate(env, funDict, operatorsDict)
	if res.type == interpreter.LIST:
		if len(res.children) == 0:
			return interpreter.LispForm(interpreter.SYMBOL, "T")
		else:
			return interpreter.LispForm(interpreter.SYMBOL, "NIL")
			
	return interpreter.LispForm(interpreter.SYMBOL, "T")