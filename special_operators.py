# -*- coding: utf-8 -*-
import interpreter

def ifOperator(params, env):
	if len(params) == 2 or len(params) == 3:
		res = interpreter.evalExpr(params[0], env)[0]
		if res.type == interpreter.SYMBOL and res.value == "NIL":
			return interpreter.evalExpr(params[2], env)[0]
		else:
			return interpreter.evalExpr(params[1], env)[0]
	else:
		raise interpreter.BadInputException("Special operator 'if' expects 2 or 3 arguments (got " + str(len(params)) + " arguments)")
		
def let(params, env):
	v = interpreter.loadInitializationList(params[0], env)[0]
	newEnv = dict(env.items() + v.items())
	
	return interpreter.evalExpr(params[1], newEnv)[0]
	
def progn(params, env):
	while len(params)>1:
		interpreter.evalExpr(params.pop(0), env)
	
	if len(params)>0:
		return interpreter.evalExpr(params.pop(), env)[0]
	else:
		return interpreter.ExprRes(SYMBOL, "NIL")
		
def setq(params, env):
	if len(params) % 2 == 1:
		raise interpreter.BadInputException("Invalid number of arguments for setq")
	
	print params
	print "before"
	print env
	
	key = None
	while len(params)>0:
		key = params.pop(0)
		env[key] = interpreter.evalExpr(params.pop(0), env)[0]
	
	print "after"
	print env
	
	if key:
		return env[key]
	else:
		return interpreter.ExprRes(SYMBOL, "NIL")