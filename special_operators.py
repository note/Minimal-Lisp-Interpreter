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
		
def let(params, env, intr):
	v = intr.loadInitializationList(params[0], env)[0]
	newEnv = dict(env.items() + v.items())
	
	return intr.evalExpr(params[1], newEnv)[0]
	
def progn(params, env, intr):
	while len(params)>1:
		intr.evalExpr(params.pop(0), env)
	
	if len(params)>0:
		return intr.evalExpr(params.pop(), env)[0]
	else:
		return intr.ExprRes(interpreter.SYMBOL, "NIL")
		
def setq(params, env, intr):
	if len(params) % 2 == 1:
		raise interpreter.BadInputException("Invalid number of arguments for setq")
	
	print params
	print "before"
	print env
	
	key = None
	while len(params)>0:
		key = params.pop(0)
		env[key] = intr.evalExpr(params.pop(0), env)[0]
	
	print "after"
	print env
	
	if key:
		return env[key]
	else:
		return intr.ExprRes(interpreter.SYMBOL, "NIL")
		
def quote(params, env, intr):
	if len(params) == 1:
		return intr.ExprRes(interpreter.STRING, params[0])
	else:
		raise interpreter.BadInputException("Special operator 'quote' expects 1 argument (got " + str(len(params)) + " arguments)")