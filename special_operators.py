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