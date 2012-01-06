# -*- coding: utf-8 -*-
import interpreter

def ifOperator(params, env):
	print "inside ifOperator"
	print params
	print "end of invocation"
	if len(params) == 2 or len(params) == 3:
		res = interpreter.evalExpr(params[0], env)[0]
		if res.type == interpreter.SYMBOL and res.value == "NIL":
			print "NIL\n"
			return interpreter.evalExpr(params[2], env)[0]
		else:
			print "NOT NIL\n"
			return interpreter.evalExpr(params[1], env)[0]
	else:
		print "Special operator 'if' expects 2 or 3 arguments (got " + str(len(params)) + " arguments)"
		
def let(params, env):
	print "\n------------------------LET\n";
	print params[0]
	print params[1]

	v = interpreter.loadInitializationList(params[0], env)[0]
	newEnv = dict(env.items() + v.items())
	print "after"
	print params[1]

	print "---------------------------let end\n\n"
	for k, v in newEnv.iteritems():
		print k + ": " + str(v.value)
	print "---------------------------let end\n\n"
	return interpreter.evalExpr(params[1], newEnv)[0]