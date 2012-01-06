# -*- coding: utf-8 -*-

import interpreter

def plus(params):
	res = 0
	while len(params):
		res += params.pop(0).value
	return interpreter.ExprRes(interpreter.INT, res)

def minus(params):
	if len(params) == 0:
		raise interpreter.BadInputException("Invalid number of arguments: 0")
	
	res = params.pop(0).value
	while len(params):
		res -= params.pop(0).value
	return interpreter.ExprRes(interpreter.INT, res)
	
def mul(params):
	res = 1
	while len(params):
		res *= params.pop().value
	return interpreter.ExprRes(interpreter.INT, res)