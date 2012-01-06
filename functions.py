# -*- coding: utf-8 -*-

import interpreter

def plus(params):
	res = 0
	while len(params):
		res += params.pop().value
	return interpreter.ExprRes(interpreter.INT, res)

def minus(params):
	res = 0
	while len(params):
		res -= params.pop().value
	return interpreter.ExprRes(interpreter.INT, res)
	
def mul(params):
	res = 1
	while len(params):
		res *= params.pop().value
	return interpreter.ExprRes(interpreter.INT, res)