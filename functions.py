# -*- coding: utf-8 -*-

import interpreter

class Function:
	def __init__(self, argNames, body):
		self.argNames = argNames
		self.body = body
	
	def evaluate(self, params, env):
		variables = dict(zip(self.argNames, params))
		newVariables = dict(env.variables.items() + variables.items()) #order is important - in the case of the same keys the values from variables will be taken
		newEnv = interpreter.Environment(newVariables, env.funDict)
		return self.body.evaluate(newEnv)

def plus(params):
	res = 0
	while len(params):
		res += params.pop(0).value
	return interpreter.LispForm(interpreter.INT, res)

def minus(params):
	if len(params) == 0:
		raise interpreter.BadInputException("Invalid number of arguments: 0")
	
	res = params.pop(0).value
	while len(params):
		res -= params.pop(0).value
	return interpreter.LispForm(interpreter.INT, res)
	
def mul(params):
	res = 1
	while len(params):
		res *= params.pop().value
	return interpreter.LispForm(interpreter.INT, res)