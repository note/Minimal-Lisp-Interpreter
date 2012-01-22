# -*- coding: utf-8 -*-

import interpreter

class Plus:
	def funcall(self, params):
		res = 0
		while len(params):
			res += params.pop(0).value
		return interpreter.LispForm(interpreter.INT, res)

class Minus:
	def funcall(self, params):
		if len(params) == 0:
			raise interpreter.BadInputException("Invalid number of arguments: 0")
		
		res = params.pop(0).value
		while len(params):
			res -= params.pop(0).value
		return interpreter.LispForm(interpreter.INT, res)
	
class Mul:
	def funcall(self, params):
		res = 1
		while len(params):
			res *= params.pop().value
		return interpreter.LispForm(interpreter.INT, res)
		
class Equal:
	def funcall(self, params):
		for param in params:
			if param.value != params[0].value:
				return interpreter.LispForm(interpreter.SYMBOL, "NIL")
		return interpreter.LispForm(interpreter.SYMBOL, "T")