# -*- coding: utf-8 -*-

import lisp_forms
from helpers import *

class Plus(lisp_forms.Function):
	def __init__(self):
		pass
	
	def funcall(self, params):
		res = 0
		while len(params):
			res += params.pop(0).value
		return lisp_forms.Number(res)

class Minus(lisp_forms.Function):
	def __init__(self):
		pass	
	
	def funcall(self, params):
		if len(params) == 0:
			raise BadInputException("Invalid number of arguments: 0")
		
		res = params.pop(0).value
		while len(params):
			res -= params.pop(0).value
		return lisp_forms.Number(res)
	
class Mul(lisp_forms.Function):
	def __init__(self):
		pass		
	
	def funcall(self, params):
		res = 1
		while len(params):
			res *= params.pop().value
		return lisp_forms.Number(res)
		
class Equal(lisp_forms.Function):
	def __init__(self):
		pass		
	
	def funcall(self, params):
		for param in params:
			if param.value != params[0].value:
				return getNil()
		return lisp_forms.Number("T")