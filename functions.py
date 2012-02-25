# -*- coding: utf-8 -*-

import lisp_forms
from helpers import *

class Function(lisp_forms.Function):
	def __init__(self):
		pass
	
	def funcall(self, params):
		pass

class Plus(Function):	
	def funcall(self, params):
		res = 0
		while len(params):
			res += params.pop(0).value
		return lisp_forms.Number(res)

class Minus(Function):
	def funcall(self, params):
		if len(params) == 0:
			raise BadInputException("Invalid number of arguments: 0")
		
		res = params.pop(0).value
		while len(params):
			res -= params.pop(0).value
		return lisp_forms.Number(res)
	
class Mul(Function):
	def funcall(self, params):
		res = 1
		while len(params):
			res *= params.pop().value
		return lisp_forms.Number(res)
		
class Equal(Function):
	def funcall(self, params):
		for param in params:
			if param.value != params[0].value:
				return getNil()
		return lisp_forms.Number("T")
		
class Less(Function):
	def funcall(self, params):
		if len(params) < 1:
			raise BadInputException("invalid number of arguments: 0")
			
		for i in xrange(len(params)-1):
			if params[i].value >= params[i+1].value:
				return getNil()
		return lisp_forms.Number("T")
		
class Greater(Less):
	def funcall(self, params):
		super(Greater, self).funcall(params)
		
class Print(Function):
	def funcall(self, params):
		if len(params) != 1:
			raise BadInputException("invalid number of arguments: " + str(len(params)))
						
		print str(params[0].getValue())
		return getNil()