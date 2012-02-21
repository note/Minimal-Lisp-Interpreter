# -*- coding: utf-8 -*-

class BadInputException(Exception):
	def __init__(self, msg):
		self.msg = msg
		
class InterpreterException(Exception):
	def __init__(self, msg):
		self.msg = msg