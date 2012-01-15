# -*- coding: utf-8 -*-

import tokenizer
import functions
import special_operators
from tokenizer import nextToken

SYNTAX_ERROR = 0
INT = 1
FLOAT = 2
DOUBLE = 3
STRING = 4;
OPENING_PARENTHESIS = 5
LIST = 5 # OPENING_PARENTHESIS and LIST have purposely the same value!
CLOSING_PARENTHESIS = 6
SYMBOL = 7
RATIO = 8
EOF = 9
		
class BadInputException(Exception):
	def __init__(self, msg):
		self.msg = msg

class Environment:	
	def __init__(self, variables, funDict):
		self.variables = variables
		self.funDict = funDict

class Interpreter:
	
	funDict = {
		"+": functions.plus,
		"-": functions.minus,
		"*": functions.mul
	}
	
	operatorsDict = {
		"if": special_operators.IfOperator(),
		"let": special_operators.Let(),
		"progn": special_operators.Progn(),
		"setq" : special_operators.Setq(),
		"quote" : special_operators.Quote(),
		"'" : special_operators.Quote(),
		"list" : special_operators.List(),
		"car" : special_operators.Car(),
		"cdr" : special_operators.Cdr(),
		"eval" : special_operators.Eval(),
		"atom" : special_operators.Atom(),
		"`" : special_operators.Backquote(),
		"," : special_operators.Comma()
	}
	
	def tokenizerToInterpreterCons(self, tokenizerCons):
		if tokenizerCons==tokenizer.INT:
			return INT
		
	
	def read(self, text):
		form = LispForm(LIST, "")
		text = self.readForm(form, text, False)[1]
		if tokenizer.nextToken(text)[0].tokenId != tokenizer.EOF:
			raise BadInputException("Syntax error")
		return form.children[0]

	# maximum x means that after loading x element function returns
	# useful eg. when implementing "'" bevaviour, "'a b" should translate to "(quote a) b". So after loading quote we want load just one element nevertheless there is no ")" in original string
	# maximum -1 means we want return from readForm onlu if there is EOF or ")" appears in original string
	def readForm(self, parent, text, openedParenthesis, maximum=-1):
		(token, text) = tokenizer.nextToken(text)
		while token.tokenId != tokenizer.EOF:
			if token.tokenId == tokenizer.SYNTAX_ERROR:
				raise BadInputException("Syntax error")
			
			if token.tokenId == tokenizer.QUOTE or token.tokenId == tokenizer.BACKQUOTE or token.tokenId == tokenizer.COMMA:
				newForm = LispForm(LIST, "(")
				newForm.children.append(LispForm(SYMBOL, token.value))
				text = self.readForm(newForm, text, False, 1)[1]
				parent.children.append(newForm)
			else:
				newForm = LispForm(token.tokenId, token.value)
				
				if token.tokenId == tokenizer.OPENING_PARENTHESIS:
					text = self.readForm(newForm, text, True)[1]
					parent.children.append(newForm)
				elif token.tokenId == tokenizer.CLOSING_PARENTHESIS:
					if not(openedParenthesis):
						raise BadInputException("Unexpected ')'")
					return (newForm, text)
				else:
					parent.children.append(newForm)
			
			maximum -= 1
			if maximum == 0:
				return (newForm, text)
			(token, text) = tokenizer.nextToken(text)	
		
		if openedParenthesis:
			raise BadInputException("Unexpected end of file")
		return (newForm, text)

	def evalExpr(self, text, variables):
		form = self.read(text)
		return form.evaluate(Environment(variables, self.funDict))
			
	def evalExpression(self, text):
		return self.evalExpr(text, {"NIL" : LispForm(SYMBOL, "NIL"), "T" : LispForm(SYMBOL, "T")})


class LispForm:
	operatorsDict = Interpreter.operatorsDict
	def __init__(self, type, value):
		self.children = []
		self.type = type
		self.value = value
		
	def evaluate(self, env, **rest):
		if self.type == tokenizer.OPENING_PARENTHESIS:
			if self.children[0].type == tokenizer.SYMBOL:
				if self.children[0].value in self.operatorsDict:
					return self.operatorsDict[self.children[0].value].evaluate(self.children[1:], env, **rest)
				elif self.children[0].value in env.funDict:
					params = []
					for i in xrange(1, len(self.children)):
						params.append(self.children[i].evaluate(env, **rest))
					return env.funDict[self.children[0].value](params)
				else:
					print self.children[0].value
					raise BadInputException("The function " + self.children[0].value + " is undefined")
			else:
				raise BadInputException("The first element of list should be a symbol\n")
		elif self.type == tokenizer.INT:
			return LispForm(self.type, int(self.value))
		elif self.type == tokenizer.SYMBOL:
			if self.value in env.variables:
				return env.variables[self.value]
			raise BadInputException("The variable " + self.value + " is unbound")
		else:
			return LispForm(self.type, self.value)
		
	def evaluateIfComma(self, env, **rest):
		if self.type == tokenizer.SYMBOL and self.value == ",":
			return self.operatorsDict[self.value].evaluateIfComma(self.children[1:], env, rest)
		elif self.type == tokenizer.OPENING_PARENTHESIS:
			if self.children[0].type == tokenizer.SYMBOL and self.children[0].value == ",":
				return self.operatorsDict[self.children[0].value].evaluate(self.children[1:], env, **rest)
			else:
				res = LispForm(self.type, self.value)
				for ch in self.children:
					res.children.append(ch.evaluateIfComma(env, **rest))
				return res
		elif self.type == tokenizer.INT or self.type == tokenizer.SYMBOL:
			return LispForm(self.type, self.value)
				
	
	def getValue(self):
		res = self.value
		for ch in self.children:
			if self.type == tokenizer.OPENING_PARENTHESIS and ch == self.children[0]:
				res += str(ch.getValue())
			else:
				res += " " + str(ch.getValue())
		if self.type == tokenizer.OPENING_PARENTHESIS:
			res += ")"
		return res
