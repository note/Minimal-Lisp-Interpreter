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
		
class LispForm:
	def __init__(self, type, value):
		self.children = []
		self.type = type
		self.value = value
		
	def evaluate(self, env, funDict, operatorsDict):
		if self.type == tokenizer.OPENING_PARENTHESIS:
			if self.children[0].type == tokenizer.SYMBOL:
				if self.children[0].value in operatorsDict:
					return operatorsDict[self.children[0].value](self.children[1:], env, funDict, operatorsDict)
				elif self.children[0].value in funDict:
					params = []
					for i in xrange(1, len(self.children)):
						params.append(self.children[i].evaluate(env, funDict, operatorsDict))
					return funDict[self.children[0].value](params)
				else:
					raise BadInputException("The function " + self.children[0].value + " is undefined")
			else:
				raise BadInputException("The first element of list should be a symbol\n")
		elif self.type == tokenizer.INT:
			return LispForm(self.type, int(self.value))
		elif self.type == tokenizer.SYMBOL:
			if self.value in env:
				return env[self.value]
			raise BadInputException("The variable " + self.value + " is unbound")
		else:
			return LispForm(self.type, self.value)
			
	def getValue(self):
		res = self.value
		for ch in self.children:
			res += " " + str(ch.getValue())
		if self.type == tokenizer.OPENING_PARENTHESIS:
			res += " )"
		return res
		
class BadInputException(Exception):
	def __init__(self, msg):
		self.msg = msg

class Interpreter:
	
	funDict = {
		"+": functions.plus,
		"-": functions.minus,
		"*": functions.mul
	}

	operatorsDict = {
		"if": special_operators.ifOperator,
		"let": special_operators.let,
		"progn": special_operators.progn,
		"setq" : special_operators.setq,
		"quote" : special_operators.quote,
		"list" : special_operators.list,
		"car" : special_operators.car,
		"cdr" : special_operators.cdr,
		"eval" : special_operators.eval,
		"atom" : special_operators.atom
	}
	
	def read(self, text):
		form = LispForm(tokenizer.OPENING_PARENTHESIS, "")
		text = self.readForm(form, text, False)[1]
		if tokenizer.nextToken(text)[0].tokenId != tokenizer.EOF:
			raise BadInputException("Syntax error")
		return form.children[0]

	def readForm(self, parent, text, openedParenthesis):
		print "+++"
		print text
		(token, text) = tokenizer.nextToken(text)
		while token.tokenId != tokenizer.EOF:
			if token.tokenId == tokenizer.SYNTAX_ERROR:
				raise BadInputException("Syntax error")
			
			newForm = LispForm(token.tokenId, token.value)
			if token.tokenId == tokenizer.OPENING_PARENTHESIS:
				print "###"
				print text
				text = self.readForm(newForm, text, True)[1]
				parent.children.append(newForm)
			elif token.tokenId == tokenizer.CLOSING_PARENTHESIS:
				if not(openedParenthesis):
					raise BadInputException("Unexpected ')'")
				return (newForm, text)
			else:
				parent.children.append(newForm)
			
			print text
			(token, text) = tokenizer.nextToken(text)	
		
		if openedParenthesis:
			raise BadInputException("Unexpected end of file")
		return (newForm, text)

	def evalExpr(self, text, env):
		form = self.read(text)
		return form.evaluate(env, self.funDict, self.operatorsDict)
			
	def evalExpression(self, text):
		print "???" + text
		return self.evalExpr(text, {"NIL" : LispForm(SYMBOL, "NIL"), "T" : LispForm(SYMBOL, "T")})
