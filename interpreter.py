# -*- coding: utf-8 -*-

import tokenizer
import functions
import special_operators
from tokenizer import nextToken

SYMBOL = 0
INT = 1

class ExprRes:
	def __init__(self, type, val):
		self.type = type
		self.value = val
		
class BadInputException(Exception):
	def __init__(self, msg):
		self.msg = msg

funDict = {
	"+": functions.plus,
	"-": functions.minus,
	"*": functions.mul
}

operatorsDict = {
	"if": special_operators.ifOperator,
	"let": special_operators.let,
	"progn": special_operators.progn,
	"setq" : special_operators.setq
}

def evalExpr(text, env):
	
	(token, text) = nextToken(text)
	
	if token.tokenId == tokenizer.OPENING_PARENTHESIS:
		return evalList(text, env)
	elif token.tokenId == tokenizer.CLOSING_PARENTHESIS or token.tokenId == tokenizer.SYNTAX_ERROR:
		raise BadInputException("Syntax error")
	elif token.tokenId == tokenizer.EOF:
		return (ExprRes(SYMBOL, "NIL"), text)
	elif token.tokenId == tokenizer.SYMBOL:
		if token.value in env:
			return (env[token.value], text)
		raise BadInputException("The variable " + token.value + " is unbound")
		
	else:
		return (ExprRes(INT, int(token.value)), text)
		
def evalExpresssion(text):
	return evalExpr(text, {"NIL" : ExprRes(SYMBOL, "NIL"), "T" : ExprRes(SYMBOL, "T")})

def evalList(text, env):
	(token, text) = nextToken(text)
	if(token.tokenId != tokenizer.SYMBOL):
		raise BadInputException("the first element of list should be a symbol\n")
	
	symbol = token.value
	params = []
	
	if(symbol in funDict):
		while len(text) > 0:
			(token, text) = nextToken(text)
			if token.tokenId == tokenizer.OPENING_PARENTHESIS:
				(res, text) = evalList(text, env)
				params.append(res)
			elif token.tokenId == tokenizer.CLOSING_PARENTHESIS:
				return (funDict[symbol](params), text)
			elif token.tokenId == tokenizer.INT:
				params.append(ExprRes(INT, int(token.value)))
			elif token.tokenId == tokenizer.SYMBOL:
				if token.value in env:
					params.append(env[token.value])
				else:
					raise BadInputException("The variable " + token.value + " is unbound")
			elif token.tokenId == tokenizer.SYNTAX_ERROR:
				raise BadInputException("Syntax error")
	
	elif symbol in operatorsDict:
		while len(text) > 0:
			(token, text) = nextToken(text)
			if token.tokenId == tokenizer.OPENING_PARENTHESIS:
				(res, text) = cutList(text)
				params.append("(" + res)
			elif token.tokenId == tokenizer.CLOSING_PARENTHESIS:
				return (operatorsDict[symbol](params, env), text)
			elif token.tokenId == tokenizer.INT:
				params.append(token.value)
			elif token.tokenId == tokenizer.SYMBOL:
				params.append(token.value)
			elif token.tokenId == SYNTAX_ERROR:
				raise BadInputException("Syntax error")
				
	else:
		raise BadInputException("Function " + symbol + " is undefined")

def cutList(text):
	opened = 1
	result = ""

	while len(text) > 0:
		if opened == 0:
			return (result, text)

		(token, text) = nextToken(text)
		if token.tokenId == tokenizer.OPENING_PARENTHESIS:
			opened += 1
			result += " ("
		elif token.tokenId == tokenizer.CLOSING_PARENTHESIS:
			opened -= 1
			result += " )"
		elif token.tokenId == tokenizer.SYNTAX_ERROR:
			raise BadInputException("Syntax error")
		else:
			result += " " + token.value
			
def loadInitializationList(text, env):
	newVars = {}
	(token, text) = nextToken(text)
	
	if token.tokenId != tokenizer.OPENING_PARENTHESIS:
		raise BadInputException("Syntax error")
		
	while len(text) > 0:
		(token, text) = nextToken(text)
		
		if token.tokenId == tokenizer.SYMBOL:
			newVars[token.value] = ExprRes(SYMBOL, "NIL")
		elif token.tokenId == tokenizer.OPENING_PARENTHESIS:
			(token, text) = nextToken(text)
			
			if token.tokenId == tokenizer.SYMBOL:
				(res, text) = evalExpr(text, env)
				newVars[token.value] = res
			else:
				raise BadInputException("Syntax error")
				
			(token, text) = nextToken(text)
			if token.tokenId != tokenizer.CLOSING_PARENTHESIS:
				raise BadInputException("Syntax error")
		elif token.tokenId == tokenizer.CLOSING_PARENTHESIS:
			return (newVars, text)
		else:
			raise BadInputException("Syntax error")
		
def evalSetqList(text, env):
	while len(text) > 0:
		(token, text) = nextToken(text)
		
		if token.tokenId == tokenizer.SYMBOL:
			(res, text) = evalExpr(text, env)
			env[token.value] = res
		elif token.tokenId == tokenizer.CLOSING_PARENTHESIS:
			return
		else:
			raise BadInputException("Syntax error")
