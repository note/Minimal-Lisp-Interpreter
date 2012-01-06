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

funDict = {
	"+": functions.plus,
	"-": functions.minus,
	"*": functions.mul
}

operatorsDict = {
	"if": special_operators.ifOperator,
	"let": special_operators.let
}

def evalExpr(text, env):
	print "evalExpr"
	print text
	
	(token, text) = nextToken(text)
	
	if token.tokenId == tokenizer.OPENING_PARENTHESIS:
		#return eval_list($_[0]) . "\n";
		return evalList(text, env)
	elif token.tokenId == tokenizer.CLOSING_PARENTHESIS or token.tokenId == tokenizer.SYNTAX_ERROR:
		if(token.tokenId == tokenizer.SYNTAX_ERROR):
			print "real syntax error\n"
		print "syntax error\n"
		return (ExprRes(SYMBOL, "NIL"), text)
	elif token.tokenId == tokenizer.EOF:
		return (ExprRes(SYMBOL, "NIL"), text)
	elif token.tokenId == tokenizer.SYMBOL:
		if token.value in env:
			return (env[token.value], text)
		print  "The variable " + token.value + " is unbound"
		return (ExprRes(SYMBOL, "NIL"), text)
	else:
		return (ExprRes(INT, int(token.value)), text)
		
def evalExpresssion(text):
	return evalExpr(text, {"NIL" : ExprRes(SYMBOL, "NIL"), "T" : ExprRes(SYMBOL, "T")})

def evalList(text, env):
	(token, text) = nextToken(text)
	if(token.tokenId != tokenizer.SYMBOL):
		print "the first element of list should be a symbol\n"
		return
	
	symbol = token.value
	params = []

	print "eval_list, symbol:" + symbol + "\n"
	
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
				print  "The variable " + token.value + " is unbound"
			elif token.tokenId == tokenizer.SYNTAX_ERROR:
				print "Syntax error\n"
				return (ExprRes(SYMBOL, "NIL"), text)
	
	elif symbol in operatorsDict:
		print "SPECIAL OPERATOR" + symbol + "\n"
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
				print "Syntax error\n"
				return (ExprRes(SYMBOL, "NIL"), text)

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
			print "Syntax error\n"
			break
		else:
			result += " " + token.value
			
def loadInitializationList(text, env):
	newVars = {}
	(token, text) = nextToken(text)
	
	if token.tokenId != tokenizer.OPENING_PARENTHESIS:
		print "Syntax error\n"
		return
		
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
				print "Syntax error\n"
				
			(token, text) = nextToken(text)
			if token.tokenId != tokenizer.CLOSING_PARENTHESIS:
				print "Syntax error\n"
				return
		elif token.tokenId == tokenizer.CLOSING_PARENTHESIS:
			return (newVars, text)
		else:
			print "Syntax error\n"
			return
