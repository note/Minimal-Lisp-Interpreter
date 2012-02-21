# -*- coding: utf-8 -*-
import re
from interpreter_consts import *

class Token:
	def __init__(self, tokenId, value):
		self.tokenId = tokenId
		self.value = value

def search(s, regex):
	p = re.compile(regex)
	return p.search(s)

def createTokenPattern(id, regex):
	return (id, re.compile(regex))

def patterns():
	unsignedInteger = "[1-9]*[0-9]";
	integer = "[\+-]?" + unsignedInteger;
	end = "(\\)|[\\s]|\Z)";
	exp = "[de][0-9]+";
	
	# it's important to remember abour proper grouping in all regular expressions
	p = []
	p.append(createTokenPattern(OPENING_PARENTHESIS, "^(\()"))
	p.append(createTokenPattern(CLOSING_PARENTHESIS, "^(\))"))
	p.append(createTokenPattern(INT, "^(" + integer + ")" + end))
	p.append(createTokenPattern(FLOAT, "^(" + integer + ")e([0-9]+)" + end))
	p.append(createTokenPattern(FLOAT, "^[\+-]?[0-9]*\.[0-9]+(e[0-9]+)?" + end))
	p.append(createTokenPattern(FLOAT, "^[\+-]?[0-9]+\.[0-9]*(e[0-9]+)?" + end))
	p.append(createTokenPattern(DOUBLE, "^(" + integer + ")d[0-9]+" + end))
	p.append(createTokenPattern(DOUBLE, "^([\+-]?[0-9]*\.[0-9]+d[0-9]+)" + end))
	p.append(createTokenPattern(DOUBLE, "^([\+-]?[0-9]+\.[0-9]*d[0-9]+)" + end))
	p.append(createTokenPattern(RATIO, "^(" + integer + ")\/" + unsignedInteger + end))
	p.append(createTokenPattern(STRING, '^"(.*)"' + end))
	p.append(createTokenPattern(SYMBOL, "^([^\s\(\)'\"`,:;\\\|]+)" + end))
	p.append(createTokenPattern(QUOTE, "^(')"))
	p.append(createTokenPattern(BACKQUOTE, "^(`)"))
	p.append(createTokenPattern(COMMA_AT, "^(,@)"))
	p.append(createTokenPattern(COMMA, "^(,)"))
	p.append(createTokenPattern(HASH, "^(#)"))
	return p

#todo: returning sensible semantic value for float, double and ratio
def nextToken(text):
	text = text.lstrip()
	
	if(len(text) == 0):
		return (Token(EOF, None), "");

	for (id, p) in patterns():
		res = p.search(text)
		if res:
			return (Token(id, res.group(1)), text[res.end(1):])
			
	return (Token(SYNTAX_ERROR, None), text)
