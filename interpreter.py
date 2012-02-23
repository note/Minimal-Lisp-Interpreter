# -*- coding: utf-8 -*-

import sys
import tokenizer
import functions
import special_operators
from lisp_forms import *
from interpreter_exceptions import BadInputException, InterpreterException
from interpreter_consts import *
from environment import *
from helpers import *

class Interpreter:
	
	funDict = {
		"+": functions.Plus(),
		"-": functions.Minus(),
		"*": functions.Mul(),
		"=": functions.Equal()
	}	
	
	def read(self, text):
		form = List()
		text = self.readForm(form, text, False)[1]
		if tokenizer.nextToken(text)[0].tokenId != EOF:
			raise BadInputException("Syntax error")
		return form.children

	# maximum x means that after loading x element function returns
	# useful eg. when implementing "'" bevaviour, "'a b" should translate to "(quote a) b". So after loading quote we want load just one element nevertheless there is no ")" in original string
	# maximum -1 means we want return from readForm onlu if there is EOF or ")" appears in original string
	def readForm(self, parent, text, openedParenthesis, maximum=-1):
		(token, text) = tokenizer.nextToken(text)
		while token.tokenId != EOF:
			if token.tokenId == SYNTAX_ERROR:
				raise BadInputException("Syntax error")
			
			if token.tokenId == QUOTE or token.tokenId == BACKQUOTE or token.tokenId == COMMA or token.tokenId == HASH or token.tokenId == COMMA_AT:
				newForm = List()
				newForm.children.append(Symbol(token.value))
				text = self.readForm(newForm, text, False, 1)[1]
				parent.children.append(newForm)
			else:
				newForm = LispForm.createLispForm(token.tokenId, token.value)
				
				if token.tokenId == OPENING_PARENTHESIS:
					text = self.readForm(newForm, text, True)[1]
					parent.children.append(newForm)
				elif token.tokenId == CLOSING_PARENTHESIS:
					if not(openedParenthesis):
						raise BadInputException("Unexpected ')'")
					return (newForm, text)
				elif token.tokenId == INT:
					newForm.value = int(newForm.value)
					parent.children.append(newForm)
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
		topLevelForm = special_operators.Progn()
		return topLevelForm.evaluate(form, Environment(Env(variables, self.funDict), Env({}, {})))
			
	def evalExpression(self, text):
		return self.evalExpr(text, {"NIL" : getNil(), "T" : Symbol("T")})

def main(argv=sys.argv):
	interpreter = Interpreter()
	if len(argv)>1:
		try:
			f = open(argv[1], 'r')
			str = f.read()
			print interpreter.evalExpression(str).getValue()
		except IOError:
			print "File " + argv[1] + " cannot be opened"
	
if __name__ == "__main__":
	main()