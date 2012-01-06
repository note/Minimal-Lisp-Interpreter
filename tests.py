# -*- coding: utf-8 -*-
import unittest
import tokenizer
from tokenizer import nextToken
from interpreter import *

class TestInterpreter(unittest.TestCase):
	def testNextToken(self):
		self.assertEqual(tokenizer.INT, nextToken("44")[0].tokenId)
		self.assertEqual(tokenizer.INT, nextToken("	44")[0].tokenId)
		self.assertEqual(tokenizer.INT, nextToken("  44 here")[0].tokenId)
		self.assertEqual(tokenizer.INT, nextToken("+435")[0].tokenId)
		self.assertEqual(tokenizer.INT, nextToken("-34")[0].tokenId)
		self.assertEqual(tokenizer.SYMBOL, nextToken("33e")[0].tokenId)
		self.assertEqual(tokenizer.DOUBLE, nextToken("34d0")[0].tokenId)
		self.assertEqual(tokenizer.DOUBLE, nextToken("34d0")[0].tokenId)
		self.assertEqual(tokenizer.DOUBLE, nextToken("34.43d0")[0].tokenId)
		self.assertEqual(tokenizer.DOUBLE, nextToken("0.d55")[0].tokenId)
		self.assertEqual(tokenizer.FLOAT, nextToken(" 3.3")[0].tokenId)
		self.assertEqual(tokenizer.SYMBOL, nextToken("abc ( )")[0].tokenId)
		self.assertEqual(tokenizer.SYMBOL, nextToken("read-in")[0].tokenId)
		self.assertEqual(tokenizer.OPENING_PARENTHESIS, nextToken("  (cos")[0].tokenId)
		
		self.assertEqual(" cos", nextToken("435 cos")[1])
		self.assertEqual(" cos", nextToken(") cos")[1])
		self.assertEqual(" cos", nextToken("( cos")[1])

		self.assertEqual("12", nextToken("12")[0].value)
		
	def testevalExpresssion(self):
		self.assertEqual(5, evalExpresssion("(+ 2 3)")[0].value)
		self.assertEqual(5, evalExpresssion("(+ 2 3) (+ 2932 434)")[0].value)
		self.assertEqual(" (+ 2932 434)", evalExpresssion("(+ 2 3) (+ 2932 434)")[1])
		self.assertEqual(36, evalExpresssion("(* 6 (+ 2 3 1))")[0].value)
		self.assertRaises(BadInputException, evalExpresssion, (")"))
		
	def testIfOperator(self):
		self.assertEqual(11, evalExpresssion("(if 43 11 10)")[0].value)
		self.assertEqual(11, evalExpresssion("(if (- 4 3) 11 10)")[0].value)
		self.assertEqual(10, evalExpresssion("(if NIL 11 10)")[0].value)
		
		self.assertRaises(BadInputException, evalExpresssion, ("(if T)"))

	def testLet(self):
		self.assertEqual(5, evalExpresssion("(let ((x 5)) x)")[0].value)
		self.assertEqual(20, evalExpresssion("(let ((x (* 2 (+ 5 5)))) x)")[0].value)
		self.assertEqual(6, evalExpresssion("(let ((x 2) (y 4)) (+ x y))")[0].value)
		self.assertEqual(14, evalExpresssion("(let ((x 10)) (let ((y 4)) (+ x y)))")[0].value)
		
	def testProgn(self):
		self.assertEqual(5, evalExpresssion("(progn 3 5)")[0].value)
		self.assertEqual(4, evalExpresssion("(progn (+ 3 4) (- 5 1))")[0].value)
	
	#def testSetq(self):
	#	self.assertEqual(99, evalExpresssion("(let ((x 2)) (progn (setq x 99) x))")[0].value)

if __name__ == "__main__":
	unittest.main()