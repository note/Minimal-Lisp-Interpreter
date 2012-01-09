# -*- coding: utf-8 -*-
import unittest
import tokenizer
from tokenizer import nextToken
from interpreter import *

class TestInterpreter(unittest.TestCase):
	def setUp(self):
		self.interpreter = Interpreter()
	
	def testNextToken(self):
		self.assertEqual(tokenizer.EOF, nextToken("")[0].tokenId)
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
		self.assertEqual(5, self.interpreter.evalExpression("(+ 2 3)").value)
		self.assertEqual(5, self.interpreter.evalExpression("(+ 2 3) (+ 2932 434)").value)
		self.assertEqual(36, self.interpreter.evalExpression("(* 6 (+ 2 3 1))").value)
		self.assertRaises(BadInputException, self.interpreter.evalExpression, (")"))
		
	def testIfOperator(self):
		self.assertEqual(11, self.interpreter.evalExpression("(if 43 11 10)").value)
		self.assertEqual(11, self.interpreter.evalExpression("(if (- 4 3) 11 10)").value)
		self.assertEqual(10, self.interpreter.evalExpression("(if NIL 11 10)").value)
		
		self.assertRaises(BadInputException, self.interpreter.evalExpression, ("(if T)"))

	'''def testLet(self):
		self.assertEqual(5, self.interpreter.evalExpression("(let ((x 5)) x)")[0].value)
		self.assertEqual(20, self.interpreter.evalExpression("(let ((x (* 2 (+ 5 5)))) x)")[0].value)
		self.assertEqual(6, self.interpreter.evalExpression("(let ((x 2) (y 4)) (+ x y))")[0].value)
		self.assertEqual(14, self.interpreter.evalExpression("(let ((x 10)) (let ((y 4)) (+ x y)))")[0].value)
		
	def testProgn(self):
		self.assertEqual(5, self.interpreter.evalExpression("(progn 3 5)")[0].value)
		self.assertEqual(4, self.interpreter.evalExpression("(progn (+ 3 4) (- 5 1))")[0].value)
	
	def testSetq(self):
		self.assertEqual(12, self.interpreter.evalExpression("(let ((a 10)) (setq a 12))")[0].value)
		self.assertEqual(99, self.interpreter.evalExpression("(let ((x 2)) (progn (setq x 99) x))")[0].value)
		self.assertEqual(2, self.interpreter.evalExpression("(let ((x 2)) (progn (let ((x 3)) (setq x 4)) x))")[0].value)
		self.assertEqual(12, self.interpreter.evalExpression("(let ((x 2) (y 6)) (progn (setq x 4 y 8) (+ x y)))")[0].value)
		
	def testQuote(self):
		self.assertEqual("( + x 5 )", self.interpreter.evalExpression("(quote (+ x 5))")[0].value)
		self.assertEqual("x", self.interpreter.evalExpression("(let ((x 12)) (quote x))")[0].value)
		
	def testList(self):
		pass	
		
	def testCar(self):
		self.assertEqual("+", self.interpreter.evalExpression("(car (quote (+ 2 4)))")[0].value)
		self.assertEqual("5", self.interpreter.evalExpression("(car (list 5 6))")[0].value)'''
		
	''' To test if closures work:
	(let ((x 0)) (defparameter
	     *fn1* (list 
		    #'(lambda () (format t "~a" x))
		    #'(lambda () (setf x (1+ x))))))
		    
	and now call a few times:
	(funcall (nth 1 *fn1*))
	(funcall (nth 0 *fn1*))'''

if __name__ == "__main__":
	unittest.main()