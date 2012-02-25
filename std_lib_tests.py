# -*- coding: utf-8 -*-
import unittest
import subprocess
import tokenizer
from tokenizer import nextToken
from interpreter_consts import *
from interpreter import *

class TestInterpreter(unittest.TestCase):
	def setUp(self):
		self.interpreter = Interpreter()
		
	def doTest(self, expectedValue, inputStr):
		self.assertEqual(expectedValue, self.interpreter.evalExpression(inputStr).getValue())
		
	def testNth(self):
		self.doTest(3, "(nth 3 '(0 1 2 3 4 5))")
		self.doTest("NIL", "(nth 8 '(0 1 2 3 4 5))")
		
	def testApply(self):
		self.doTest(5, "(apply #'+ '(2 3))")
	
	def testDo(self):
		self.doTest(12, "(do ((i 0 (+ 3 i))) ((> i 10) i) (print i))")
		self.doTest(28, "(let ((x 10)) (progn (do ((i 0 (+ 3 i))) ((> i 10) x) (setq x (+ x i))) x))")
		self.doTest(28, "(let ((x 10)) (do ((i 0 (+ 3 i))) ((> i 10) x) (setq x (+ x i))))")
		self.doTest(32, "(let ((x 10)) (do ((i 0 (+ 3 i))) ((> i 10) x) (setq x (+ x i)) (incf x)))")
		
	def testDotimes(self):
		self.doTest(6, "(let ((x 0)) (dotimes (i 4 x) (setq x (+ x i))))")
		self.doTest(18, "(let ((x 0) (y 3)) (dotimes (i 4 (* x y)) (setq x (+ x i))))")
		self.doTest("NIL", "(dotimes (i 4) (print i))")
		self.doTest(4, "(dotimes (i 4 i) (print i))")
		
	def testDolist(self):
		self.doTest(21, "(let ((res 0)) (dolist (i '(5 6 10) res) (setq res (+ res i))))")
		self.doTest("NIL", "(let ((res 0)) (dolist (i '(5 6 10)) (setq res (+ res i))))")
		
if __name__ == "__main__":
	unittest.main()