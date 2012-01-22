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
		
	def testEvalExpresssion(self):
		self.assertEqual(5, self.interpreter.evalExpression("(+ 2 3)").getValue())
		self.assertEqual(5, self.interpreter.evalExpression("(+ 2 3) (+ 2932 434)").getValue())
		self.assertEqual(36, self.interpreter.evalExpression("(* 6 (+ 2 3 1))").getValue())
		self.assertRaises(BadInputException, self.interpreter.evalExpression, (")"))
		
	def testIfOperator(self):
		self.assertEqual(11, self.interpreter.evalExpression("(if 43 11 10)").getValue())
		self.assertEqual(11, self.interpreter.evalExpression("(if (- 4 3) 11 10)").getValue())
		self.assertEqual(10, self.interpreter.evalExpression("(if NIL 11 10)").getValue())
		
		self.assertRaises(BadInputException, self.interpreter.evalExpression, ("(if T)"))

	def testLet(self):
		self.assertEqual(5, self.interpreter.evalExpression("(let ((x 5)) x)").getValue())
		self.assertEqual(20, self.interpreter.evalExpression("(let ((x (* 2 (+ 5 5)))) x)").getValue())
		self.assertEqual(6, self.interpreter.evalExpression("(let ((x 2) (y 4)) (+ x y))").getValue())
		self.assertEqual(14, self.interpreter.evalExpression("(let ((x 10)) (let ((y 4)) (+ x y)))").getValue())
		
	def testProgn(self):
		self.assertEqual(5, self.interpreter.evalExpression("(progn 3 5)").getValue())
		self.assertEqual(4, self.interpreter.evalExpression("(progn (+ 3 4) (- 5 1))").getValue())
	
	def testSetq(self):
		self.assertEqual(12, self.interpreter.evalExpression("(let ((a 10)) (setq a 12))").getValue())
		self.assertEqual(99, self.interpreter.evalExpression("(let ((x 2)) (progn (setq x 99) x))").getValue())
		self.assertEqual(2, self.interpreter.evalExpression("(let ((x 2)) (progn (let ((x 3)) (setq x 4)) x))").getValue())
		self.assertEqual(12, self.interpreter.evalExpression("(let ((x 2) (y 6)) (progn (setq x 4 y 8) (+ x y)))").getValue())
		
	def testQuote(self):
		self.assertEqual("(+ x 5)", self.interpreter.evalExpression("(quote (+ x 5))").getValue())
		self.assertEqual("x", self.interpreter.evalExpression("(let ((x 12)) (quote x))").getValue())
		self.assertEqual("(x)", self.interpreter.evalExpression("(quote (x))").getValue())
		self.assertEqual("(quote (x))", self.interpreter.evalExpression("(quote (quote (x)))").getValue())
		
	def testLiteralQuote(self):
		self.assertEqual("(+ x 5)", self.interpreter.evalExpression("'(+ x 5)").getValue())
		self.assertEqual("x", self.interpreter.evalExpression("(let ((x 12)) 'x)").getValue())
		self.assertEqual("(x)", self.interpreter.evalExpression("'(x)").getValue())
		#self.assertEqual("'(x)", self.interpreter.evalExpression("''(x)").getValue())
		
	def testBackQuote(self):
		self.assertEqual("(+ x 5)", self.interpreter.evalExpression("`(+ x 5)").getValue())
		self.assertEqual("x", self.interpreter.evalExpression("(let ((x 12)) `x)").getValue())
		self.assertEqual("x", self.interpreter.evalExpression("`x").getValue())
		self.assertEqual("(+ 4 10)", self.interpreter.evalExpression("`(+ 4 ,(* 2 5))").getValue())
		self.assertEqual("(+ (* 3 3) 10)", self.interpreter.evalExpression("`(+ (* 3 3) ,(* 2 5))").getValue())
		self.assertEqual("(* (+ 4 10) 11)", self.interpreter.evalExpression("`(* (+ 4 ,(+ 3 7)) 11)").getValue())
		
	def testList(self):
		self.assertRaises(BadInputException, self.interpreter.evalExpression, ("(list (+ x 5))"))
		self.assertEqual("(3 4 5)", self.interpreter.evalExpression("(list 3 4 5)").getValue())
		self.assertEqual("(1 2 3)", self.interpreter.evalExpression("(let ((x 2)) (list 1 x 3))").getValue())
		self.assertEqual("(10 20 (30 40))", self.interpreter.evalExpression("(list 10 20 (list 30 40))").getValue())
		#self.assertEqual("NIL", self.interpreter.evalExpression("(list)").getValue())
		self.assertRaises(BadInputException, self.interpreter.evalExpression, ("(list))"))
		
	def testCar(self):
		self.assertEqual("+", self.interpreter.evalExpression("(car (quote (+ 2 4)))").getValue())
		self.assertEqual(5, self.interpreter.evalExpression("(car (list 5 6))").getValue())
		self.assertEqual(5, self.interpreter.evalExpression("(car (list (+ 1 4) 6))").getValue())
		self.assertEqual("(5 6)", self.interpreter.evalExpression("(car (list (list 5 6) 9 10))").getValue())
		
	def testCdr(self):
		self.assertEqual("(2 4)", self.interpreter.evalExpression("(cdr (quote (+ 2 4)))").getValue())
		self.assertEqual("NIL", self.interpreter.evalExpression("(cdr (list 2))").getValue())
		self.assertEqual("NIL", self.interpreter.evalExpression("(cdr (list))").getValue())
		self.assertEqual("((* 3 4))", self.interpreter.evalExpression("(cdr (quote (2 (* 3 4))))").getValue())
		self.assertEqual("(6)", self.interpreter.evalExpression("(cdr (list 5 6))").getValue())
		self.assertEqual(6, self.interpreter.evalExpression("(car (cdr (list 5 6 9 10)))").getValue())
		
	def testEval(self):
		self.assertEqual(5, self.interpreter.evalExpression("(eval (quote (+ 3 2)))").getValue())
		self.assertEqual(5, self.interpreter.evalExpression("(eval (+ 3 2))").getValue())
		
	def testAtom(self):
		self.assertEqual("T", self.interpreter.evalExpression("(atom (+ 4 3))").getValue())
		self.assertEqual("T", self.interpreter.evalExpression("(atom (quote 43))").getValue())
		self.assertEqual("T", self.interpreter.evalExpression("(atom (quote ()))").getValue())
		self.assertEqual("T", self.interpreter.evalExpression("(atom (quote +))").getValue())
		self.assertEqual("NIL", self.interpreter.evalExpression("(atom (quote (+)))").getValue())
		self.assertEqual("NIL", self.interpreter.evalExpression("(atom (quote (+ 4 5)))").getValue())
		self.assertEqual("NIL", self.interpreter.evalExpression("(atom (quote (4)))").getValue())
		self.assertEqual("NIL", self.interpreter.evalExpression("(atom (list 5 4))").getValue())
		
	def testEq(self):
		self.assertEqual("T", self.interpreter.evalExpression("(let ((x (list 3 4)) (y (list 3 4))) (eq x y))").getValue())
		self.assertEqual("NIL", self.interpreter.evalExpression("(let ((x (list 3 4)) (y (list 3))) (eq x y))").getValue())
		self.assertEqual("NIL", self.interpreter.evalExpression("(eq (list 3 4) (list 3 4))").getValue())
		self.assertValue("T", self.interpreter.evalExpression("(eq (+ 3 4) (+ 3 4))").getValue())
		self.assertValue("NIL", self.interpreter.evalExpression("(eq '(+ 3 4) '(+ 3 4))").getValue())
		self.assertValue("T", self.interpreter.evalExpression("(eq 'fddf 'fddf)").getValue())
		self.assertValue("NIL", self.interpreter.evalExpression("(eq '(fddf) '(fddf))").getValue())
		
	def testLambda(self):
		self.assertEqual(9, self.interpreter.evalExpression("((lambda (x) (* x x)) 3)").getValue())
		self.assertEqual(19, self.interpreter.evalExpression("((lambda (x y) (+ (* 2 x) y)) 4 (+ 4 7))").getValue())
		self.assertEqual(96, self.interpreter.evalExpression("(let ((x 32)) ((lambda (y) (* x y)) 3))").getValue())
		self.assertEqual(144, self.interpreter.evalExpression("(let ((x ((lambda (x) (* x x)) 12))) x)").getValue())
		self.assertEqual(FUN_OBJ, self.interpreter.evalExpression("(lambda (x) x)").getType())
		self.assertEqual("#<FUNCTION LAMBDA>", self.interpreter.evalExpression("(lambda (x) x)").getValue())
		self.assertEqual("#<FUNCTION LAMBDA>", self.interpreter.evalExpression("(let ((x (lambda (x y) (* x y)))) x)").getValue())
		
	def testDefun(self):
		self.assertEqual("f", self.interpreter.evalExpression("(defun f (x y z) (* x y (+ 2 z)))").getValue())
		self.assertEqual(36, self.interpreter.evalExpression("(f 2 3 4)").getValue())
		self.assertEqual(FUN_OBJ, self.interpreter.evalExpression("#'f").getType())
		self.assertEqual("#<FUNCTION f>", self.interpreter.evalExpression("#'f").getValue())
		self.assertEqual("g", self.interpreter.evalExpression("(let ((a 15)) (defun g (x) (* x a)))").getValue())
		self.assertEqual(30, self.interpreter.evalExpression("(g 2)").getValue())
		self.assertEqual(10, self.interpreter.evalExpression("(let ((a 15)) (progn (defun h (x) (* x a)) (setq a 10)))").getValue())
		self.assertEqual(20, self.interpreter.evalExpression("(h 2)").getValue())
		self.assertEqual(20, self.interpreter.evalExpression("(let ((a 30)) (h 2))").getValue())
		self.interpreter.evalExpression("(defun fib (x) (if (= x 0) 1 (* x (fib (- x 1)))))")
		self.assertEqual(120, self.interpreter.evalExpression("(fib 5)").getValue())
		#self.interpreter.evalExpression("(defun mymap (l f) (if (car l) (cons (funcall f (car l)) (mymap (cdr l) f))))")
		#self.assertEqual("(2 4 6)", self.interpreter.evalExpression("(mymap '(1 2 3) (lambda (x) (* 2 x)))").getValue())
		
	def testFuncall(self):
		self.assertEqual(16, self.interpreter.evalExpression("(let ((fn (lambda (x) (* x x)))) (funcall fn 4))").getValue())
		self.assertEqual("f", self.interpreter.evalExpression("(defun f (x) (* x x))").getValue())
		self.assertEqual(9, self.interpreter.evalExpression("(funcall #'f 3)").getValue())
		
	'''Difference between global scope and any other:
		(let ((a 15)) (progn (defun h(x) (* x a b)) (setf a 10)))
		(let ((b 13)) (h 2)) - does not work
		(defparameter b 13)
		(h 2) - works'''
	
	''' To test if closures work:
	(let ((x 0)) (defparameter
	     *fn1* (list 
		    #'(lambda () (format t "~a" x))
		    #'(lambda () (setf x (1+ x))))))
		    
	and now call a few times:
	(funcall (nth 1 *fn1*))
	(funcall (nth 0 *fn1*))
	
	another interesting test:
	(let ((sq 12)) (flet ((sq (x) (* x x))) (sq sq)))'''

if __name__ == "__main__":
	unittest.main()