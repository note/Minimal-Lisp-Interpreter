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
	
	def testNextToken(self):
		self.assertEqual(EOF, nextToken("")[0].tokenId)
		self.assertEqual(INT, nextToken("44")[0].tokenId)
		self.assertEqual(INT, nextToken("	44")[0].tokenId)
		self.assertEqual(INT, nextToken("  44 here")[0].tokenId)
		self.assertEqual(INT, nextToken("+435")[0].tokenId)
		self.assertEqual(INT, nextToken("-34")[0].tokenId)
		self.assertEqual(SYMBOL, nextToken("33e")[0].tokenId)
		self.assertEqual(DOUBLE, nextToken("34d0")[0].tokenId)
		self.assertEqual(DOUBLE, nextToken("34d0")[0].tokenId)
		self.assertEqual(DOUBLE, nextToken("34.43d0")[0].tokenId)
		self.assertEqual(DOUBLE, nextToken("0.d55")[0].tokenId)
		self.assertEqual(FLOAT, nextToken(" 3.3")[0].tokenId)
		self.assertEqual(STRING, nextToken('"some"')[0].tokenId)
		self.assertEqual(STRING, nextToken('"hello world"')[0].tokenId)
		self.assertEqual("", nextToken('"hello world"')[1])
		self.assertEqual(SYMBOL, nextToken("abc ( )")[0].tokenId)
		self.assertEqual(SYMBOL, nextToken("read-in")[0].tokenId)
		self.assertEqual(OPENING_PARENTHESIS, nextToken("  (cos")[0].tokenId)
		
		self.assertEqual(" cos", nextToken("435 cos")[1])
		self.assertEqual(" cos", nextToken(") cos")[1])
		self.assertEqual(" cos", nextToken("( cos")[1])

		self.assertEqual("12", nextToken("12")[0].value)
		
	def testEvalExpresssion(self):
		self.assertEqual(5, self.interpreter.evalExpression("(+ 2 3)").getValue())
		self.assertEqual(13, self.interpreter.evalExpression("(+ 2 3) (+ 5 8)").getValue())
		self.assertEqual(36, self.interpreter.evalExpression("(* 6 (+ 2 3 1))  ").getValue())
		self.doTest(12, "(+ 2 3) 	 (* 3 4)")
		self.assertRaises(BadInputException, self.interpreter.evalExpression, ")")
		self.doTest("NIL", "()")
		
	def testComments(self):
		self.assertRaises(BadInputException, self.interpreter.evalExpression, "(+ 3 4) )")
		self.doTest(7, "(+ 3 4) ;; )")
		self.doTest(14, """(+ 3 5) ;; ( 44
				(* 3 2)
				(+ 6 8) ; (* 7 12)""")
		
	def testEval(self):
		self.doTest(9, "(eval '(+ 4 5))")
		self.doTest(99, "(eval '(let ((x 2)) (progn (setq x 99) x)))")
		
	def testArithmetic(self):
		self.doTest("T", "(< 4 5 10 22)")
		self.doTest("NIL", "(< 44 22)")
		self.doTest("NIL", "(< 2 5 5 6)")
		
	def testIfOperator(self):
		self.doTest(11, "(if 43 11 10)")
		self.doTest(11, "(if (- 4 3) 11 10)")
		self.doTest(10, "(if NIL 11 10)")
		
		self.assertRaises(BadInputException, self.interpreter.evalExpression, ("(if T)"))

	def testLet(self):
		self.doTest(5, "(let ((x 5)) x)")
		self.doTest(20, "(let ((x (* 2 (+ 5 5)))) x)")
		self.doTest(6, "(let ((x 2) (y 4)) (+ x y))")
		self.doTest(14, "(let ((x 10)) (let ((y 4)) (+ x y)))")
		self.doTest(20, "(let ((w 4)) (setq w 5) (* w 4))")
		self.doTest(14, "(let ((x 12)) (let ((ff (lambda (y) (+ x y)))) (let ((x 55)) (funcall ff 2))))")
		
	def testProgn(self):
		self.doTest(5, "(progn 3 5)")
		self.doTest(4, "(progn (+ 3 4) (- 5 1))")
	
	def testSetq(self):
		self.doTest(12, "(let ((a 10)) (setq a 12))")
		self.doTest(99, "(let ((x 2)) (progn (setq x 99) x))")
		self.doTest(2, "(let ((x 2)) (progn (let ((x 3)) (setq x 4)) x))")
		self.doTest(12, "(let ((x 2) (y 6)) (progn (setq x 4 y 8) (+ x y)))")
		
	def testQuote(self):
		self.doTest("(+ x 5)", "(quote (+ x 5))")
		self.doTest("x", "(let ((x 12)) (quote x))")
		self.doTest("(x)", "(quote (x))")
		self.doTest("(quote (x))", "(quote (quote (x)))")
		
	def testLiteralQuote(self):
		self.doTest("(+ x 5)", "'(+ x 5)")
		self.doTest("x", "(let ((x 12)) 'x)")
		self.doTest("(x)", "'(x)")
		#self.doTest("'(x)", "''(x)")
		
	def testBackQuote(self):
		self.doTest("(+ x 5)", "`(+ x 5)")
		self.doTest("x", "(let ((x 12)) `x)")
		self.doTest("x", "`x")
		self.doTest("(+ 4 10)", "`(+ 4 ,(* 2 5))")
		self.doTest("(+ (* 3 3) 10)", "`(+ (* 3 3) ,(* 2 5))")
		self.doTest("(* (+ 4 10) 11)", "`(* (+ 4 ,(+ 3 7)) 11)")
		self.doTest("(3 4 (7 8))", "`(3 4 ,(list 7 8))")
		self.doTest("(3 4 7 8)", "`(3 4 ,@(list 7 8))")
	
	def testCons(self):
		self.doTest("(5 3 4)", "(cons 5 '(3 4))")
		self.doTest("(1 2 3)", "(cons 1 (cons 2 (cons 3 ())))")
		self.doTest("(5 3 4)", "(cons (+ 2 3) '(3 4))")
		self.doTest("(22)", "(cons 22 NIL)")
		self.doTest("(22)", "(let ((x ())) (cons 22 x))")
	
	def testList(self):
		self.assertRaises(BadInputException, self.interpreter.evalExpression, ("(list (+ x 5))"))
		self.doTest("(3 4 5)", "(list 3 4 5)")
		
		self.doTest("(1 2 3)", "(let ((x 2)) (list 1 x 3))")
		self.doTest("(10 20 (30 40))", "(list 10 20 (list 30 40))")
		self.doTest("NIL", "(list)")
		self.assertRaises(BadInputException, self.interpreter.evalExpression, ("(list))"))
		
	def testCar(self):
		self.doTest("NIL", "(car ())")
		self.doTest("NIL", "(car NIL)")
		self.doTest(5, "(car (cons 5 ()))")
		self.doTest(8, "(car (quote (8 2 4)))")
		self.doTest("+", "(car (quote (+ 2 4)))")
		self.doTest(5, "(car (list 5 6))")
		self.doTest(5, "(car (list (+ 1 4) 6))")
		self.doTest("(5 6)", "(car (list (list 5 6) 9 10))")
		
	def testCdr(self):
		self.doTest("(2 4)", "(cdr (quote (+ 2 4)))")
		self.doTest("NIL", "(cdr (list 2))")
		self.doTest("NIL", "(cdr (list))")
		self.doTest("((* 3 4))", "(cdr (quote (2 (* 3 4))))")
		self.doTest("(6)", "(cdr (list 5 6))")
		self.assertEqual(6, self.interpreter.evalExpression("(car (cdr (list 5 6 9 10)))").getValue())
		
	def testEval(self):
		self.assertEqual(5, self.interpreter.evalExpression("(eval (quote (+ 3 2)))").getValue())
		self.assertEqual(5, self.interpreter.evalExpression("(eval `(+ 3 2))").getValue())
		
	def testAtom(self):
		self.doTest("T", "(atom (+ 4 3))")
		self.doTest("T", "(atom (quote 43))")
		self.doTest("T", "(atom (quote ()))")
		self.doTest("T", "(atom (quote +))")
		self.doTest("NIL", "(atom (quote (+)))")
		self.doTest("NIL", "(atom (quote (+ 4 5)))")
		self.doTest("NIL", "(atom (quote (4)))")
		self.doTest("NIL", "(atom (list 5 4))")
		
	def testEq(self):
		self.doTest("T", "(let ((x (list 3 4)) (y (list 3 4))) (eq x y))")
		self.doTest("NIL", "(let ((x (list 3 4)) (y (list 3))) (eq x y))")
		self.doTest("NIL", "(eq (list 3 4) (list 3 4))")
		self.assertValue("T", self.interpreter.evalExpression("(eq (+ 3 4) (+ 3 4))").getValue())
		self.assertValue("NIL", self.interpreter.evalExpression("(eq '(+ 3 4) '(+ 3 4))").getValue())
		self.assertValue("T", self.interpreter.evalExpression("(eq 'fddf 'fddf)").getValue())
		self.assertValue("NIL", self.interpreter.evalExpression("(eq '(fddf) '(fddf))").getValue())
		
	def testLambda(self):
		self.doTest(9, "((lambda (x) (* x x)) 3)")
		self.doTest(19, "((lambda (x y) (+ (* 2 x) y)) 4 (+ 4 7))")
		self.doTest(96, "(let ((x 32)) ((lambda (y) (* x y)) 3))")
		self.doTest(144, "(let ((x ((lambda (x) (* x x)) 12))) x)")
		self.doTest("#<FUNCTION LAMBDA>", "(lambda (x) x)")
		self.doTest("#<FUNCTION LAMBDA>", "(let ((x (lambda (x y) (* x y)))) x)")
		self.doTest(10, "(let ((ff (lambda (x) (* 2 x)))) (funcall ff 5))")
		
		self.assertEqual(FUN_OBJ, self.interpreter.evalExpression("(lambda (x) x)").getType())
		self.assertRaises(BadInputException, self.interpreter.evalExpression, "(let ((ff (lambda (x) (* 2 x)))) (ff 5))")
		
	def testDefun(self):
		self.doTest("f", "(defun f (x y z) (* x y (+ 2 z)))")
		self.assertEqual(36, self.interpreter.evalExpression("(f 2 3 4)").getValue())
		self.assertEqual(FUN_OBJ, self.interpreter.evalExpression("#'f").getType())
		self.doTest("#<FUNCTION f>", "#'f")
		self.doTest("g", "(let ((a 15)) (defun g (x) (* x a)))")
		self.assertEqual(30, self.interpreter.evalExpression("(g 2)").getValue())
		self.assertEqual(10, self.interpreter.evalExpression("(let ((a 15)) (progn (defun h (x) (* x a)) (setq a 10)))").getValue())
		self.assertEqual(20, self.interpreter.evalExpression("(h 2)").getValue())
		self.assertEqual(20, self.interpreter.evalExpression("(let ((a 30)) (h 2))").getValue())
		self.interpreter.evalExpression("(defun fib (x) (if (= x 0) 1 (* x (fib (- x 1)))))")
		self.assertEqual(120, self.interpreter.evalExpression("(fib 5)").getValue())
		self.doTest("mymap", "(defun mymap (l f) (if (car l) (cons (funcall f (car l)) (mymap (cdr l) f))))")
		self.doTest("(2 4 6)", "(mymap '(1 2 3) (lambda (x) (* 2 x)))")
		
		self.interpreter.evalExpression("(defun last (l) (if (cdr l) (last (cdr l)) (car l)))")
		self.assertEqual(6, self.interpreter.evalExpression("(last '(3 4 6))").getValue())
		
		self.doTest("h", "(let ((yy 33)) (defun h () (progn (setq yy (+ yy 10)) yy)))")
		self.doTest(43, "(h)")
		self.doTest(53, "(h)")
		self.doTest(63, "(let ((yy 1)) (h))")
		
	def testDefmacro(self):
		self.doTest("when.", "(defmacro when. (test &rest forms) `(if ,test (progn ,@forms)))")
		
		# condition is satisfied - we can see that there was some side effects
		self.doTest(53, "(let ((x 33)) (progn (when. (= x 33) (setq x (+ x 10)) (setq x (+ x 10))) x))")
		
		# condition is not satisfied - no side effects which means that parameters wasn't evaluated
		self.doTest(33, "(let ((x 33)) (progn (when. (= x 34) (setq x (+ x 10)) (setq x (+ x 10))) x))")
		
		self.doTest(44, "(let ((x 44)) (when (= 3 3) x))")
		self.doTest("x", "(let ((x 44)) (when (= 3 3) 'x))")
		
		self.doTest("backwards", "(defmacro backwards (code) (reverse code))")
		self.doTest(5, "(backwards (3 2 +))")
		
		# check if deeper macro expanding works
		self.doTest(2, "(backwards (2 (= 3 3) when))")
		self.doTest("NIL", "(backwards (2 (= 4 3) when))")
		
		self.doTest("f", "(defmacro f (x) `(when ,x 3))")
		self.doTest(3, "(f (= 6 6))")
		self.doTest("NIL", "(f (= 6 2))")
		
	def testGensym(self):
		self.doTest("evaluate", "(defmacro evaluate (condition &rest body) `(let ((cond ,condition)) (when cond ,@body)))")
		self.doTest("T", "(let ((cond 56)) (evaluate (= 3 3) cond))")
		
		# the same macro as above except of gensym usage
		self.doTest("evaluate2", "(defmacro evaluate2 (condition &rest body) `(let ((cond-name (gensym))) (let ((cond-name ,condition)) (when cond-name ,@body))))")
		self.doTest(56, "(let ((cond 56)) (evaluate2 (= 3 3) cond))")
		
		self.assertEqual("__##G", self.interpreter.evalExpression("(gensym)").getValue()[:5])
		
	def testRest(self):
		self.doTest("f", "(defun f (x &rest r) ( car (cdr r)))")
		self.assertEqual(5, self.interpreter.evalExpression("(f 1 4 5 8 9)").getValue())
		self.assertRaises(BadInputException, self.interpreter.evalExpression, ("(def gg (x &rest) x)"))
		self.assertRaises(BadInputException, self.interpreter.evalExpression, ("(def gg (x &rest rest abc) x)"))
		
	def testOptional(self):
		self.doTest("f", "(defun f (a &optional (b 3) (c 4)) (+ a b c))")
		self.doTest(9, "(f 2)")
		self.doTest(12, "(f 2 8 2)")
		
		self.doTest("g", "(defun g (a &optional b) b)")
		self.doTest("NIL", "(g 3)")
		self.doTest(55, "(g 33 55)")
		
		self.doTest("foo", "(defun foo (a b &optional (c 3 c-supplied-p)) (list a b c c-supplied-p))")
		self.doTest("(1 2 3 NIL", "(foo 1 2)")
		self.doTest("(1 2 3 T)", "(foo 1 2 3)")
		self.doTest("(1 2 4 T)", "(foo 1 2 4)")
		
	def testKey(self):
		self.doTest("f", "(defun f (&key a b c) (list a b c))")
		self.doTest("(2 4 6)", "(f 2 4 6)")
		self.doTest("(2 4 6)", "(f :b 4 :c 6 :a 2)")
		self.doTest("(2 NIL 6)", "(f :c 6 :a 2)")
		
		self.doTest("foo", "(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b))) (list a b c b-supplied-p))")

		self.doTest("(1 0 1 NIL", "(foo :a 1)")
		self.doTest("(0 1 1 T)", "(foo :b 1)")
		self.doTest("(0 1 4 T)", "(foo :b 1 :c 4)")
		self.doTest("(2 1 4 T)", "(foo :a 2 :b 1 :c 4)")
		
	def testString(self):
		self.doTest("hello world", '"hello world"')
		self.doTest("hello", '(let ((x "hello")) x)')
	
	def testFuncall(self):
		self.assertEqual(16, self.interpreter.evalExpression("(let ((fn (lambda (x) (* x x)))) (funcall fn 4))").getValue())
		self.doTest("f", "(defun f (x) (* x x))")
		self.assertEqual(9, self.interpreter.evalExpression("(funcall #'f 3)").getValue())
		
	def testReadFile(self):
		subprocess.call(["python", "lisp_interpreter.py", "test.lisp"], stdout=open("out.txt", "w"))
		f = open("out.txt", "r")
		self.assertEqual("8\n", f.read())
		
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
	'''(defun cons. (el l)
	      `(list ,el ,@l))'''
	      
if __name__ == "__main__":
	unittest.main()