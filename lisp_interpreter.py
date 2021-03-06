#!/usr/bin/python
# -*- coding: utf-8 -*-

import subprocess
import sys
from src.interpreter import Interpreter

def main(argv=sys.argv):
	interpreter = Interpreter()
	if len(argv)>1:
		if argv[1] == "-h":
			print "Minimal Lisp Interpreter"
			print "https://github.com/note/Minimal-Lisp-Interpreter\n"
			print "Usage: \n./lisp_interpreter [FILE]"
			print "To exit interactive mode use quit"
		else:
			try: # load lisp code from file
				print interpreter.interpretFile(argv[1])
			except IOError:
				print "File " + argv[1] + " cannot be opened"
	else: # interactive mode, very simple so far (only one-line expressions)
		if subprocess.call(["which", "rlwrap"], stdout=open("/dev/null", "w")) == 0:
			subprocess.call(["rlwrap", "python", "src/interpreter.py"])
		else:
			interpreter.startInteractiveMode()
	
if __name__ == "__main__":
	main()
