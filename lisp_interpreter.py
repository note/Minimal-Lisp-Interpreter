#!/usr/bin/python
# -*- coding: utf-8 -*-

import subprocess
import sys
from interpreter import Interpreter

def main(argv=sys.argv):
	interpreter = Interpreter()
	if len(argv)>1: # load code from file
		try:
			print interpreter.interpretFile(argv[1])
		except IOError:
			print "File " + argv[1] + " cannot be opened"
	else: # interactive mode, very simple so far (only one-line expressions)
		if subprocess.call(["which", "rlwrap"], stdout=open("/dev/null", "w")) == 0:
			subprocess.call(["rlwrap", "python", "interpreter.py"])
		else:
			interpreter.startInteractiveMode()
	
if __name__ == "__main__":
	main()