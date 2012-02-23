class Env:	
	def __init__(self, variables, funDict):
		self.variables = variables
		self.funDict = funDict

class Environment:
	def __init__(self, globalEnv, lexicalEnv):
		self.globalEnv = globalEnv
		self.lexicalEnv = lexicalEnv
		
	def getVariable(self, varName):
		val = self.lexicalEnv.variables.get(varName)
		if val:
			return val
		return self.globalEnv.variables.get(varName)
		
	def getFunction(self, fnName):
		val = self.lexicalEnv.funDict.get(fnName)
		if val:
			return val
		return self.globalEnv.funDict.get(fnName)
		