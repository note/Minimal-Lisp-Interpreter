class Value:
	def __init__(self, val):
		self.value = val

class Env:	
	def __init__(self, variables, funDict):
		self.variables = variables
		self.funDict = funDict

class Environment:
	def __init__(self, globalEnv, lexicalEnv):
		self.globalEnv = globalEnv
		self.lexicalEnv = lexicalEnv
			
	def setLexicalVariable(self, varName, value):
		self.lexicalEnv.variables[varName].value = value
		
	def overwriteLexicalVariable(self, varName, value):
		self.lexicalEnv.variables[varName] = Value(value)
		
	def getVariable(self, varName):
		val = self.lexicalEnv.variables.get(varName)
		if val:
			return val.value
		return self.globalEnv.variables.get(varName)
		
	def getFunction(self, fnName):
		val = self.lexicalEnv.funDict.get(fnName)
		if val:
			return val
		return self.globalEnv.funDict.get(fnName)
	
	def overwriteLexicalVariables(self, newVariablesDict):
		for k, v in newVariablesDict.iteritems():
			self.lexicalEnv.variables[k] = Value(v)
	
	def getCopy(self):
		return Environment(Env(self.globalEnv.variables, self.globalEnv.funDict), Env(dict(self.lexicalEnv.variables), dict(self.lexicalEnv.funDict)))