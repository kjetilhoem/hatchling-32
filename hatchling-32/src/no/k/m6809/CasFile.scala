package no.k.m6809

trait CasFile {
	def blockType(b: Int)
	def blockLength(b: Int)
	def programName(name: String)
	def fileId(b: Int)
	def asciiFlag(b: Int)
	def gapFlag(b: Int)
	def defaultExecAddress(w: Int)
	def defaultLoadAddress(w: Int)
	def checksum(b: Int)
	def trailer(b: Int)
	def beginBlock()
	def endBlock()
	def block(function: => Any): Any = {
		beginBlock()
		val result = function
		endBlock()
		result
	}
	def data(a: Array[Byte])
}
	
  
