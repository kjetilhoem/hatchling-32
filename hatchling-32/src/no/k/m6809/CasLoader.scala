package no.k.m6809

import RunMe._

class CasLoader extends CasFile {
	var blockType = 0

	def blockType(b: Int) = blockType = b
	def blockLength(b: Int) {}
	def programName(name: String) = if (DBG_LOG_CAS_INFO) println("PROGRAM NAME: " + name)
	def fileId(b: Int) = if (DBG_LOG_CAS_INFO) println("FILE ID: " + b)
	def asciiFlag(b: Int) = {}
	def gapFlag(b: Int) = {}
	def defaultExecAddress(w: Int) = { execAddress = w; if (DBG_LOG_CAS_INFO) println("EXEC ADDR: " + hexWord(execAddress)) }
	def defaultLoadAddress(w: Int) = { loadAddress = w; if (DBG_LOG_CAS_INFO) println("LOAD ADDR: " + hexWord(loadAddress)) }
	def checksum(b: Int) = {}
	def trailer(b: Int) = {}
	def beginBlock() = {}
	def endBlock() = {}
	def data(a: Array[Byte]) = 
		for (i <- 0 until a.length) {
			__ram(loadAddress) = a(i)
			loadAddress += 1
		}
}
