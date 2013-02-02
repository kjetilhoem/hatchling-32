package no.k.m6809

import RunMe._

class CasPrinter extends CasFile {
	def blockType(b: Int) = println("Block type: " + blockTypeString(b))
	def blockLength(b: Int) = println("Block length: " + hexByte(b))
	def programName(name: String) = println("Program name: " + name)
	def fileId(b: Int) = println("File ID: " + fileIdString(b))
	def asciiFlag(b: Int) = println("ASCII flag: " + asciiFlagString(b))
	def gapFlag(b: Int) = println("Gap flag: " + gapFlagString(b))
	def defaultExecAddress(w: Int) = println("Default EXEC address: " + hexWord(w))
	def defaultLoadAddress(w: Int) = println("Default LOAD address: " + hexWord(w))
	def checksum(b: Int) = println("Checksum: " + hexByte(b))
	def trailer(b: Int) = println("Trailer: " + hexByte(b))
	def beginBlock() = println("\nBEGIN BLOCK")
	def endBlock() = println("END BLOCK")
	def data(a: Array[Byte]) = 
		println(
			(a grouped 16 map { r =>  
				"\n" :: (r map { 
					_ formatted("%02X ")
				}).mkString :: " : " :: (r map {
					_ & 0xFF match {
						case it if it < 0x20 => "[" + it + "]"
						case it if it >= 0x20 => it toChar
					}
				}).toList
			}).flatten
		mkString)
}
  
