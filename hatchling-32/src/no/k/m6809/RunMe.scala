package no.k.m6809

import java.io.FileInputStream
import java.io.InputStream
import java.util.Arrays
import scala.swing.MainFrame
import scala.actors.Actor
import scala.actors.Actor._
import scala.swing.SimpleSwingApplication

import scala.swing._
import scala.swing.Swing
import scala.swing.Dimension
import scala.swing.event._

object RunMe extends SimpleSwingApplication {
	val DBG_STOP_PERIODICALLY = false
	val DBG_BREAK_ON_INSTRUCTION = null //"LBSR"
	val DBG_EXEC_LIMIT = -1//10950
	val DBG_STEP_COUNT = -1
	
	val ADDRESS_SPACE = 0x10000
	val RAM_END = 0x8000
	val ROM_BASIC_BEGIN = RAM_END
	val ROM_BASIC_END = 0xC000
		
	val ROM_BASIC_BINARY = "/home/kjetil/dragon32/rom/BASIC.ROM"
	val AUTOLOAD_CAS_BINARY = "/home/kjetil/dragon32/donkeyking/donkey1.cas"
	val CAS_LOAD_MAX_BLOCKS = 8
	
	val TXT_START = 0x0400
	val TXT_SIZE = 0x0200
  
	val __ram:Array[Byte] = new Array(ADDRESS_SPACE)
	def top = new Display(__ram, TXT_START)

	var terminate = false
	var analysis = ""

	object M6809 {
		object CC {
			object FLAG_BIT {
				val C = 0
				val V = 1
				val Z = 2
				val N = 3
				val I = 4
				val H = 5
				val F = 6
				val E = 7
			}
		}
		object INDEX_REG {
			val X = 0
			val Y = 1
			val U = 2
			val S = 3
		}
		object INDEX_MODE {
			val POST_INC_8 = 0
			val POST_INC_16 = 1
			val PRE_DEC_8 = 2
			val PRE_DEC_16 = 3
			val PC_RELATIVE_16 = 13			
			val EXTENDED_INDIRECT = 15
		}
	}
		
	actor {
	  	initRomBasic
	  	loadCas
  		Cpu.setPC(execAddress)
	  	
	  	var limit = 0
	  	var stepCounter = DBG_STEP_COUNT
	  	
	  	while (limit != DBG_EXEC_LIMIT && !terminate) {    
	  		limit += 1
			stepCounter -= 1
			if (stepCounter == 0) {				
				System.in.read()
				stepCounter = DBG_STEP_COUNT
			}
		
	  		if (Cpu.PC == 0xBBE5) {
	  			println("==> CALL 0xBBE5 ROM \"Keyboard input\"")
	  			Cpu.setA(if (memW(Cpu.S) == 0x2C14 || memW(Cpu.S) == 0x2C33) 0x4E else 0)
	  			Cpu.setPC(memW(Cpu.S))
	  			Cpu.setS(Cpu.S + 2)
	  		} else if (Cpu.PC == 0xB87E) {
	  			println("==> CALL 0xB87E ROM \"??\"")
	  		} else if (Cpu.PC == 0xBDE7) {
	  			println("==> CALL 0xBDE7 ROM \"8021 (BDE7)  Cassette on for reading:disable IRQ,FIRQ "+
                   "calls cassette on(8015) and usses bit "+ 
                   "sync information to get into sync.\"")
                 
                // TODO effect
                 
	  			Cpu.setPC(memW(Cpu.S))
	  			Cpu.setS(Cpu.S + 2)
	  		}
	  		
	  		/*
      A006 (B93E) Cassette block in from tape:tape should be 
                  up to speed and in bit sync.Set up through
                  $7C,7D,7E:7F.On exit CC.Z is zero if I/O
                  error occured and $81 holds the source of
                  error.IRQ and FIRQ remain disabled.
	  		 
	  		 */
    
	  		val b0 = memB(Cpu.PC)
			val opcode = if (b0 == 0x10 || b0 == 0x11) b0 << 8 | memB(Cpu.PC + 1) else b0
    
			InstructionSet.mode(opcode)(opcode, Cpu.PC)
    
			println(Cpu.dump)
			print(analysis)
			analysis = ""
    
			if (InstructionSet.mnemonic(opcode) == DBG_BREAK_ON_INSTRUCTION) terminate = true
			if (terminate) println("termination op: " + InstructionSet.mnemonic(opcode))
	  	}
  
	  	if (limit == DBG_EXEC_LIMIT) println("execution limit reached")
  
	    println("Press enter to exit.")
	    System.in.read
		top.closeOperation
	}
  
  
	def maskLo(n:Int) = bit(n) - 1
	def maskLS(n:Int, v:Int) = v & maskLo(n)
	def maskLS8(v:Int) = maskLS(8, v)
	def maskLS16(v:Int) = maskLS(16, v)
	
	def complement(v:Int) = -1 ^ v
	def bit(n:Int) = 1 << n
	def nbit(n:Int) = complement(bit(n))
	
	def setBit(num:Int, value:Int) = value | bit(num)
	def resetBit(num:Int, value:Int) = value & nbit(num)
	def alterBit(set:Boolean) = if (set) setBit _ else resetBit _ 
	
	def utos8(v:Int) = if (v < 0x80) v else (v - 0x100)
	def utos16(v:Int) = if (v < 0x8000) v else (v - 0x10000)	
		
	
	
	
	def readLeader: Boolean =
    	readByte match {
    		case 0x55 => readLeader
    		case 0x3C => true
    		case _ => false
    	}
  
	def readProgramName = new String(readBytes(8)) 
  
  
	def readBytes(num: Int) = {
		val bytes = new Array[Byte](num)
		cas.read(bytes)
		bytes
	}
	
	def readBlockLength = cas read
  
	def blockTypeString(t: Int) = 
		t match {
			case BT_NAMEFILE => "NAMEFILE"
			case BT_DATA => "DATA"
			case BT_EOF => "EOF"
    	}
  
	def fileIdString(b: Int) =
    	b match {
    		case 0x00 => "BASIC program"
    		case 0x01 => "Data file"
    		case 0x02 => "Binary file"
  		}
  
	def asciiFlagString(b: Int) =
		b match {
			case 0x00 => "Binary"
			case 0xFF => "ASCII"
		}
  
	def gapFlagString(b: Int) =
		b match {
			case 0x00 => "Continuous (binary/BASIC files)"
			case 0xFF => "Stopping (data files)"
  		}
  
	def readAddress = hexWord(readWord)
	def readChecksum = hexByte(readByte)
	def readTrailer = hexByte(readByte)  
	def readByte = cas.read
	def readWord = readByte << 8 | readByte
  
		
	def hex(nibbles:Int, v:Int) = maskLS(4 * nibbles, v) formatted (nibbles formatted "$%%0%dX")

	
	def hexByte(b: Int) = maskLS8(b) formatted "$%02X"
	def hexWord(w: Int) = maskLS16(w) formatted "$%04X"  
	def hexMemSection(begin:Int, end:Int) = ("" /: (begin until end toList).map(memB(_) formatted "%02X"))(_+_)

	def binByte(b: Int) = ("" /: bits(b))(_+_)
	def bits(b: Int) = (7 to 0 by -1 toList).map(b >> _ & 1)

	def initRomBasic =  {
		val rom = new FileInputStream(ROM_BASIC_BINARY)
	  
		var b = rom.read()
		var ix = ROM_BASIC_BEGIN
	  
		while (b != -1) {
			__ram(ix) = (b toByte)
			ix += 1
			b = rom.read()
		}
	  
		rom.close()
	}
  
	val cas = new FileInputStream(AUTOLOAD_CAS_BINARY)
	
	var loadAddress = 0
	var execAddress = 0
  
	class CasLoader extends CasFile {
		var blockType = 0
    
		def blockType(b: Int) = blockType = b
		def blockLength(b: Int) {}
		def programName(name: String) = println("PROGRAM NAME: " + name)
		def fileId(b: Int) = println("FILE ID: " + b)
		def asciiFlag(b: Int) = {}
		def gapFlag(b: Int) = {}
		def defaultExecAddress(w: Int) = { execAddress = w; println("EXEC ADDR: " + hexWord(execAddress)) }
		def defaultLoadAddress(w: Int) = { loadAddress = w; println("LOAD ADDR: " + hexWord(loadAddress)) }
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
	
  
	val BT_NAMEFILE = 0x00
	val BT_DATA = 0x01
	val BT_EOF = 0xFF
  
	def loadCas {
		val f = new CasLoader
		import f._
		
		var blocksLeft = CAS_LOAD_MAX_BLOCKS
		
		while (readLeader && blocksLeft > 0)
			block {
				blocksLeft -= 1
				
				val blkType = readByte
				val blkLen = readByte

				println(blockTypeString(blkType) + " BLOCK: " + hexByte(blkLen) + " BYTES @ " + hexWord(loadAddress))
				
				blockType(blkType)		
				blockLength(blkLen)
		
				blkType match {
					case BT_NAMEFILE => {
						programName(readProgramName)
						fileId(readByte)
						asciiFlag(readByte)
						gapFlag(readByte)
						defaultExecAddress(readWord)
						defaultLoadAddress(readWord)
					}
					case BT_DATA => data(readBytes(blkLen))
					case BT_EOF => {}
				}
	    
				checksum(readByte)
				trailer(readByte)
			}
  
		cas.close();
		println("LOAD END ADDR: " + hexWord(loadAddress))
	}
	
	
	object Cpu {
		import M6809.CC.FLAG_BIT._
	  
		private var __PC = 0
		private var __B = 0
		private var __A = 0
		private var __X = 0
		private var __Y = 0
		private var __U = 0
		private var __S = 0x7E36
		private var __CC = 0
		private var __DP = 0
		
		def PC = __PC
		def B = __B
		def A = __A
		def X = __X
		def Y = __Y
		def U = __U	
		def S = __S
		def CC = __CC
		def DP = __DP
		def D = __A << 8 | __B
		
		def setA(b:Int) = __A = maskLS8(b)
		def setB(b:Int) = __B = maskLS8(b)
		def setX(w:Int) = __X = maskLS16(w)
		def setY(w:Int) = __Y = maskLS16(w)    
		def setU(w:Int) = __U = maskLS16(w)    
		def setS(w:Int) = __S = maskLS16(w)    
		def setPC(w:Int) = __PC = maskLS16(w)
		def setCC(b:Int) = __CC = maskLS8(b)    
		def setDP(b:Int) = __DP = maskLS8(b)		
		def setD(w:Int) { setA(w >> 8); setB(w) }
				
		def addX(o:Int) = setX(__X + o)
		def addY(o:Int) = setY(__Y + o)
		def addU(o:Int) = setU(__U + o)
		def addS(o:Int) = setS(__S + o)
		def addPC(o:Int) = setPC(__PC + o)
		
		def ccSet(v:Boolean, n:Int) = __CC = alterBit(v)(n, __CC)
		def cc(n:Int) = (__CC & bit(n)) != 0
		
		def ccSetC(v:Boolean) = ccSet(v, C)
		def ccSetV(v:Boolean) = ccSet(v, V)
		def ccSetZ(v:Boolean) = ccSet(v, Z)
		def ccSetN(v:Boolean) = ccSet(v, N)
		def ccSetI(v:Boolean) = ccSet(v, I)
		def ccSetH(v:Boolean) = ccSet(v, H)
		def ccSetF(v:Boolean) = ccSet(v, F)
		def ccSetE(v:Boolean) = ccSet(v, E)

		def ccC = cc(C)
		def ccV = cc(V)
		def ccZ = cc(Z)
		def ccN = cc(N)
    	def ccI = cc(I)
    	def ccH = cc(H)
    	def ccF = cc(F)
    	def ccE = cc(E)
    
    	def dump() = format(
			"A=%02X B=%02X X=%04X Y=%04X U=%04X S=%04X DP=%02X PC=%04X CC=%s%s%s%s%s%s%s%s [S]=%04X ", 
			A, B, X, Y, U, S, DP, PC, 
			if (ccE) "E" else "e",
			if (ccF) "F" else "f",
			if (ccH) "H" else "h",
			if (ccI) "I" else "i",
			if (ccN) "N" else "n",
			if (ccZ) "Z" else "z",
			if (ccV) "V" else "v",
			if (ccC) "C" else "c",
			memW(S)
    	)
	}
	
	def memSB(address: Int) = checkGet8(address, __ram(address))	
	def memSW(address: Int) = checkGet16(address, (maskLS8(__ram(address)) << 8) | maskLS8(__ram(address + 1)))		
	def memB(address: Int) = maskLS8(memSB(address))
	def memW(address: Int) = maskLS16(memSW(address))
		
	def setMemB(address:Int, b:Int) = {
		checkSet(address, b, 1)
		__ram(address) = b toByte
	}
	def setMemW(address:Int, w:Int) {
		checkSet(address, w, 2)
		__ram(address) = (w >> 8 toByte)
		__ram(address + 1) = w toByte
	}
	def checkSet(at:Int, v:Int, width:Int) = if (at < 0x01e9 || at >= RAM_END) ioWrite(at, v, width)
	
	
	def ioWrite(at:Int, v:Int, width:Int) = {
			analysis += ("                    " + hex(width, v) + " ==> " + hexWord(at) + " \"" +
				(at match {       
					case 0x0068 => "Current Line number (0xffff in direct mode)"
					case 0x0070 => "Cassette EOF flag"
					case 0x007E => "%DBADR% Cassette I/O Buffer address - contains 1 + End address of last program loaded"
					case 0x0082 => "Cassette I/O - Pulse width counter"
					case 0x0088 => "Current VDU cursor addr"
					case 0x00B0 => "Ptr to start of USR table ($0134; DOS - $0683)"
					case 0x00B7 => "Ptr to last byte+1 of current graphics mode ($0c00 w/o Dos)"
					case 0x00BA => "Ptr to first byte of current graphics mode ($0600)"
					case 0x0167 => "Setting to 0x39 (RTS) allows use of SCREEN 0,1 etc. ??{1}"
					case 0x01D1 => "Cassette filename length in range 0-8"
					case 0xFF40 => "DOS - Disk Controller command/status reg"
					case 0xFF20 => { __out_PIA1_A_dataReg(v); "PIA 1 A side Data reg" }
					case 0xFF22 => "PIA 1 B side Data reg"
					case 0xFF23 => "PIA 1 B side Control reg"
					case 0x0072 => "Restart vector"
					case 0xFFC7 => "SAM Display Offset bit F0 (set)"
					case 0xFFC8 => "SAM Display Offset bit F1 (clear)"
					case 0xFFCB => "SAM Display Offset bit F2 (set)"
					case 0xFFCD => "SAM Display Offset bit F3 (set)"
					case 0xFFC0 => "SAM VDG Reg V0 (clear)"
					case 0xFFC3 => "SAM VDG Reg V1 (set)"
					case 0xFFC5 => "SAM VDG Reg V2 (set)"
					case 0xB64C => "CLOSE token dispatch address"
        
					case _ => {
						println("UNHANDLED I/O WRITE")
						System.exit(0)
						""
					}
				}) + "\"\n")
	}
	
	def __out_PIA1_A_dataReg(v:Int) {
	  println("\nOUT PIA1 A data reg: " + hexByte(v))
	}
	def __in_PIA1_A_dataReg(v:Byte) = {
	  println("\nIN PIA1 A data reg: " + hexByte(v))
	  v
	}
	

	def checkGet8(at:Int, v:Int) = checkGet(at, hexByte(v), v)
	def checkGet16(at:Int, v:Int) = checkGet(at, hexWord(v), v)
	
	def checkGet(at:Int, vStr:String, v:Int) = {
		if (at < 0x01e9 || at >= ROM_BASIC_END)
			analysis += ("                    " + vStr + " <== " + hexWord(at) + " \"" +
				(at match {
					case 0x0068 => "Current Line number (0xffff in direct mode)"
					case 0x0070 => "Cassette EOF flag"
					case 0x007E => "%DBADR% Cassette I/O Buffer address - contains 1 + End address of last program loaded"
					case 0x0082 => "Cassette I/O - Pulse width counter"
					case 0x0084 => "Cassette I/O - Bit phase flag"
					case 0xFF20 => { __ram(at) = __in_PIA1_A_dataReg(__ram(at)); "PIA 1 A side Data reg" }
          
					case _ => {
						println("UNHANDLED GET " + hexWord(at) + " ==> " + vStr)
						System.exit(0)
						""
					}
				}) 
				+ "\"\n")
				
		at match {
		  case 0xFF20 => 0xFF
		  case _ => v
		}
	}
	
	
	object InstructionSet {
		import Cpu._
		import AddressingModes._

		def unimpl = terminate = true

		def operation(opcode:Int) = get(opcode)._4
		def mode(opcode:Int) = get(opcode)._3
		def mnemonic(opcode:Int) = get(opcode)._2

		def illegalop(ea:Int) = unimpl
		
		def get(opcode:Int) = OPS.find(_._1 == opcode).getOrElse(-1, "???", illegal _, illegalop _)
		
		val OPS = Array(
			(0x00, "NEG", direct _, neg _),
			(0x03, "COM", direct _, com _),
			(0x04, "LSR", direct _, lsr _),
			(0x06, "ROR", direct _, ror _),
			(0x07, "ASR", direct _, asr _),
			(0x08, "LSL", direct _, lsl _),
			(0x09, "ROL", direct _, rol _),
			(0x0A, "DEC", direct _, dec _),
			(0x0C, "INC", direct _, inc _),
			(0x0D, "TST", direct _, tst _),
			(0x0E, "JMP", direct _, jmp _),
			(0x0F, "CLR", direct _, clr _),
			(0x12, "NOP", inherent _, nop _),
			(0x13, "SYNC", inherent _, sync _),
			(0x16, "LBRA", relative16 _, lbra _),
			(0x17, "LBSR", relative16 _, lbsr _),
			(0x19, "DAA", inherent _, daa _),
			(0x1A, "ORCC", immediate8 _, orcc _),
			(0x1C, "ANDCC", immediate8 _, andcc _),
			(0x1D, "SEX", inherent _, sex _),
			(0x1E, "EXG", inherent _, exg _),
			(0x1F, "TFR", inherent _, tfr _),
			(0x20, "BRA", relative8 _, bra _),
			(0x21, "BRN", relative8 _, brn _),
			(0x22, "BHI", relative8 _, bhi _),
			(0x23, "BLS", relative8 _, bls _),
			(0x24, "BHS", relative8 _, bhs _),
			(0x25, "BLO", relative8 _, blo _),
			(0x26, "BNE", relative8 _, bne _),
			(0x27, "BEQ", relative8 _, beq _),
			(0x28, "BVC", relative8 _, bvc _),
			(0x29, "BVS", relative8 _, bvs _),
			(0x2A, "BPL", relative8 _, bpl _),
			(0x2B, "BMI", relative8 _, bmi _),
			(0x2C, "BGE", relative8 _, bge _),
			(0x2D, "BLT", relative8 _, blt _),
			(0x2E, "BGT", relative8 _, bgt _),
			(0x2F, "BLE", relative8 _, ble _),
			(0x30, "LEAX", indexed _, leax _),
			(0x31, "LEAY", indexed _, leay _),
			(0x32, "LEAS", indexed _, leas _),
			(0x33, "LEAU", indexed _, leau _),
			(0x34, "PSHS", inherent _, pshs _),
			(0x35, "PULS", inherent _, puls _),
			(0x36, "PSHU", inherent _, pshu _),
			(0x37, "PULU", inherent _, pulu _),
			(0x39, "RTS", inherent _, rts _),
			(0x3A, "ABX", inherent _, abx _),
			(0x3B, "RTI", inherent _, rti _),
			(0x3C, "CWAI", inherent _, cwai _),
			(0x3D, "MUL", inherent _, mul _),
			(0x3E, "RESET", inherent _, reset _),
			(0x3F, "SWI", inherent _, swi _),
			(0x40, "NEGA", inherent _, nega _),
			(0x43, "COMA", inherent _, coma _),
			(0x44, "LSRA", inherent _, lsra _),
			(0x46, "RORA", inherent _, rora _),
			(0x47, "ASRA", inherent _, asra _),
			(0x48, "LSLA", inherent _, lsla _),
			(0x49, "ROLA", inherent _, rola _),
			(0x4A, "DECA", inherent _, deca _),
			(0x4C, "INCA", inherent _, inca _),
			(0x4D, "TSTA", inherent _, tsta _),
			(0x4F, "CLRA", inherent _, clra _),
			(0x50, "NEGB", inherent _, negb _),
			(0x53, "COMB", inherent _, comb _),
			(0x54, "LSRB", inherent _, lsrb _),
			(0x56, "RORB", inherent _, rorb _),
			(0x57, "ASRB", inherent _, asrb _),
			(0x58, "LSLB", inherent _, lslb _),
			(0x59, "ROLB", inherent _, rolb _),
			(0x5A, "DECB", inherent _, decb _),
			(0x5C, "INCB", inherent _, incb _),
			(0x5D, "TSTB", inherent _, tstb _),
			(0x5F, "CLRB", inherent _, clrb _),
			(0x60, "NEG", indexed _, neg _),
			(0x63, "COM", indexed _, com _),
			(0x64, "LSR", indexed _, lsr _),
			(0x66, "ROR", indexed _, ror _),
			(0x67, "ASR", indexed _, asr _),
			(0x68, "LSL", indexed _, lsl _),
			(0x69, "ROL", indexed _, rol _),
			(0x6A, "DEC", indexed _, dec _),
			(0x6C, "INC", indexed _, inc _),
			(0x6D, "TST", indexed _, tst _),
			(0x6E, "JMP", indexed _, jmp _),
			(0x6F, "CLR", indexed _, clr _),
			(0x70, "NEG", extended _, neg _),
			(0x73, "COM", extended _, com _),
			(0x74, "LSR", extended _, lsr _),
			(0x76, "ROR", extended _, ror _),
			(0x77, "ASR", extended _, asr _),
			(0x78, "LSL", extended _, lsl _),
			(0x79, "ROL", extended _, rol _),
			(0x7A, "DEC", extended _, dec _),
			(0x7C, "INC", extended _, inc _),
			(0x7D, "TST", extended _, tst _),
			(0x7E, "JMP", extended _, jmp _),
			(0x7F, "CLR", extended _, clr _),
			(0x80, "SUBA", immediate8 _, suba _),
			(0x81, "CMPA", immediate8 _, cmpa _),
			(0x82, "SBCA", immediate8 _, sbca _),
			(0x83, "SUBD", immediate16 _, subd _),
			(0x84, "ANDA", immediate8 _, anda _),
			(0x85, "BITA", immediate8 _, bita _),
			(0x86, "LDA", immediate8 _, lda _),
			(0x88, "EORA", immediate8 _, eora _),
			(0x89, "ADCA", immediate8 _, adca _),
			(0x8A, "ORA", immediate8 _, ora _),
			(0x8B, "ADDA", immediate8 _, adda _),
			(0x8C, "CMPX", immediate16 _, cmpx _),
			(0x8D, "BSR", relative8 _, bsr _),
			(0x8E, "LDX", immediate16 _, ldx _),
			(0x90, "SUBA", direct _, suba _),
			(0x91, "CMPA", direct _, cmpa _),
			(0x92, "SBCA", direct _, sbca _),
			(0x93, "SUBD", direct _, subd _),
			(0x94, "ANDA", direct _, anda _),
			(0x95, "BITA", direct _, bita _),
			(0x96, "LDA", direct _, lda _),
			(0x97, "STA", direct _, sta _),
			(0x98, "EORA", direct _, eora _),
			(0x99, "ADCA", direct _, adca _),
			(0x9A, "ORA", direct _, ora _),
			(0x9B, "ADDA", direct _, adda _),
			(0x9C, "CMPX", direct _, cmpx _),
			(0x9D, "JSR", direct _, jsr _),
			(0x9E, "LDX", direct _, ldx _),
			(0x9F, "STX", direct _, stx _),
			(0xA0, "SUBA", indexed _, suba _),
			(0xA1, "CMPA", indexed _, cmpa _),
			(0xA2, "SBCA", indexed _, sbca _),
			(0xA3, "SUBD", indexed _, subd _),
			(0xA4, "ANDA", indexed _, anda _),
			(0xA5, "BITA", indexed _, bita _),
			(0xA6, "LDA", indexed _, lda _),
			(0xA7, "STA", indexed _, sta _),
			(0xA8, "EORA", indexed _, eora _),
			(0xA9, "ADCA", indexed _, adca _),
			(0xAA, "ORA", indexed _, ora _),
			(0xAB, "ADDA", indexed _, adda _),
			(0xAC, "CMPX", indexed _, cmpx _),
			(0xAD, "JSR", indexed _, jsr _),
			(0xAE, "LDX", indexed _, ldx _),
			(0xAF, "STX", indexed _, stx _),
			(0xB0, "SUBA", extended _, suba _),
			(0xB1, "CMPA", extended _, cmpa _),
			(0xB2, "SBCA", extended _, sbca _),
			(0xB3, "SUBD", extended _, subd _),
			(0xB4, "ANDA", extended _, anda _),
			(0xB5, "BITA", extended _, bita _),
			(0xB6, "LDA", extended _, lda _),
			(0xB7, "STA", extended _, sta _),
			(0xB8, "EORA", extended _, eora _),
			(0xB9, "ADCA", extended _, adca _),
			(0xBA, "ORA", extended _, ora _),
			(0xBB, "ADDA", extended _, adda _),
			(0xBC, "CMPX", extended _, cmpx _),
			(0xBD, "JSR", extended _, jsr _),
			(0xBE, "LDX", extended _, ldx _),
			(0xBF, "STX", extended _, stx _),
			(0xC0, "SUBB", immediate8 _, subb _),
			(0xC1, "CMPB", immediate8 _, cmpb _),
			(0xC2, "SBCB", immediate8 _, sbcb _),
			(0xC3, "ADDD", immediate8 _, addd _),
			(0xC4, "ANDB", immediate8 _, andb _),
			(0xC5, "BITB", immediate8 _, bitb _),
			(0xC6, "LDB", immediate8 _, ldb _),
			(0xC8, "EORB", immediate8 _, eorb _),
			(0xC9, "ADCB", immediate8 _, adcb _),
			(0xCA, "ORB", immediate8 _, orb _),
			(0xCB, "ADDB", immediate8 _, addb _),
			(0xCC, "LDD", immediate16 _, ldd _),
			(0xCE, "LDU", immediate16 _, ldu _),
			(0xD0, "SUBB", direct _, subb _),
			(0xD1, "CMPB", direct _, cmpb _),
			(0xD2, "SBCB", direct _, sbcb _),
			(0xD3, "ADDD", direct _, addd _),
			(0xD4, "ANDB", direct _, andb _),
			(0xD5, "BITB", direct _, bitb _),
			(0xD6, "LDB", direct _, ldb _),
			(0xD7, "STB", direct _, stb _),
			(0xD8, "EORB", direct _, eorb _),
			(0xD9, "ADCB", direct _, adcb _),
			(0xDA, "ORB", direct _, orb _),
			(0xDB, "ADDB", direct _, addb _),
			(0xDC, "LDD", direct _, ldd _),
			(0xDD, "STD", direct _, std _),
			(0xDE, "LDU", direct _, ldu _),
			(0xDF, "STU", direct _, stu _),
			(0xE0, "SUBB", indexed _, subb _),
			(0xE1, "CMPB", indexed _, cmpb _),
			(0xE2, "SBCB", indexed _, sbcb _),
			(0xE3, "ADDD", indexed _, addd _),
			(0xE4, "ANDB", indexed _, andb _),
			(0xE5, "BITB", indexed _, bitb _),
			(0xE6, "LDB", indexed _, ldb _),
			(0xE7, "STB", indexed _, stb _),
			(0xE8, "EORB", indexed _, eorb _),
			(0xE9, "ADCB", indexed _, adcb _),
			(0xEA, "ORB", indexed _, orb _),
			(0xEB, "ADDB", indexed _, addb _),
			(0xEC, "LDD", indexed _, ldd _),
			(0xED, "STD", indexed _, std _),
			(0xEE, "LDU", indexed _, ldu _),
			(0xEF, "STU", indexed _, stu _),
			(0xF0, "SUBB", extended _, subb _),
			(0xF1, "CMPB", extended _, cmpb _),
			(0xF2, "SBCB", extended _, sbcb _),
			(0xF3, "ADDD", extended _, addd _),
			(0xF4, "ANDB", extended _, andb _),
			(0xF5, "BITB", extended _, bitb _),
			(0xF6, "LDB", extended _, ldb _),
			(0xF7, "STB", extended _, stb _),
			(0xF8, "EORB", extended _, eorb _),
			(0xF9, "ADCB", extended _, adcb _),
			(0xFA, "ORB", extended _, orb _),
			(0xFB, "ADDB", extended _, addb _),
			(0xFC, "LDD", extended _, ldd _),
			(0xFD, "STD", extended _, std _),
			(0xFE, "LDU", extended _, ldu _),
			(0xFF, "STU", extended _, stu _),
			
			(0x1021, "LBRN", relative16 _, lbrn _),
			(0x1022, "LBHI", relative16 _, lbhi _),
			(0x1023, "LBLS", relative16 _, lbls _),
			(0x1024, "LBHS", relative16 _, lbhs _),
			(0x1025, "LBLO", relative16 _, lblo _),
			(0x1026, "LBNE", relative16 _, lbne _),
			(0x1027, "LBEQ", relative16 _, lbeq _),
			(0x1028, "LBVC", relative16 _, lbvc _),
			(0x1029, "LBVS", relative16 _, lbvs _),
			(0x102A, "LBPL", relative16 _, lbpl _),
			(0x102B, "LBMI", relative16 _, lbmi _),
			(0x102C, "LBGE", relative16 _, lbge _),
			(0x102D, "LBLT", relative16 _, lblt _),
			(0x102E, "LBGT", relative16 _, lbgt _),
			(0x102F, "LBLE", relative16 _, lble _),
			
			(0x103F, "SWI2", inherent _, swi2 _),
			(0x1083, "CMPD", immediate16 _, cmpd _),
			(0x108C, "CMPY", immediate16 _, cmpy _),
			(0x108E, "LDY", immediate16 _, ldy _),
			(0x1093, "CMPD", direct _, cmpd _),
			(0x109C, "CMPY", direct _, cmpy _),
			(0x109E, "LDY", direct _, ldy _),
			(0x109F, "STY", direct _, sty _),
			(0x10A3, "CMPD", indexed _, cmpd _),
			(0x10AC, "CMPY", indexed _, cmpy _),
			(0x10AE, "LDY", indexed _, ldy _),
			(0x10AF, "STY", indexed _, sty _),
			(0x10B3, "CMPD", extended _, cmpd _),
			(0x10BC, "CMPY", extended _, cmpy _),
			(0x10BE, "LDY", extended _, ldy _),
			(0x10BF, "STY", extended _, sty _),
			(0x10CE, "LDS", immediate16 _, lds _),
			(0x10DE, "LDS", direct _, lds _),
			(0x10DF, "STS", direct _, sts _),
			(0x10EE, "LDS", indexed _, lds _),
			(0x10EF, "STS", indexed _, sts _),
			(0x10FE, "LDS", extended _, lds _),
			(0x10FF, "STS", extended _, sts _),
			
			(0x113F, "SWI3", inherent _, swi3 _),
			(0x1183, "CMPU", immediate16 _, cmpu _),
			(0x118C, "CMPS", immediate16 _, cmps _),
			(0x1193, "CMPU", direct _, cmpu _),
			(0x119C, "CMPS", direct _, cmps _),
			(0x11A3, "CMPU", indexed _, cmpu _),
			(0x11AC, "CMPS", indexed _, cmps _),
			(0x11B3, "CMPU", extended _, cmpu _),
			(0x11BC, "CMPS", extended _, cmps _)
		)

		def __unimpl = {
			unimpl
			0
		}
		
		def pushPC = __push16_S(PC)
		def popPC = setPC(__pull16_S)
		
		def __push16_S(w:Int) = {
			addS(-2)
			setMemW(S, w)
		}
		def __push8_S(b:Int) = {
			addS(-1)
			setMemB(S, b)
		}
		def __pull16_S = {
			addS(2)
			memW(S - 2)
		}
		def __pull8_S = {
			addS(1)
			memB(S - 1)
		}
		def __push16_U(w:Int) = {
			addU(-2)
			setMemW(U, w)
		}
		def __push8_U(b:Int) = {
			addU(-1)
			setMemB(U, b)
		}
		def __pull16_U = {
			addU(2)
			memW(U - 2)
		}
		def __pull8_U = {
			addU(1)
			memB(U - 1)
		}
		
		
		def __inc(i:Int) = { 
			val k = i + 1
		  
			// -aaa-
			ccSetN((k & 0x80) != 0)
			ccSetZ((k & 0xFF) == 0)
			ccSetV(k == 0x80)
			
			k
		}
		def __dec(i:Int) = {
			val k = i - 1
			
			// -aaa-
			ccSetN((k & 0x80) != 0)
			ccSetZ((k & 0xFF) == 0)
			ccSetV(k == 0x7F)
			
			k
		}
		def __rol(i:Int) = {
			val k = (i << 1) | (if (ccC) 1 else 0) 
			
			// -aaas
			ccSetN((k & 0x80) != 0)
			ccSetZ((k & 0xFF) == 0)
			ccSetV(false) // TODO
			ccSetC((k & 0x100) != 0)
			
			k
		}
		def __ror(i:Int) = {
			val k = (i >> 1) | (if (ccC) 0x80 else 0)
			
			// -aa-s
			ccSetN((k & 0x80) != 0)
			ccSetZ((k & 0xFF) == 0)
			ccSetC((i & 1) != 0)
			
			k
		}		
		def __lsr(i:Int) = {
			val k = i >> 1

			// -0a-s
			ccSetN(false)
			ccSetZ((k & 0xFF) == 0)
			ccSetC((i & 1) != 0)
			
			k
		}
		def __lsl(i:Int) = __unimpl
		def __asr(i:Int) = __unimpl
		def __asl(i:Int) = __unimpl
		
		def __com(i:Int) = {
			val k = i ^ -1
		  
			// -aa01
			ccSetN((k & 0x80) == 0)
			ccSetZ((k & 0xFF) == 0)
			ccSetV(false)
			ccSetC(true)
			
			k
		}
		def __neg(i:Int) = {
			val k = -i 
			
			// uaaaa
			ccSetN((k & 0x80) == 0)
			ccSetZ((k & 0xFF) == 0)
			ccSetV(k == 0x80)
			ccSetC(k == 0)
			
			k
		}
		def __clr = {
			// -0100
			ccSetN(false)
			ccSetZ(true)
			ccSetV(false)
			ccSetC(false)
			
			0
		}
		def __sub8(i:Int, j:Int) = {			
			val k = i - j
					
			// -aaaa
			ccSetN(k < 0)
			ccSetZ(k == 0)
			ccSetV(i > 0x7F && k < 0x80 || k < -0x80)
			ccSetC(k < 0)
			
			k
		}
		def __sub16(i:Int, j:Int) = {
			val k = i - j
			
			// -aaaa
			ccSetN(k < 0)
			ccSetZ(k == 0)
			ccSetV(i > 0x7FFF && k < 0x8000 || k < -0x8000)
			ccSetC(k < 0)
			
			k
		}
		def __sbc(i:Int, j:Int) = __unimpl
		
		def __cmp8 = __sub8 _
		def __cmp16 = __sub16 _
		
		def __add8(i:Int, j:Int) = {
			val k = i + j
			
			// aaaaa
			// TODO set H (half carry)
			ccSetN((k & 0x80) != 0)
			ccSetZ((k & 0xFF) == 0)
			ccSetV(i < 0x80 && k > 0x7F || k > 0x17F)
			ccSetC(k > 0xFF)
			
			k
		}
		def __add16(i:Int, j:Int) = {
			val k = i + j
			
			// aaaaa
			// TODO set H (half carry)
			ccSetN((k & 0x8000) != 0)
			ccSetZ((k & 0xFFFF) == 0)
			ccSetV(i < 0x8000 && k > 0x7FFF || k > 0x17FFF)
			ccSetC(k > 0xFFFF)
			
			k
		}
		def __adc(i:Int, j:Int) = __unimpl
		def __bit = __and _
		def __ld = __tst _ 
		def __st = __tst _
		def __eor(i:Int, j:Int) = __tst(i ^ j)
		def __or(i:Int, j:Int) = __tst(i | j)
		def __and(i:Int, j:Int) = __tst(i & j)
		def __tst(i:Int) = {
			// -aa0-
			ccSetN(i < 0)
			ccSetZ(i == 0)
			ccSetV(false)
			
			i
		}
		def __bsr = __jsr _
		def __lbsr = __jsr _
		def __jsr(i:Int) = {
			__push16_S(PC)
			setPC(i)
		}
		
		def __orcc(i:Int) = setCC(CC | i)
		def __andcc(i:Int) = setCC(CC & i)
		
		def __lea(ea:Int) = {
		  ccSetZ(ea == 0)
		  ea
		}
		
		
	  
		def lda(ea:Int) = setA(__ld(memB(ea)))
		def ldb(ea:Int) = setB(__ld(memB(ea)))
		def ldx(ea:Int) = setX(__ld(memW(ea))) 
		def ldy(ea:Int) = setY(__ld(memW(ea)))
		
		def sta(ea:Int) = setMemB(ea, __st(A)) 
		def stb(ea:Int) = setMemB(ea, __st(B)) 
		def stx(ea:Int) = setMemW(ea, __st(X))
		def sty(ea:Int) = setMemW(ea, __st(Y))
		
		def neg(ea:Int) = setMemB(ea, __neg(memB(ea)))
		def com(ea:Int) = setMemB(ea, __com(memB(ea)))
		def lsr(ea:Int) = setMemB(ea, __lsr(memB(ea)))
		def ror(ea:Int) = setMemB(ea, __ror(memB(ea)))
		def rol(ea:Int) = setMemB(ea, __rol(memB(ea)))
		def asr(ea:Int) = setMemB(ea, __asr(memB(ea)))
		def lsl(ea:Int) = setMemB(ea, __lsl(memB(ea)))
		def inc(ea:Int) = setMemB(ea, __inc(memB(ea)))
		def dec(ea:Int) = setMemB(ea, __dec(memB(ea)))
		def tst(ea:Int) = __tst(memB(ea))
		def jmp(ea:Int) = setPC(ea)
		def clr(ea:Int) = setMemB(ea, __clr)
		
		def nop(ea:Int) = {}
		def sync(ea:Int) = unimpl
		def lbra(ea:Int) = setPC(ea)
		def lbsr(ea:Int) = __lbsr(memW(ea))
		def daa(ea:Int) = unimpl
		def orcc(ea:Int) = __orcc(memB(ea))
		def andcc(ea:Int) = __andcc(memB(ea))
		def sex(ea:Int) = unimpl
		def exg(ea:Int) = unimpl
		def tfr(ea:Int) = unimpl
	  
		def bra(ea:Int) = setPC(ea)
		def brn(ea:Int) = {}
		def bhi(ea:Int) = if (!(ccC || ccZ)) setPC(ea) 
		def bls(ea:Int) = if (ccC || ccZ) setPC(ea)
		def bhs(ea:Int) = if (!ccC) setPC(ea)
		def blo(ea:Int) = if (ccC) setPC(ea)
		def bne(ea:Int) = if (!ccZ) setPC(ea)
		def beq(ea:Int) = if (ccZ) setPC(ea)
		def bvc(ea:Int) = if (!ccV) setPC(ea)
		def bvs(ea:Int) = if (ccV) setPC(ea)
		def bpl(ea:Int) = if (!ccN) setPC(ea)
		def bmi(ea:Int) = if (ccN) setPC(ea)
		def bge(ea:Int) = if (!(ccN ^ ccV)) setPC(ea)
		def blt(ea:Int) = if (ccN ^ ccV) setPC(ea)
		def bgt(ea:Int) = if (!(ccZ || (ccN ^ ccV))) setPC(ea)
		def ble(ea:Int) = if (ccZ || (ccN ^ ccV)) setPC(ea)
		
		def leax(ea:Int) = setX(__lea(ea))
		def leay(ea:Int) = setY(__lea(ea))
		def leas(ea:Int) = setS(ea)
		def leau(ea:Int) = setU(ea)
				
		def pshs(ea:Int) = {
			val regs = memB(ea)
			
			if ((regs & bit(7)) != 0) __push16_S(PC)
			if ((regs & bit(6)) != 0) __push16_S(U) // <-- S/U
			if ((regs & bit(5)) != 0) __push16_S(Y)
			if ((regs & bit(4)) != 0) __push16_S(X)
			if ((regs & bit(3)) != 0) __push8_S(DP)
			if ((regs & bit(2)) != 0) __push8_S(B)
			if ((regs & bit(1)) != 0) __push8_S(A)
			if ((regs & bit(0)) != 0) __push8_S(CC)
		}
		def puls(ea:Int) = {
			val regs = memB(ea)
			
			if ((regs & bit(0)) != 0) setCC(__pull8_S)
			if ((regs & bit(1)) != 0) setA(__pull8_S)
			if ((regs & bit(2)) != 0) setB(__pull8_S)
			if ((regs & bit(3)) != 0) setDP(__pull8_S)
			if ((regs & bit(4)) != 0) setX(__pull16_S)
			if ((regs & bit(5)) != 0) setY(__pull16_S)
			if ((regs & bit(6)) != 0) setU(__pull16_S) // <-- S/U
			if ((regs & bit(7)) != 0) setPC(__pull16_S)
		}
		def pshu(ea:Int) = {
			val regs = memB(ea)
			
			if ((regs & bit(7)) != 0) __push16_U(PC)
			if ((regs & bit(6)) != 0) __push16_U(S) // <-- S/U
			if ((regs & bit(5)) != 0) __push16_U(Y)
			if ((regs & bit(4)) != 0) __push16_U(X)
			if ((regs & bit(3)) != 0) __push8_U(DP)
			if ((regs & bit(2)) != 0) __push8_U(B)
			if ((regs & bit(1)) != 0) __push8_U(A)
			if ((regs & bit(0)) != 0) __push8_U(CC)
		}
		def pulu(ea:Int) = {
			val regs = memB(ea)
			
			if ((regs & bit(0)) != 0) setCC(__pull8_U)
			if ((regs & bit(1)) != 0) setA(__pull8_U)
			if ((regs & bit(2)) != 0) setB(__pull8_U)
			if ((regs & bit(3)) != 0) setDP(__pull8_U)
			if ((regs & bit(4)) != 0) setX(__pull16_U)
			if ((regs & bit(5)) != 0) setY(__pull16_U)
			if ((regs & bit(6)) != 0) setS(__pull16_U) // <-- S/U
			if ((regs & bit(7)) != 0) setPC(__pull16_U)
		}
		
		def rts(ea:Int) = popPC
		def abx(ea:Int) = unimpl
		def rti(ea:Int) = unimpl
		def cwai(ea:Int) = unimpl
		def mul(ea:Int) = unimpl
		def reset(ea:Int) = unimpl
		def swi(ea:Int) = unimpl
		
		def nega(ea:Int) = setA(__neg(A))
		def coma(ea:Int) = setA(__com(A))
		def lsra(ea:Int) = setA(__lsr(A))
		def rora(ea:Int) = setA(__ror(A))
		def asra(ea:Int) = setA(__asr(A))
		def lsla(ea:Int) = setA(__lsl(A))
		def rola(ea:Int) = setA(__rol(A))
		def deca(ea:Int) = setA(__dec(A))
		def inca(ea:Int) = setA(__inc(A))    
		def tsta(ea:Int) = __tst(A)
		def clra(ea:Int) = setA(__clr)
		
		def negb(ea:Int) = setB(__neg(B))
		def comb(ea:Int) = setB(__com(B))
		def lsrb(ea:Int) = setB(__lsr(B))
		def rorb(ea:Int) = setB(__ror(B))
		def asrb(ea:Int) = setB(__asr(B))
		def lslb(ea:Int) = setB(__lsl(B))
		def rolb(ea:Int) = setB(__rol(B))
		def decb(ea:Int) = setB(__dec(B))
		def incb(ea:Int) = setB(__inc(B))
		def tstb(ea:Int) = __tst(B)
		def clrb(ea:Int) = setB(__clr)
		
		def suba(ea:Int) = setA(__sub8(A, memB(ea)))
		def cmpa(ea:Int) = __cmp8(A, memB(ea))
		def sbca(ea:Int) = setA(__sbc(A, memB(ea)))
		def subd(ea:Int) = setD(__sub16(D, memW(ea)))
		def anda(ea:Int) = setA(__and(A, memB(ea)))
		def bita(ea:Int) = setA(__bit(A, memB(ea)))
		def eora(ea:Int) = setA(__eor(A, memB(ea)))
		def adca(ea:Int) = setA(__adc(A, memB(ea)))
		def ora(ea:Int) = setA(__or(A, memB(ea)))
		def adda(ea:Int) = setA(__add8(A, memB(ea)))
		
		def cmpx(ea:Int) = __cmp16(X, memW(ea))
		def bsr(ea:Int) = __bsr(ea)
		def jsr(ea:Int) = __jsr(ea)
		
		def subb(ea:Int) = setB(__sub8(B, memB(ea)))
		def cmpb(ea:Int) = __cmp8(B, memB(ea))
		def sbcb(ea:Int) = setB(__sbc(B, memB(ea)))
		def addd(ea:Int) = setD(__add16(D, memW(ea)))
		def andb(ea:Int) = setB(__and(B, memB(ea)))
		def bitb(ea:Int) = setB(__bit(B, memB(ea)))
		def eorb(ea:Int) = setB(__eor(B, memB(ea)))
		def adcb(ea:Int) = setB(__adc(B, memB(ea)))
		def orb(ea:Int) = setB(__or(B, memB(ea)))
		def addb(ea:Int) = setB(__add8(B, memB(ea)))
		
		def ldd(ea:Int) = setD(__ld(memW(ea)))
		def ldu(ea:Int) = setU(__ld(memW(ea)))
		def std(ea:Int) = setMemW(ea, __st(D))
		def stu(ea:Int) = setMemW(ea, __st(U))
		
		def lbrn(ea:Int) = brn _
		def lbhi(ea:Int) = bhi _
		def lbls(ea:Int) = bls _
		def lbhs(ea:Int) = bhs _
		def lblo(ea:Int) = blo _
		def lbne(ea:Int) = bne _
		def lbeq(ea:Int) = beq _
		def lbvc(ea:Int) = bvc _
		def lbvs(ea:Int) = bvs _
		def lbpl(ea:Int) = bpl _
		def lbmi(ea:Int) = bmi _
		def lbge(ea:Int) = bge _
		def lblt(ea:Int) = blt _
		def lbgt(ea:Int) = bgt _
		def lble(ea:Int) = ble _
		
		def swi2(ea:Int) = unimpl  
		def cmpd(ea:Int) = __cmp16(D, memW(ea))
		def cmpy(ea:Int) = __cmp16(Y, memW(ea))
		def lds(ea:Int) = setS(__ld(memW(ea)))
		def sts(ea:Int) = setMemW(ea, __st(S))
		def swi3(ea:Int) = unimpl
		def cmpu(ea:Int) = __cmp16(U, memW(ea))
		def cmps(ea:Int) = __cmp16(S, memW(ea))
	}
	
 
	object AddressingModes {
		import Cpu._
		import InstructionSet._
		import M6809._
		
		def illegal(opcode:Int, pos:Int) {
			printOp(pos, opcode, "unrecognized opcode: " + hexByte(opcode))
			System.exit(0)
		}
		
		def direct(opcode:Int, pos:Int) {
			addPC(2)
			printOp(pos, opcode, "<" + hexByte(memB(pos + 1)))
			val ea = DP << 8 | memB(pos + 1)
			operation(opcode)(ea)
		}
		def immediate8(opcode:Int, pos:Int) = immediate(opcode, pos, 1, memB)
		def immediate16(opcode:Int, pos:Int) = immediate(opcode, pos, 2, memW)
		def immediate(opcode:Int, pos:Int, width:Int, accessor:Int => Int) {
			addPC(if (opcode > 0xFF) 2 else 1)
			addPC(width)
			val ea = PC - width
			printOp(pos, opcode, "#" + hex(2 * width, accessor(ea)))
			operation(opcode)(ea)
		}
		def extended(opcode:Int, pos:Int) {
			addPC(3)
			val ea = memW(PC - 2)
			printOp(pos, opcode, hexWord(ea))
			operation(opcode)(ea)
		}  
		def relative8(opcode:Int, pos:Int) = relative(opcode, pos, 1, memSB)
		def relative16(opcode:Int, pos:Int) = relative(opcode, pos, 2, memSW)
		def relative(opcode:Int, pos:Int, width:Int, accessor:Int => Int) {
			addPC(if (opcode > 0xFF) 2 else 1)
			addPC(width)
			val ea = PC + accessor(pos + 1)
			printOp(pos, opcode, hexWord(ea))
			operation(opcode)(ea)
		}
		def inherent(opcode:Int, pos:Int) {
			addPC(1)
			printOp(pos, opcode, "")
			val ea = 0
			operation(opcode)(ea)
		}
		def indexed(opcode:Int, pos:Int) {
		    import INDEX_MODE._
		    
			addPC(2)
		
			val postByte = memB(pos + 1)
			val registerCode = maskLS(2, postByte >> 5)
		
			val register = registerCode match {
				case INDEX_REG.X => "X"
				case INDEX_REG.Y => "Y"
				case INDEX_REG.U => "U"
				case INDEX_REG.S => "S"
			}
		
			val mode = maskLS(4, postByte)
			val indirect = (postByte & bit(5)) != 0

			// determine effective address
			val ea = 	  
				if (mode == POST_INC_8 || mode == POST_INC_16) 
				  registerCode match {
					case INDEX_REG.X => X
					case INDEX_REG.Y => Y 
					case INDEX_REG.U => U
					case INDEX_REG.S => S
				} else if (mode == PC_RELATIVE_16) {
					addPC(2)
					PC + memSW(if (indirect) memW(PC - 2) else (PC - 2))
				} else if (mode == EXTENDED_INDIRECT) {
					if (indirect) unimpl
					addPC(2)
					memW(memW(PC - 2)) // TODO indirect extended asm out form
				} else {
					print("unimpl addressing mode: " + mode);
					0
				}
			
			val operands = mode match {
				case POST_INC_8 => register formatted ",%s+"
				case POST_INC_16 => register formatted ",%s++"
				case PC_RELATIVE_16 => (hexWord(memW(PC - 2))) formatted (if (indirect) "[%s,PC]" else "%s,PC")
				case EXTENDED_INDIRECT => "["+hexWord(ea)+"] (EXTENDED INDIRECT)"
			}
		
			printOp(pos, opcode, operands)
		
			// exec
			operation(opcode)(ea)
		
			// auto increment/decrement
			mode match {
				case POST_INC_8 => addR(registerCode)(1)	
				case POST_INC_16 => addR(registerCode)(2)
				case _ => {}
			}	
		}  
		
		def addR(registerCode:Int) = registerCode match {
			case INDEX_REG.X => addX _
			case INDEX_REG.Y => addY _
			case INDEX_REG.U => addU _
			case INDEX_REG.S => addS _
		}

	  
		def printOp(pos:Int, opcode:Int, operands:String = "") = 
			printf("%04X %-8s %-5s %-9s ", pos, (hexMemSection(pos, PC)), mnemonic(opcode), operands)
		
	}
}