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
import javax.sound.sampled.AudioSystem
import IO._


object RunMe extends SimpleSwingApplication {
	val DBG_STOP_PERIODICALLY = true
	val DBG_BREAK_ON_INSTRUCTION = ""//"LBSR"//"PSHS"//
	val DBG_EXEC_LIMIT = -1//10950
	val DBG_STEP_COUNT = -1//40//-1
	val DBG_BREAKPOINT_ADDR = -1//0x32D8
	val DBG_WAIT_AT_KBDIN = false
	val DBG_SLEEP_FOR_PAUSE = false
	val DBG_BREAK_ON_DISPLAY_CHANGE = false	
	val DBG_LOG_VERBOSE = true
	val DBG_LOG_DISASM = DBG_LOG_VERBOSE | false
	val DBG_LOG_JUMP_MARKER = DBG_LOG_VERBOSE | false
	val DBG_LOG_REG_BLOCK = DBG_LOG_VERBOSE | false
	val DBG_LOG_MEM_ANALYSIS = DBG_LOG_VERBOSE | false
	val DBG_LOG_IO_SPECIAL = false
	val DBG_LOG_CAS_BLOCKS = false
	val DBG_LOG_CAS_INFO = false
	val DBG_LOG_DISPLAY_CHANGES = false
	val DBG_LOG_TIME_CONSUMPTION = true
	val DBG_LOG_MIPS = true
	val DBG_LOG_CYCLES = true
	val DBG_MEM_CHECKED = false

	val DBG_LOG_NEWLINES = DBG_LOG_DISASM | DBG_LOG_REG_BLOCK | DBG_LOG_MEM_ANALYSIS | DBG_LOG_IO_SPECIAL

	val ADDRESS_SPACE = 0x10000
	val RAM_END = 0x8000
	val ROM_BASIC_BEGIN = RAM_END
	val ROM_BASIC_END = 0xC000
	val BASEDIR = "/home/kjetil/Ubuntu One/dragon32"
	val ROM_BASIC_BINARY = BASEDIR + "/rom/BASIC.ROM"
	val AUTOLOAD_CAS_BINARY = BASEDIR + "/donkeyking/donkey1.cas"
	val CAS_LOAD_BLOCKS_1 = 8
	val CAS_LOAD_BLOCKS_2 = 200
	
	val TXT_START = 0x0400
	val TXT_SIZE = 0x0200
	
	val VPU_FREQ = 1000000.
	val VPU_CYCLE_PERIOD = 1. / VPU_FREQ
	
	val SYNC_CHUNK_FREQ = 50.
	val SYNC_CHUNK_PERIOD = 1. / SYNC_CHUNK_FREQ
	val SYNC_CHUNK_CYCLES = SYNC_CHUNK_PERIOD / VPU_CYCLE_PERIOD
	
	print("SYNC_CHUNK_CYCLES="+SYNC_CHUNK_CYCLES)
  
	val mode:Array[Int] = new Array(2)
	mode(0) = 0
	mode(1) = 0x400
	val __ram:Array[Byte] = new Array(ADDRESS_SPACE)
	def top = new Display(__ram, TXT_START, BASEDIR, mode)

	var terminate = false
	var analysis = ""
	var jumped = false

	val divide = "\n----------------------------------------------------------------------------------------"
	def printDivide = print(divide)
	def printLegend = print("\nADDR M/C        MNEM. OPERANDS     A#B# X### Y### U### S### DP PC## CC######   @S+0 @S+2 INTERCEPTION") 
		
	actor {
	  	initRomBasic
	  	loadCas(CAS_LOAD_BLOCKS_1)
  		Cpu.setPC(execAddress)
	  	
	  	var limit = 0
	  	var stepCounter = DBG_STEP_COUNT
	  	var instructions = 0
	  	var t0Ins = System.nanoTime
	  	var cycles = 0

	  	if (DBG_LOG_REG_BLOCK) {
	 		printDivide
	 		printLegend
	 		printDivide
	 		println
	  	}

	  	while (limit != DBG_EXEC_LIMIT && !terminate) {    
	  		limit += 1
	  		
	  		// FETCH
	  		val pos = Cpu.PC
	  		val b0 = memB(pos)
			val opcode = if (b0 == 0x10 || b0 == 0x11) {
			    Cpu.addPC(1)
			    b0 << 8 | memB(Cpu.PC)
			} else {			    
			    b0
			}
	  		Cpu.addPC(1)
    
			// EXECUTE
			InstructionSet.mode(opcode)(opcode, pos)
			checkExecOverride
			
			if (DBG_LOG_REG_BLOCK) print(Cpu.dump)
			if (DBG_LOG_MEM_ANALYSIS) {
			    print(analysis)
			    analysis = ""
			}

	  		if (jumped) {
	  		    if (DBG_LOG_JUMP_MARKER) print(divide)
	  			jumped = false
	  		}

	  		cycles += InstructionSet.baseCycles(opcode)

			stepCounter -= 1
			if (stepCounter == 0 || Cpu.PC == DBG_BREAKPOINT_ADDR || InstructionSet.mnemonic(opcode) == DBG_BREAK_ON_INSTRUCTION) {
			    println("\n\n----BREAK----")
				System.in.read()
				stepCounter = DBG_STEP_COUNT
			} else {
			    if (DBG_LOG_NEWLINES) println
			}

	  		instructions += 1
	  		val tIns = System.nanoTime
	  		if (cycles > SYNC_CHUNK_CYCLES) {
	  		    cycles = 0
	  		    instructions = 0
	  		    val elapsed = tIns - t0Ins
	  		    t0Ins = tIns
	  		    val remainingNs = SYNC_CHUNK_PERIOD * 1000000000. - elapsed
	  		    val remainingMs = ((remainingNs / 1000000.) toLong)
	  		    if (remainingMs > 0) Thread.sleep(remainingMs)
	  		}
	  		
			if (terminate) println("termination op: " + InstructionSet.mnemonic(opcode))
	  	}
  
	  	if (limit == DBG_EXEC_LIMIT) println("execution limit reached")
  
	    println("Press enter to exit.")
	    System.in.read
		top.closeOperation
	}

	def __executeNormally(i:Int) = {}
	def __return(i:Int) = {
		Cpu.setPC(memW(Cpu.S))
		Cpu.addS(2)			  
	}
	def __return2(i:Int) = {
		Cpu.setPC(memW(Cpu.S + 2))
		Cpu.addS(4)  
	}
	def __kbdIn(i:Int) = {
	    if (DBG_WAIT_AT_KBDIN) System.in.read()
	    
	    if (memW(Cpu.S) == 0x2C14 || memW(Cpu.S) == 0x2C33) {
	        Cpu.setA(0x4E)
	    } else if (memW(Cpu.S+2) == 0x34D1) {
	        Cpu.ccSetZ(true)
	    } else if (memW(Cpu.S+2) == 0x33AC) {
	        Cpu.setA(0x31)
	        Cpu.ccSetZ(false)
	    } else if (memW(Cpu.S+2) == 0x33DF) {
	        Cpu.setA(0x6E)
	        Cpu.ccSetZ(false)
	    } else {
	        printf("Unregistered kbdIn-invocation: [S+2]=%04X\n", memW(Cpu.S+2))
	        System.exit(0)
	    }
		    
		__return(0)
	}
	def __readBinaryFileFromTape(i:Int) = {
	    loadCas(CAS_LOAD_BLOCKS_2)
	    Cpu.setPC(execAddress)
	}
	val ADDR_CURRENT_VDU_CURSOR_ADDR = 0x0088
	def __outChar(i:Int) = {
	    val cursorAddr = memW(ADDR_CURRENT_VDU_CURSOR_ADDR)
	    
	    if (Cpu.A == 13) {
	        setMemW(ADDR_CURRENT_VDU_CURSOR_ADDR, cursorAddr - (cursorAddr % 0x20) + 0x20)
	    } else if (Cpu.A >= 32 && Cpu.A < 64) {
            setMemB(cursorAddr, Cpu.A + 64)
            setMemW(ADDR_CURRENT_VDU_CURSOR_ADDR, cursorAddr + 1)
	    } else if (Cpu.A >= 97 && Cpu.A < 128) {
            setMemB(cursorAddr, Cpu.A - 96)
            setMemW(ADDR_CURRENT_VDU_CURSOR_ADDR, cursorAddr + 1)
	    } else {
            setMemB(cursorAddr, Cpu.A)
            setMemW(ADDR_CURRENT_VDU_CURSOR_ADDR, cursorAddr + 1)
	    }
	    	    
	    __return(0)
	}
	def __initVdg110(i:Int) {
	    mode(0) = 1
	    if (DBG_LOG_DISPLAY_CHANGES) println("SET MODE 128x192 4 COLOR GFX")
	    if (DBG_BREAK_ON_DISPLAY_CHANGE) System.in.read
	    __return(0)
	}
	def __initVdg000(i:Int) {
	    mode(0) = 0
	    if (DBG_LOG_DISPLAY_CHANGES) println("SET MODE 32x16 ALPHANUMERIC")
	    if (DBG_BREAK_ON_DISPLAY_CHANGE) System.in.read
	    __return(0)
	}
	def __setDisplayOffset0001(i:Int) {
	    mode(1) = 0x1 * 0x200
	    if (DBG_LOG_DISPLAY_CHANGES) println("SET DISPLAY OFFSET: "+hexWord(mode(1)))
	    if (DBG_BREAK_ON_DISPLAY_CHANGE) System.in.read
	    __return(0)
	}
	def __setDisplayOffset1101(i:Int) {
	    mode(1) = 0xD * 0x200
	    if (DBG_LOG_DISPLAY_CHANGES) println("SET DISPLAY OFFSET: "+hexWord(mode(1)))
	    if (DBG_BREAK_ON_DISPLAY_CHANGE) System.in.read
	    __return(0)
	}
	def __pause(i:Int) {
	    if (DBG_SLEEP_FOR_PAUSE) Thread.sleep(1000)
	    __return(0)
	}
	
	val EXECUTION_OVERRIDE_TABLE = Array(
	    (0x33F3, "SET SAM => 1000", __setDisplayOffset0001 _),
	    (0x3400, "SET SAM => 1011", __setDisplayOffset1101 _),
	    (0x340D, "INIT VDG => 011, invoke PIA1 B data", __initVdg110 _),
	    (0x341C, "INIT VDG => 000, invoke PIA1 B data", __initVdg000 _),
	    (0x3464, "PAUSE (1sec)", __pause _),
	    /*(0x43E9, "PLAY MELODY 'HOW FAR UP CAN YOU CLIMB'", __return _),*/
		(0xBBE5, "Keyboard input", __kbdIn _),
		(0xB87E, "Find File: searches tape for matching filename (alt 0xB8B3)", __return _),
		(0xBAC3, "Audio off: Disables sound", __return _),
		(0xBD52, "%JOYIN% Reads Joysticks updates $015a-015d, JMPd to from $8012", __return _),
		(0xBAC5, "Enables sound", __return _),
		(0xBDE7, "Cassette on for reading: disable IRQ, FIRQ calls cassette on and performs bit sync", __return _),
		(0xB93E, 
		    "A006 (B93E) Cassette block in from tape:tape should be up to speed and in bit sync. Set up through"+
			"$7C,7D,7E:7F. On exit CC.Z is zero if I/O error occured and $81 holds the source of error. " +
			"IRQ and FIRQ remain disabled.", __executeNormally _),
		(0xBA77, "CLS text screen", __executeNormally _),
		(0xB748, "Read Binary file from tape (CoCo $a511)", __readBinaryFileFromTape _),
		(0xB54A, "%OUTCHR% Sends char in A to DEVN", __outChar _)
	)
	
	def checkExecOverride = {
		val xo = getExecOverride(Cpu.PC)
		
		if (xo._1 > 0) {
		    if (DBG_LOG_MEM_ANALYSIS) analysis += " X/O @ " + hexWord(xo._1) + " \"" + xo._2 + "\""
			xo._3(0)
		}
	}

	val MAX_EXEC_OVERRIDE = 0xC000
	
	val __execOverrideByAddressIndex = new Array[(Int, String, (Int) => Unit)](MAX_EXEC_OVERRIDE)
    for (i <- 0 until MAX_EXEC_OVERRIDE) __execOverrideByAddressIndex(i) = __findExecOverride(i)
	def __findExecOverride(addr:Int) = EXECUTION_OVERRIDE_TABLE.find(_._1 == addr).getOrElse(-1, "unrestricted", __executeNormally _)
	def getExecOverride(addr:Int) = __execOverrideByAddressIndex(addr)
  
	def maskLo(n:Int) = (1 << n) - 1
	def maskLS(n:Int, v:Int) = v & ((1 << n) - 1)
	def maskLS8(v:Int) = v & 0xFF
	def maskLS16(v:Int) = v & 0xFFFF
	
	def complement(v:Int) = -1 ^ v
	def bit(n:Int) = 1 << n
	def nbit(n:Int) = complement(bit(n))
	
	def setBit(num:Int, value:Int) = value | bit(num)
	def resetBit(num:Int, value:Int) = value & nbit(num)
	def alterBit(set:Boolean) = if (set) setBit _ else resetBit _ 
	
	def utos5(v:Int) = if (v < 0x10) v else (v - 0x20)
	def utos8(v:Int) = if (v < 0x80) v else (v - 0x100)
	def utos16(v:Int) = if (v < 0x8000) v else (v - 0x10000)	
	
	def readLeader: Boolean = readByte match {
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
	
	def blockTypeString(t: Int) = t match {
		case BT_NAMEFILE => "NAMEFILE"
		case BT_DATA => "DATA"
		case BT_EOF => "EOF"
	}
	def fileIdString(b: Int) = b match {
		case 0x00 => "BASIC program"
		case 0x01 => "Data file"
		case 0x02 => "Binary file"
	}
	def asciiFlagString(b: Int) = b match {
		case 0x00 => "Binary"
		case 0xFF => "ASCII"
	}
	def gapFlagString(b: Int) = b match {
		case 0x00 => "Continuous (binary/BASIC files)"
		case 0xFF => "Stopping (data files)"
  	}
  	
	def readAddress = hexWord(readWord)
	def readChecksum = hexByte(readByte)
	def readTrailer = hexByte(readByte)  
	def readByte = cas.read
	def readWord = readByte << 8 | readByte
  
	def hex(nibbles:Int, v:Int) = maskLS(4 * nibbles, v) formatted (nibbles formatted "%%0%dX")
	def hexByte(b: Int) = maskLS8(b) formatted "%02X"
	def hexWord(w: Int) = maskLS16(w) formatted "%04X"  
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

	val BT_NAMEFILE = 0x00
	val BT_DATA = 0x01
	val BT_EOF = 0xFF
	
	def loadCas(numBlocks:Int) {
		val f = new CasLoader
		import f._
		
		var blocksLeft = numBlocks
		
		while (blocksLeft > 0 && readLeader)
			block {
				blocksLeft -= 1
				
				val blkType = readByte
				val blkLen = readByte

				if (DBG_LOG_CAS_BLOCKS)
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
  
		//cas.close();
		if (DBG_LOG_CAS_INFO) println("LOAD END ADDR: " + hexWord(loadAddress))
	}
	
	
	
}