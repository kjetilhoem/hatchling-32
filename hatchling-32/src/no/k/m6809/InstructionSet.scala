package no.k.m6809

import RunMe._
import Cpu._
import AddressingModes._
import IO._

object InstructionSet {
	def unimpl = terminate = true

	def operation(opcode:Int) = get(opcode)._5
	def mode(opcode:Int) = get(opcode)._4
	def baseCycles(opcode:Int) = get(opcode)._2
	def mnemonic(opcode:Int) = get(opcode)._3

	def illegalop(ea:Int) = unimpl
	
	val INSTRUCTION_TABLE = Array(
		(0x00, 6, "NEG", direct _, neg _),
		(0x03, 6, "COM", direct _, com _),
		(0x04, 6, "LSR", direct _, lsr _),
		(0x06, 6, "ROR", direct _, ror _),
		(0x07, 6, "ASR", direct _, asr _),
		(0x08, 6, "LSL", direct _, lsl _),
		(0x09, 6, "ROL", direct _, rol _),
		(0x0A, 6, "DEC", direct _, dec _),
		(0x0C, 6, "INC", direct _, inc _),
		(0x0D, 6, "TST", direct _, tst _),
		(0x0E, 3, "JMP", direct _, jmp _),
		(0x0F, 6, "CLR", direct _, clr _),
		(0x12, 2, "NOP", inherent _, nop _),
		(0x13, 2, "SYNC", inherent _, sync _),
		(0x16, 5, "LBRA", relative16 _, lbra _),
		(0x17, 9, "LBSR", relative16 _, lbsr _),
		(0x19, 2, "DAA", inherent _, daa _),
		(0x1A, 3, "ORCC", immediate8 _, orcc _),
		(0x1C, 3, "ANDCC", immediate8 _, andcc _),
		(0x1D, 2, "SEX", inherent _, sex _),
		(0x1E, 8, "EXG", inherent _, exg _),
		(0x1F, 7, "TFR", inherent _, tfr _),
		(0x20, 3, "BRA", relative8 _, bra _),
		(0x21, 3, "BRN", relative8 _, brn _),
		(0x22, 3, "BHI", relative8 _, bhi _),
		(0x23, 3, "BLS", relative8 _, bls _),
		(0x24, 3, "BHS", relative8 _, bhs _),
		(0x25, 3, "BLO", relative8 _, blo _),
		(0x26, 3, "BNE", relative8 _, bne _),
		(0x27, 3, "BEQ", relative8 _, beq _),
		(0x28, 3, "BVC", relative8 _, bvc _),
		(0x29, 3, "BVS", relative8 _, bvs _),
		(0x2A, 3, "BPL", relative8 _, bpl _),
		(0x2B, 3, "BMI", relative8 _, bmi _),
		(0x2C, 3, "BGE", relative8 _, bge _),
		(0x2D, 3, "BLT", relative8 _, blt _),
		(0x2E, 3, "BGT", relative8 _, bgt _),
		(0x2F, 3, "BLE", relative8 _, ble _),
		(0x30, 4, "LEAX", indexed _, leax _),
		(0x31, 4, "LEAY", indexed _, leay _),
		(0x32, 4, "LEAS", indexed _, leas _),
		(0x33, 4, "LEAU", indexed _, leau _),
		(0x34, 5, "PSHS", inherent _, pshs _),
		(0x35, 5, "PULS", inherent _, puls _),
		(0x36, 5, "PSHU", inherent _, pshu _),
		(0x37, 5, "PULU", inherent _, pulu _),
		(0x39, 5, "RTS", inherent _, rts _),
		(0x3A, 3, "ABX", inherent _, abx _),
		(0x3B, 6, "RTI", inherent _, rti _),
		(0x3C, 21, "CWAI", inherent _, cwai _),
		(0x3D, 11, "MUL", inherent _, mul _),
		(0x3E, 0, "RESET", inherent _, reset _),
		(0x3F, 19, "SWI", inherent _, swi _),
		(0x40, 2, "NEGA", inherent _, nega _),
		(0x43, 2, "COMA", inherent _, coma _),
		(0x44, 2, "LSRA", inherent _, lsra _),
		(0x46, 2, "RORA", inherent _, rora _),
		(0x47, 2, "ASRA", inherent _, asra _),
		(0x48, 2, "LSLA", inherent _, lsla _),
		(0x49, 2, "ROLA", inherent _, rola _),
		(0x4A, 2, "DECA", inherent _, deca _),
		(0x4C, 2, "INCA", inherent _, inca _),
		(0x4D, 2, "TSTA", inherent _, tsta _),
		(0x4F, 2, "CLRA", inherent _, clra _),
		(0x50, 2, "NEGB", inherent _, negb _),
		(0x53, 2, "COMB", inherent _, comb _),
		(0x54, 2, "LSRB", inherent _, lsrb _),
		(0x56, 2, "RORB", inherent _, rorb _),
		(0x57, 2, "ASRB", inherent _, asrb _),
		(0x58, 2, "LSLB", inherent _, lslb _),
		(0x59, 2, "ROLB", inherent _, rolb _),
		(0x5A, 2, "DECB", inherent _, decb _),
		(0x5C, 2, "INCB", inherent _, incb _),
		(0x5D, 2, "TSTB", inherent _, tstb _),
		(0x5F, 2, "CLRB", inherent _, clrb _),
		(0x60, 6, "NEG", indexed _, neg _),
		(0x63, 6, "COM", indexed _, com _),
		(0x64, 6, "LSR", indexed _, lsr _),
		(0x66, 6, "ROR", indexed _, ror _),
		(0x67, 6, "ASR", indexed _, asr _),
		(0x68, 6, "LSL", indexed _, lsl _),
		(0x69, 6, "ROL", indexed _, rol _),
		(0x6A, 6, "DEC", indexed _, dec _),
		(0x6C, 6, "INC", indexed _, inc _),
		(0x6D, 6, "TST", indexed _, tst _),
		(0x6E, 3, "JMP", indexed _, jmp _),
		(0x6F, 6, "CLR", indexed _, clr _),
		(0x70, 7, "NEG", extended _, neg _),
		(0x73, 7, "COM", extended _, com _),
		(0x74, 7, "LSR", extended _, lsr _),
		(0x76, 7, "ROR", extended _, ror _),
		(0x77, 7, "ASR", extended _, asr _),
		(0x78, 7, "LSL", extended _, lsl _),
		(0x79, 7, "ROL", extended _, rol _),
		(0x7A, 7, "DEC", extended _, dec _),
		(0x7C, 7, "INC", extended _, inc _),
		(0x7D, 7, "TST", extended _, tst _),
		(0x7E, 3, "JMP", extended _, jmp _),
		(0x7F, 7, "CLR", extended _, clr _),
		(0x80, 2, "SUBA", immediate8 _, suba _),
		(0x81, 2, "CMPA", immediate8 _, cmpa _),
		(0x82, 2, "SBCA", immediate8 _, sbca _),
		(0x83, 4, "SUBD", immediate16 _, subd _),
		(0x84, 2, "ANDA", immediate8 _, anda _),
		(0x85, 2, "BITA", immediate8 _, bita _),
		(0x86, 2, "LDA", immediate8 _, lda _),
		(0x88, 2, "EORA", immediate8 _, eora _),
		(0x89, 2, "ADCA", immediate8 _, adca _),
		(0x8A, 2, "ORA", immediate8 _, ora _),
		(0x8B, 2, "ADDA", immediate8 _, adda _),
		(0x8C, 4, "CMPX", immediate16 _, cmpx _),
		(0x8D, 7, "BSR", relative8 _, bsr _),
		(0x8E, 3, "LDX", immediate16 _, ldx _),
		(0x90, 4, "SUBA", direct _, suba _),
		(0x91, 4, "CMPA", direct _, cmpa _),
		(0x92, 4, "SBCA", direct _, sbca _),
		(0x93, 6, "SUBD", direct _, subd _),
		(0x94, 4, "ANDA", direct _, anda _),
		(0x95, 4, "BITA", direct _, bita _),
		(0x96, 4, "LDA", direct _, lda _),
		(0x97, 4, "STA", direct _, sta _),
		(0x98, 4, "EORA", direct _, eora _),
		(0x99, 4, "ADCA", direct _, adca _),
		(0x9A, 4, "ORA", direct _, ora _),
		(0x9B, 4, "ADDA", direct _, adda _),
		(0x9C, 6, "CMPX", direct _, cmpx _),
		(0x9D, 7, "JSR", direct _, jsr _),
		(0x9E, 5, "LDX", direct _, ldx _),
		(0x9F, 5, "STX", direct _, stx _),
		(0xA0, 4, "SUBA", indexed _, suba _),
		(0xA1, 4, "CMPA", indexed _, cmpa _),
		(0xA2, 4, "SBCA", indexed _, sbca _),
		(0xA3, 6, "SUBD", indexed _, subd _),
		(0xA4, 4, "ANDA", indexed _, anda _),
		(0xA5, 4, "BITA", indexed _, bita _),
		(0xA6, 4, "LDA", indexed _, lda _),
		(0xA7, 4, "STA", indexed _, sta _),
		(0xA8, 4, "EORA", indexed _, eora _),
		(0xA9, 4, "ADCA", indexed _, adca _),
		(0xAA, 4, "ORA", indexed _, ora _),
		(0xAB, 4, "ADDA", indexed _, adda _),
		(0xAC, 6, "CMPX", indexed _, cmpx _),
		(0xAD, 7, "JSR", indexed _, jsr _),
		(0xAE, 5, "LDX", indexed _, ldx _),
		(0xAF, 5, "STX", indexed _, stx _),
		(0xB0, 5, "SUBA", extended _, suba _),
		(0xB1, 5, "CMPA", extended _, cmpa _),
		(0xB2, 5, "SBCA", extended _, sbca _),
		(0xB3, 7, "SUBD", extended _, subd _),
		(0xB4, 5, "ANDA", extended _, anda _),
		(0xB5, 5, "BITA", extended _, bita _),
		(0xB6, 5, "LDA", extended _, lda _),
		(0xB7, 5, "STA", extended _, sta _),
		(0xB8, 5, "EORA", extended _, eora _),
		(0xB9, 5, "ADCA", extended _, adca _),
		(0xBA, 5, "ORA", extended _, ora _),
		(0xBB, 5, "ADDA", extended _, adda _),
		(0xBC, 7, "CMPX", extended _, cmpx _),
		(0xBD, 8, "JSR", extended _, jsr _),
		(0xBE, 6, "LDX", extended _, ldx _),
		(0xBF, 6, "STX", extended _, stx _),
		(0xC0, 2, "SUBB", immediate8 _, subb _),
		(0xC1, 2, "CMPB", immediate8 _, cmpb _),
		(0xC2, 2, "SBCB", immediate8 _, sbcb _),
		(0xC3, 4, "ADDD", immediate8 _, addd _),
		(0xC4, 2, "ANDB", immediate8 _, andb _),
		(0xC5, 2, "BITB", immediate8 _, bitb _),
		(0xC6, 2, "LDB", immediate8 _, ldb _),
		(0xC8, 2, "EORB", immediate8 _, eorb _),
		(0xC9, 2, "ADCB", immediate8 _, adcb _),
		(0xCA, 2, "ORB", immediate8 _, orb _),
		(0xCB, 2, "ADDB", immediate8 _, addb _),
		(0xCC, 3, "LDD", immediate16 _, ldd _),
		(0xCE, 3, "LDU", immediate16 _, ldu _),
		(0xD0, 4, "SUBB", direct _, subb _),
		(0xD1, 4, "CMPB", direct _, cmpb _),
		(0xD2, 4, "SBCB", direct _, sbcb _),
		(0xD3, 6, "ADDD", direct _, addd _),
		(0xD4, 4, "ANDB", direct _, andb _),
		(0xD5, 4, "BITB", direct _, bitb _),
		(0xD6, 4, "LDB", direct _, ldb _),
		(0xD7, 4, "STB", direct _, stb _),
		(0xD8, 4, "EORB", direct _, eorb _),
		(0xD9, 4, "ADCB", direct _, adcb _),
		(0xDA, 4, "ORB", direct _, orb _),
		(0xDB, 4, "ADDB", direct _, addb _),
		(0xDC, 5, "LDD", direct _, ldd _),
		(0xDD, 5, "STD", direct _, std _),
		(0xDE, 5, "LDU", direct _, ldu _),
		(0xDF, 5, "STU", direct _, stu _),
		(0xE0, 4, "SUBB", indexed _, subb _),
		(0xE1, 4, "CMPB", indexed _, cmpb _),
		(0xE2, 4, "SBCB", indexed _, sbcb _),
		(0xE3, 6, "ADDD", indexed _, addd _),
		(0xE4, 4, "ANDB", indexed _, andb _),
		(0xE5, 4, "BITB", indexed _, bitb _),
		(0xE6, 4, "LDB", indexed _, ldb _),
		(0xE7, 4, "STB", indexed _, stb _),
		(0xE8, 4, "EORB", indexed _, eorb _),
		(0xE9, 4, "ADCB", indexed _, adcb _),
		(0xEA, 4, "ORB", indexed _, orb _),
		(0xEB, 4, "ADDB", indexed _, addb _),
		(0xEC, 5, "LDD", indexed _, ldd _),
		(0xED, 5, "STD", indexed _, std _),
		(0xEE, 5, "LDU", indexed _, ldu _),
		(0xEF, 5, "STU", indexed _, stu _),
		(0xF0, 5, "SUBB", extended _, subb _),
		(0xF1, 5, "CMPB", extended _, cmpb _),
		(0xF2, 5, "SBCB", extended _, sbcb _),
		(0xF3, 7, "ADDD", extended _, addd _),
		(0xF4, 5, "ANDB", extended _, andb _),
		(0xF5, 5, "BITB", extended _, bitb _),
		(0xF6, 5, "LDB", extended _, ldb _),
		(0xF7, 5, "STB", extended _, stb _),
		(0xF8, 5, "EORB", extended _, eorb _),
		(0xF9, 5, "ADCB", extended _, adcb _),
		(0xFA, 5, "ORB", extended _, orb _),
		(0xFB, 5, "ADDB", extended _, addb _),
		(0xFC, 6, "LDD", extended _, ldd _),
		(0xFD, 6, "STD", extended _, std _),
		(0xFE, 6, "LDU", extended _, ldu _),
		(0xFF, 6, "STU", extended _, stu _),
		
		(0x1021, 5, "LBRN", relative16 _, lbrn _),
		(0x1022, 5, "LBHI", relative16 _, lbhi _),
		(0x1023, 5, "LBLS", relative16 _, lbls _),
		(0x1024, 5, "LBHS", relative16 _, lbhs _),
		(0x1025, 5, "LBLO", relative16 _, lblo _),
		(0x1026, 5, "LBNE", relative16 _, lbne _),
		(0x1027, 5, "LBEQ", relative16 _, lbeq _),
		(0x1028, 5, "LBVC", relative16 _, lbvc _),
		(0x1029, 5, "LBVS", relative16 _, lbvs _),
		(0x102A, 5, "LBPL", relative16 _, lbpl _),
		(0x102B, 5, "LBMI", relative16 _, lbmi _),
		(0x102C, 5, "LBGE", relative16 _, lbge _),
		(0x102D, 5, "LBLT", relative16 _, lblt _),
		(0x102E, 5, "LBGT", relative16 _, lbgt _),
		(0x102F, 5, "LBLE", relative16 _, lble _),
		
		(0x103F, 20, "SWI2", inherent _, swi2 _),
		(0x1083, 5, "CMPD", immediate16 _, cmpd _),
		(0x108C, 5, "CMPY", immediate16 _, cmpy _),
		(0x108E, 4, "LDY", immediate16 _, ldy _),
		(0x1093, 7, "CMPD", direct _, cmpd _),
		(0x109C, 7, "CMPY", direct _, cmpy _),
		(0x109E, 6, "LDY", direct _, ldy _),
		(0x109F, 6, "STY", direct _, sty _),
		(0x10A3, 7, "CMPD", indexed _, cmpd _),
		(0x10AC, 7, "CMPY", indexed _, cmpy _),
		(0x10AE, 6, "LDY", indexed _, ldy _),
		(0x10AF, 6, "STY", indexed _, sty _),
		(0x10B3, 8, "CMPD", extended _, cmpd _),
		(0x10BC, 8, "CMPY", extended _, cmpy _),
		(0x10BE, 7, "LDY", extended _, ldy _),
		(0x10BF, 7, "STY", extended _, sty _),
		(0x10CE, 4, "LDS", immediate16 _, lds _),
		(0x10DE, 6, "LDS", direct _, lds _),
		(0x10DF, 6, "STS", direct _, sts _),
		(0x10EE, 6, "LDS", indexed _, lds _),
		(0x10EF, 6, "STS", indexed _, sts _),
		(0x10FE, 7, "LDS", extended _, lds _),
		(0x10FF, 7, "STS", extended _, sts _),
		
		(0x113F, 20, "SWI3", inherent _, swi3 _),
		(0x1183, 5, "CMPU", immediate16 _, cmpu _),
		(0x118C, 5, "CMPS", immediate16 _, cmps _),
		(0x1193, 7, "CMPU", direct _, cmpu _),
		(0x119C, 7, "CMPS", direct _, cmps _),
		(0x11A3, 7, "CMPU", indexed _, cmpu _),
		(0x11AC, 7, "CMPS", indexed _, cmps _),
		(0x11B3, 8, "CMPU", extended _, cmpu _),
		(0x11BC, 8, "CMPS", extended _, cmps _)
	)

	val MAX_OPCODE = 0x11BD
	
	val __instructionByOpcodeIndex = new Array[(Int, Int, String, (Int, Int) => Unit, Int => Any)](MAX_OPCODE)
    for (i <- 0 until MAX_OPCODE) __instructionByOpcodeIndex(i) = __find(i)
	def __find(opcode:Int) = INSTRUCTION_TABLE.find(_._1 == opcode).getOrElse(-1, 0, "???", illegal _, illegalop _)
	
	def get(opcode:Int) = __instructionByOpcodeIndex(opcode)

	
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
	  
		ccSetN { (k & 0x80) != 0 }
		ccSetZ { (k & 0xFF) == 0 }
		ccSetV { (k & 0xFF) == 0x80 }
		
		k
	}
	def __dec(i:Int) = {
		val k = i - 1
		
		ccSetN { (k & 0x80) != 0 }
		ccSetZ { (k & 0xFF) == 0 }
		ccSetV { (k & 0xFF) == 0x7F }
		
		k
	}
	def __rol(i:Int) = {
		val k = (i << 1) | (if (ccC) 1 else 0) 
		
		ccSetN { (k & 0x80) != 0 }
		ccSetZ { (k & 0xFF) == 0 }
		ccSetV { ((k ^ i) & 0x80) == 1 }
		ccSetC { (k & 0x100) != 0 }
		
		k
	}
	def __ror(i:Int) = {
		val k = (i >> 1) | (if (ccC) 0x80 else 0)
		
		ccSetN { (k & 0x80) != 0 }
		ccSetZ { (k & 0xFF) == 0 }
		ccSetC { (i & 1) != 0 }
		
		k
	}		
	def __lsr(i:Int) = {
		val k = i >> 1

		ccSetN { false }
		ccSetZ { (k & 0xFF) == 0 }
		ccSetC { (i & 1) != 0 }
		
		k
	}
	def __lsl(i:Int) = {
	    val k = i << 1
	    
	    ccSetN { (k & 0x80) != 0 }
	    ccSetZ { (k & 0xFF) == 0 }
	    ccSetV { ((k ^ i) & 0x80) == 1 }
	    ccSetC { (k & 0x100) != 0 }
	    
	    k
	}
	def __asr(i:Int) = __unimpl
	def __asl(i:Int) = __unimpl
	
	def __com(i:Int) = {
		val k = i ^ -1
	  
		ccSetN { (k & 0x80) == 0 }
		ccSetZ { (k & 0xFF) == 0 }
		ccSetV { false }
		ccSetC { true }
		
		k
	}
	def __neg(i:Int) = {
		val k = -i 
		
		ccSetN { (k & 0x80) == 0 }
		ccSetZ { (k & 0xFF) == 0 }
		ccSetV { ((k ^ i) & 0x80) == 1 }
		ccSetC { (k & 0xFF) == 0 }
		
		k
	}
	def __clr = {
		ccSetN { false }
		ccSetZ { true }
		ccSetV { false }
		ccSetC { false }
		
		0
	}
	def __sub8(i:Int, j:Int) = {			
		val k = i - j
				
		ccSetN { k < 0 }
		ccSetZ { k == 0 }
		ccSetV { i > 0x7F && k < 0x80 || k < -0x80 }
		ccSetC { k < 0 }
		
		k
	}
	def __sub16(i:Int, j:Int) = {
		val k = i - j
		
		ccSetN { k < 0 }
		ccSetZ { k == 0 }
		ccSetV { i > 0x7FFF && k < 0x8000 || k < -0x8000 }
		ccSetC { k < 0 }
		
		k
	}
	def __sbc(i:Int, j:Int) = __unimpl
	
	def __cmp8 = __sub8 _
	def __cmp16 = __sub16 _
	
	def __da(i:Int) = {
		val k = i + (if (maskLS(4, i) > 9 || ccH) 6 else 0) + (if (maskLS(4, i >> 4) > 9 || ccC) 96 else 0)
	    
		ccSetN { (k & 0x80) != 0 }
		ccSetZ { (k & 0xFF) == 0 }
		ccSetV { i < 0x80 && k > 0x7F || k > 0x17F }
		ccSetC { k > 0xFF }
		
		k
	}
	
	def __mul(i:Int, j:Int) = {
	    val k = i * j

		ccSetZ { (k & 0xFFFF) == 0 }
	    ccSetC { (k & 0x80) != 0 }
	    
	    k
	}
	
	def __add8(i:Int, j:Int) = {
		val k = i + j
		
		// TODO half carry
		ccSetN { (k & 0x80) != 0 }
		ccSetZ { (k & 0xFF) == 0 }
		ccSetV { i < 0x80 && k > 0x7F || k > 0x17F }
		ccSetC { k > 0xFF }
		
		k
	}
	def __add16(i:Int, j:Int) = {
		val k = i + j
		
		// TODO half carry
		ccSetN { (k & 0x8000) != 0 }
		ccSetZ { (k & 0xFFFF) == 0 }
		ccSetV { i < 0x8000 && k > 0x7FFF || k > 0x17FFF }
		ccSetC { k > 0xFFFF }
		
		k
	}
	def __adc(i:Int, j:Int) = {
		val k = i + j + (if (ccC) 1 else 0)
		
		// TODO half carry
		ccSetN { (k & 0x80) != 0 }
		ccSetZ { (k & 0xFF) == 0 }
		ccSetV { i < 0x80 && k > 0x7F || k > 0x17F } // fishy?
		ccSetC { k > 0xFF }
		
		k
	}
	def __bit = __and _
	def __ld = __tst _ 
	def __st = __tst _
	def __eor(i:Int, j:Int) = __tst(i ^ j)
	def __or(i:Int, j:Int) = __tst(i | j)
	def __and(i:Int, j:Int) = __tst(i & j)
	def __tst(i:Int) = {
		ccSetN { (i & 0x80) != 0 }
		ccSetZ { i == 0 }
		ccSetV { false }
		
		i
	}
	def __bsr(ea:Int) = __jsr(ea)
	def __lbsr(ea:Int) = __jsr(ea)
	def __jsr(ea:Int) = {
		__push16_S(PC)
		setPC(ea)
	}
	
	def __orcc(i:Int) = setCC(CC | i)
	def __andcc(i:Int) = setCC(CC & i)
	
	def __lea(ea:Int) = {
    	ccSetZ { ea == 0 }
    	ea
	}
	
	def __getRegVal(reg:Int) = reg match {
	    case 0x0 => D
	    case 0x1 => X
	    case 0x2 => Y
	    case 0x3 => U
	    case 0x4 => S
	    case 0x5 => PC
	    case 0x8 => A
	    case 0x9 => B
	    case 0xA => CC
	    case 0xB => DP
	}

	def __setRegVal(reg:Int, v:Int) = reg match {
        case 0x0 => setD(v)
	    case 0x1 => setX(v)
	    case 0x2 => setY(v)
	    case 0x3 => setU(v)
	    case 0x4 => setS(v)
	    case 0x5 => setPC(v)
	    case 0x8 => setA(v)
	    case 0x9 => setB(v)
	    case 0xA => setCC(v)
	    case 0xB => setDP(v)
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
	def lbsr(ea:Int) = __lbsr(ea)
	def daa(ea:Int) = setA(__da(A))
	def orcc(ea:Int) = __orcc(memB(ea))
	def andcc(ea:Int) = __andcc(memB(ea))
	def sex(ea:Int) = unimpl

	def exg(ea:Int) = {
	    val dst = (memB(ea) & 0xF0) >> 4
	    val src = memB(ea) & 0x0F
	    
		val dstVal = __getRegVal(dst)
	    __setRegVal(dst, __getRegVal(src))
	    __setRegVal(src, dstVal)
	}
	def tfr(ea:Int) = {
	    val dst = (memB(ea) & 0xF0) >> 4
	    val src = memB(ea) & 0x0F
	    
	    __setRegVal(dst, __getRegVal(src))
	}
  
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
	def abx(ea:Int) = setX(B + X)
	def rti(ea:Int) = unimpl
	def cwai(ea:Int) = unimpl
	def mul(ea:Int) = setD(__mul(A, B))
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
	def bita(ea:Int) = __bit(A, memB(ea))
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
	def bitb(ea:Int) = __bit(B, memB(ea))
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

