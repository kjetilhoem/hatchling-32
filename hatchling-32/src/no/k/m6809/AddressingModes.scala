package no.k.m6809

import Cpu._
import InstructionSet._
import RunMe._
import M6809._
import IO._

object AddressingModes {
	def illegal(opcode:Int, pos:Int) {
		printOp(pos, opcode, "unrecognized opcode: " + hexByte(opcode))
		System.exit(0)
	}
	def direct(opcode:Int, pos:Int) {
	    addPC(1)
		if (DBG_LOG_DISASM) printOp(pos, opcode, "<" + hexByte(memB(PC - 1)))
		val ea = DP << 8 | memB(PC - 1)
		operation(opcode)(ea)
	}
	def immediate8(opcode:Int, pos:Int) = immediate(opcode, pos, 1, memB)
	def immediate16(opcode:Int, pos:Int) = immediate(opcode, pos, 2, memW)
	def immediate(opcode:Int, pos:Int, width:Int, accessor:Int => Int) {
		addPC(width)
		val ea = PC - width
		if (DBG_LOG_DISASM) printOp(pos, opcode, "#" + hex(2 * width, accessor(ea)))
		operation(opcode)(ea)
	}
	def extended(opcode:Int, pos:Int) {
	    addPC(2)
		val pcBefore = PC
		val ea = memW(PC - 2)
		if (DBG_LOG_DISASM) printOp(pos, opcode, "$" + hexWord(ea))
		operation(opcode)(ea)
		if (PC != pcBefore) jumped = true
	}  
	def relative8(opcode:Int, pos:Int) = relative(opcode, pos, 1, memSB)
	def relative16(opcode:Int, pos:Int) = relative(opcode, pos, 2, memSW)
	def relative(opcode:Int, pos:Int, width:Int, accessor:Int => Int) {
		addPC(width)
		val pcBefore = PC
		val ea = PC + accessor(pos + 1)
		if (DBG_LOG_DISASM) printOp(pos, opcode, "$" + hexWord(ea))
		operation(opcode)(ea)
		if (PC != pcBefore) jumped = true
	}
	def inherent(opcode:Int, pos:Int) {
		val ea = PC
		
		if (opcode >= 0x34 && opcode <= 0x37) {
		    addPC(1)
		    
		    if (DBG_LOG_DISASM) {
			    val regs = memB(ea)
			    
			    if (opcode == 0x34) {
			        val regStr = 
			            (if ((regs & bit(7)) != 0) "PC," else "") +
			            (if ((regs & bit(6)) != 0) "U," else "") +
			            (if ((regs & bit(5)) != 0) "Y," else "") +
			            (if ((regs & bit(4)) != 0) "X," else "") +
			            (if ((regs & bit(3)) != 0) "DP," else "") +
			            (if ((regs & bit(2)) != 0) "B," else "") +
			            (if ((regs & bit(1)) != 0) "A," else "") +
			            (if ((regs & bit(0)) != 0) "CC," else "");
			        
			    	printOp(pos, opcode, regStr.substring(0, regStr.length() - 1))
			    } else if (opcode == 0x35) {
			        val regStr =
			            (if ((regs & bit(0)) != 0) "CC," else "") +
			            (if ((regs & bit(1)) != 0) "A," else "") +
			            (if ((regs & bit(2)) != 0) "B," else "") +
			            (if ((regs & bit(3)) != 0) "DP," else "") +
			            (if ((regs & bit(4)) != 0) "X," else "") +
			            (if ((regs & bit(5)) != 0) "Y," else "") +
			            (if ((regs & bit(6)) != 0) "U," else "") +
			            (if ((regs & bit(7)) != 0) "PC," else "");
			        
			    	printOp(pos, opcode, regStr.substring(0, regStr.length() - 1))
			    } else if (opcode == 0x36) {
			        val regStr = 
			            (if ((regs & bit(7)) != 0) "PC," else "") +
			            (if ((regs & bit(6)) != 0) "S," else "") +
			            (if ((regs & bit(5)) != 0) "Y," else "") +
			            (if ((regs & bit(4)) != 0) "X," else "") +
			            (if ((regs & bit(3)) != 0) "DP," else "") +
			            (if ((regs & bit(2)) != 0) "B," else "") +
			            (if ((regs & bit(1)) != 0) "A," else "") +
			            (if ((regs & bit(0)) != 0) "CC," else "");
			        
			    	printOp(pos, opcode, regStr.substring(0, regStr.length() - 1))
			    } else if (opcode == 0x37) {
			        val regStr =
			            (if ((regs & bit(0)) != 0) "CC," else "") +
			            (if ((regs & bit(1)) != 0) "A," else "") +
			            (if ((regs & bit(2)) != 0) "B," else "") +
			            (if ((regs & bit(3)) != 0) "DP," else "") +
			            (if ((regs & bit(4)) != 0) "X," else "") +
			            (if ((regs & bit(5)) != 0) "Y," else "") +
			            (if ((regs & bit(6)) != 0) "S," else "") +
			            (if ((regs & bit(7)) != 0) "PC," else "");
			        
			    	printOp(pos, opcode, regStr.substring(0, regStr.length() - 1))
			    }
		    }
		} else if (opcode == 0x1E || opcode == 0x1F) {
		    addPC(1)

		    if (DBG_LOG_DISASM) {
			    val regs = memB(ea)
			    
			    val dstReg = maskLS(4, regs >> 4) match {
			        case 0x0 => "D"
			        case 0x1 => "X"
			        case 0x2 => "Y" 
			        case 0x3 => "U"
			        case 0x4 => "S"
			        case 0x5 => "PC"
			        case 0x8 => "A"
			        case 0x9 => "B"
			        case 0xA => "CC"
			        case 0xB => "DP"
			    }
			    
			    val srcReg = maskLS(4, regs) match {
			        case 0x0 => "D"
			        case 0x1 => "X"
			        case 0x2 => "Y" 
			        case 0x3 => "U"
			        case 0x4 => "S"
			        case 0x5 => "PC"
			        case 0x8 => "A"
			        case 0x9 => "B"
			        case 0xA => "CC"
			        case 0xB => "DP"
			    }			    
			    
			    printOp(pos, opcode, dstReg + "," + srcReg)
		    }
		} else {
		    if (DBG_LOG_DISASM) printOp(pos, opcode, "")
		}
		
		operation(opcode)(ea)
	}
	def indexed(opcode:Int, pos:Int) {
	    import INDEX_MODE._
	    
		val postByte = memB(PC)
		val registerCode = maskLS(2, postByte >> 5)
	
		addPC(1)
		
	    val constOff5 = (postByte & 0x80) == 0
    	val mode = maskLS(4, postByte)
		val indirect = (postByte & bit(5)) != 0
		val constOff5Value = utos5(maskLS(5, postByte)) 
		
		if (!constOff5) {
		    mode match {
		        case PRE_DEC_8 => addR(registerCode)(-1)
		        case PRE_DEC_16 => addR(registerCode)(-2)
		        case _ => {}
		    }
		}
		
		val regVal = (registerCode match {
			case INDEX_REG.X => X
			case INDEX_REG.Y => Y 
			case INDEX_REG.U => U
			case INDEX_REG.S => S
		  })
		
		// determine effective address
		val ea =
		    if (constOff5) {
		    	regVal + constOff5Value
		    } else if (mode == POST_INC_8 
		            || mode == POST_INC_16 
		            || mode == NO_OFFSET 
		            || mode == PRE_DEC_8 
		            || mode == PRE_DEC_16) { 
		    	regVal
		    } else if (mode == A_REG_OFFSET) {
		        regVal + A
		    } else if (mode == B_REG_OFFSET) {
		        regVal + B
		    } else if (mode == D_REG_OFFSET) {
		        regVal + D
		    } else if (mode == REG_RELATIVE_8) {
				if (indirect) unimpl
				addPC(1)
		        regVal + memSB(PC - 1) 
		    } else if (mode == REG_RELATIVE_16) {
				if (indirect) unimpl
				addPC(2)
		        regVal + memSW(PC - 2) 
			} else if (mode == PC_RELATIVE_16) {
				addPC(2)
				PC + memSW(if (indirect) memW(PC - 2) else (PC - 2))
			} else if (mode == EXTENDED_INDIRECT) {
				if (indirect) unimpl
				addPC(2)
				memW(memW(PC - 2))
			} else {
				print("unimpl addressing mode: " + mode);
				0
			}
	    
	    if (DBG_LOG_DISASM) {
			val register = registerCode match {
				case INDEX_REG.X => "X"
				case INDEX_REG.Y => "Y"
				case INDEX_REG.U => "U"
				case INDEX_REG.S => "S"
			}
		
			val operands = if (constOff5) (constOff5Value + "," + register) else mode match {
				case POST_INC_8 => register formatted ",%s+"
				case POST_INC_16 => register formatted ",%s++"
				case PRE_DEC_8 => register formatted ",-%s"
				case PRE_DEC_16 => register formatted ",--%s"
				case NO_OFFSET => register formatted ",%s"
				case A_REG_OFFSET => register formatted "A,%s"
				case B_REG_OFFSET => register formatted "B,%s"
				case D_REG_OFFSET => register formatted "D,%s"
				case REG_RELATIVE_8 => memW(PC - 1) formatted (register formatted "%%02X,%s")
				case REG_RELATIVE_16 => memW(PC - 2) formatted (register formatted "%%04X,%s")
				case PC_RELATIVE_16 => (hexWord(memW(PC - 2))) formatted (if (indirect) "[%s,PC]" else "%s,PC")
				case EXTENDED_INDIRECT => "["+hexWord(memW(PC - 2))+"]"
			}
		    
			printOp(pos, opcode, operands)		        
	    }
	
		// exec
		operation(opcode)(ea)
	
		if (!constOff5) {
			// auto increment/decrement
			mode match {
				case POST_INC_8 => addR(registerCode)(1)	
				case POST_INC_16 => addR(registerCode)(2)
				case _ => {}
			}	
		}
	}  
	
	def addR(registerCode:Int) = registerCode match {
		case INDEX_REG.X => addX _
		case INDEX_REG.Y => addY _
		case INDEX_REG.U => addU _
		case INDEX_REG.S => addS _
	}

  
	def printOp(pos:Int, opcode:Int, operands:String = "") = 
	     	printf("%04X %-10s %-5s %-12s ", pos, (hexMemSection(pos, PC)), mnemonic(opcode), operands)
	
}
