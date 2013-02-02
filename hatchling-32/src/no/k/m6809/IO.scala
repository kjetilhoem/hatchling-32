package no.k.m6809

import RunMe._

object IO {
	def __checked_memSB(address: Int):Int = checkGet8(address, __unchecked_memSB(address))	
	def __checked_memSW(address: Int):Int = checkGet16(address, __unchecked_memSW(address))
	
	def __unchecked_memSB(address: Int):Int = __ram(address)
	def __unchecked_memSW(address: Int):Int = (maskLS8(__ram(address)) << 8) | maskLS8(__ram(address + 1))		
	
	val memSB = if (DBG_MEM_CHECKED) __checked_memSB _ else __unchecked_memSB _
	val memSW = if (DBG_MEM_CHECKED) __checked_memSW _ else __unchecked_memSW _
	
	def memB(address: Int) = maskLS8(memSB(address))
	def memW(address: Int) = maskLS16(memSW(address))
		
	val setMemB = if (DBG_MEM_CHECKED) __checked_setMemB _ else __unchecked_setMemB _
	val setMemW = if (DBG_MEM_CHECKED) __checked_setMemW _ else __unchecked_setMemW _
	
	def __unchecked_setMemB(address:Int, b:Int) = {
	    __ram(address) = b toByte
	}
	def __unchecked_setMemW(address:Int, w:Int) {
		__ram(address) = (w >> 8 toByte)
		__ram(address + 1) = w toByte
	}
	
	def __checked_setMemB(address:Int, b:Int) = {
		checkSet(address, b, 1)
		__unchecked_setMemB(address, b)
	}
	def __checked_setMemW(address:Int, w:Int) {
		checkSet(address, w, 2)
		__unchecked_setMemW(address, w)
	}
	def checkSet(at:Int, v:Int, width:Int) = if (at < 0x01e9 || at >= RAM_END) ioWrite(at, v, width)
	
	
	def ioWrite(at:Int, v:Int, width:Int) = {
			if (!DBG_LOG_MEM_ANALYSIS) throw new RuntimeException("DBG_LOG_MEM_ANALYSIS should be on");
			analysis += (" W " + hexWord(at) + " << " + hex(width * 2, v) + " \"" +
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
					//case 0x0176 => {if (v == 0x848D) System.exit(0);"?"}
					case 0x01D1 => "Cassette filename length in range 0-8"
					case 0xFF40 => "DOS - Disk Controller command/status reg"
					case 0xFF20 => "PIA 1 A side Data reg"
					case 0xFF22 => "PIA 1 B side Data reg"
					case 0xFF23 => "PIA 1 B side Control reg"
					case 0x0072 => "Restart vector"
					    /*
					case 0xFFC0 => "SAM VDG Reg V0 (clear)"
					case 0xFFC2 => "SAM VDG Reg V1 (clear)"
					case 0xFFC3 => "SAM VDG Reg V1 (set)"
					case 0xFFC4 => "SAM VDG Reg V2 (clear)"
					case 0xFFC5 => "SAM VDG Reg V2 (set)"
					case 0xFFC7 => "SAM Display Offset bit F0 (set)"
					case 0xFFC8 => "SAM Display Offset bit F1 (clear)"
					case 0xFFCA => "SAM Display Offset bit F2 (clear)"
					case 0xFFCB => "SAM Display Offset bit F2 (set)"
					case 0xFFCC => "SAM Display Offset bit F3 (clear)"
					case 0xFFCD => "SAM Display Offset bit F3 (set)"
					*/
					case 0xB64C => "CLOSE token dispatch address"
					    /*
					case 0x848D => "ROM BASIC ????"
					case 0x848E => "ROM BASIC ????"
					case 0x848F => "ROM BASIC ????"
					case 0x8490 => "ROM BASIC ????"
					case 0x8491 => "ROM BASIC ????"
					case 0x84AD => "ROM BASIC ????"
					case 0x84AE => "ROM BASIC ????"
					case 0x84AF => "ROM BASIC ????"
					case 0x84B0 => "ROM BASIC ????"
					case 0x84B1 => "ROM BASIC ????"
					case 0x84B2 => "ROM BASIC ????"
					*/
        
					case _ => {
					    if (at <= 0x0200 || (at >= 0x0134 && at <= 0x0190)) { // PERMIT WRITE
					    	
					    } else {
					        terminate = true
					    }
						"UNHANDLED I/O WRITE"
					}
				}) + "\"")
	}
	
	def checkGet8(at:Int, v:Int) = checkGet(at, hexByte(v), v)
	def checkGet16(at:Int, v:Int) = checkGet(at, hexWord(v), v)
	
	def checkGet(at:Int, vStr:String, v:Int) = {
		if (at < 0x01e9 || at >= ROM_BASIC_END)
		    if (!DBG_LOG_MEM_ANALYSIS) throw new RuntimeException("DBG_LOG_MEM_ANALYSIS should be on");
			analysis += (" R " + hexWord(at) + " >> " + vStr + " \"" +
				(at match {
					case 0x0068 => "Current Line number (0xffff in direct mode)"
					case 0x0070 => "Cassette EOF flag"
					case 0x007E => "%DBADR% Cassette I/O Buffer address - contains 1 + End address of last program loaded"
					case 0x0082 => "Cassette I/O - Pulse width counter"
					case 0x0084 => "Cassette I/O - Bit phase flag"
					case 0x0088 => "Current VDU cursor addr"
					case 0x008A => "Gen purpose 16bit scratch pad / 16bit zero (0x0000) {2}"
					case 0xFF00 => "PIA 0 A side Data reg (kbd + joystick buttons)"
					case 0xFF20 => "PIA 1 A side Data reg"
          
					case _ => {
					    if (at <= 0x0200 || at >= 0x0170 && at <= 0x0190) { // PERMIT WRITE
					    } else {
					        terminate = true
					    }
						
						("UNHANDLED I/O READ " + hexWord(at) + " ==> " + vStr)
					}
				}) 
				+ "\"")
				
		at match {
		  case 0xFF00 => 0xFF
		  case 0xFF20 => 0xFF
		  case _ => v
		}
	}
}
