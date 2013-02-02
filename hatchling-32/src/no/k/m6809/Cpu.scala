package no.k.m6809

import RunMe._
import M6809.CC.FLAG_BIT._
import IO._

object Cpu {
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

	def dump = format(
	    "%04X %04X %04X %04X %04X %02X %04X %s S %04X %04X",
		D, X, Y, U, S, DP, PC, dumpCC, memW(S), memW(S+2))
	
	val ccHi = "EFHINZVC"
	val ccLo = "efhinzvc"
		
	def dumpCC = ("" /: (0 to 7 toList).map { b => 
	    if ((CC & bit(7 - b)) != 0) ccHi(b) else ccLo(b)
	})(_+_)
}
