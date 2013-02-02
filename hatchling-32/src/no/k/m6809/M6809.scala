package no.k.m6809

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
		val NO_OFFSET = 4
		val B_REG_OFFSET = 5
		val A_REG_OFFSET = 6
		val REG_RELATIVE_8 = 8
		val REG_RELATIVE_16 = 9
		val D_REG_OFFSET = 11
		val PC_RELATIVE_16 = 13			
		val EXTENDED_INDIRECT = 15
	}
}

