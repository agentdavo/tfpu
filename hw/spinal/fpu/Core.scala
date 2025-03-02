// SPDX-FileCopyrightText: 2025 David Smith <david.smith@linux.com>
// SPDX-License-Identifier: MIT

package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

// IEEE-754 Single Precision (32-bit)
object Fp32 {
  def apply(): Fp32 = new Fp32()
  def apply(sign: Bool, exp: UInt, mant: Bits): Fp32 = {
    val fp = new Fp32()
    fp.sign := sign
    fp.exp := exp
    fp.mant := mant
    fp
  }
}
class Fp32 extends Bundle {
  val sign = Bool()
  val exp = UInt(8 bits)
  val mant = Bits(23 bits)

  def asBits: Bits = Cat(sign, exp, mant)
  def isNaN: Bool = exp === 255 && mant =/= 0
  def isInf: Bool = exp === 255 && mant === 0
  def isZero: Bool = exp === 0 && mant === 0
  def isDenorm: Bool = exp === 0 && mant =/= 0

  def toFp64: Fp64 = {
    val res = Fp64()
    res.sign := sign
    res.exp := Mux(isZero || isDenorm, U(0, 11 bits), exp.resize(11) + (1023 - 127))
    res.mant := Mux(isDenorm, mant.resize(52), Cat(mant, B(0, 29 bits)))
    res
  }
}

// IEEE-754 Double Precision (64-bit)
object Fp64 {
  def apply(): Fp64 = new Fp64()
  def apply(sign: Bool, exp: UInt, mant: Bits): Fp64 = {
    val fp = new Fp64()
    fp.sign := sign
    fp.exp := exp
    fp.mant := mant
    fp
  }
}
class Fp64 extends Bundle {
  val sign = Bool()
  val exp = UInt(11 bits)
  val mant = Bits(52 bits)

  def asBits: Bits = Cat(sign, exp, mant)
  def isNaN: Bool = exp === 2047 && mant =/= 0
  def isInf: Bool = exp === 2047 && mant === 0
  def isZero: Bool = exp === 0 && mant === 0
  def isDenorm: Bool = exp === 0 && mant =/= 0

  def toFp32: Fp32 = {
    val res = Fp32()
    res.sign := sign
    res.exp := Mux(exp > 127 + 255, U(255, 8 bits), exp - (1023 - 127)).resize(8)
    res.mant := mant(51 downto 29)
    res
  }

  def assignFromBits(bits: Bits): Fp64 = {
    val fp = Fp64()
    fp.sign := bits(63)
    fp.exp := bits(62 downto 52).asUInt
    fp.mant := bits(51 downto 0)
    fp
  }
}

// Rounding Modes (ISM 11.12.1, Table 11.23)
object RoundingMode extends SpinalEnum(binarySequential) {
  val RTZ, RNE, RUP, RDN = newElement()
}

// Exception Flags (ISM 11.13, Table 11.27)
object FpuFlags {
  def apply(): FpuFlags = new FpuFlags()
}
class FpuFlags extends Bundle {
  val NV = Bool() // Invalid Operation
  val NX = Bool() // Inexact
  val OF = Bool() // Overflow
  val UF = Bool() // Underflow
  val DZ = Bool() // Divide by Zero

  def assignFromBits(bits: Bits): FpuFlags = {
    val flags = FpuFlags()
    flags.NV := bits(4)
    flags.NX := bits(3)
    flags.OF := bits(2)
    flags.UF := bits(1)
    flags.DZ := bits(0)
    flags
  }
}

// Floating-Point Status Word (ISM 11.12.1, Table 11.22)
object FpStatus {
  def apply(): FpStatus = FpStatus(B"01", B"00", B"00", B"00")
  def apply(roundingMode: Bits, fpaType: Bits, fpbType: Bits, fpcType: Bits): FpStatus = {
    val status = new FpStatus()
    status.roundingMode := roundingMode
    status.fpaType := fpaType
    status.fpbType := fpbType
    status.fpcType := fpcType
    status.reserved := B(0, 24 bits)
    status
  }
  def assignFromBits(bits: Bits): FpStatus = {
    val status = new FpStatus()
    status.roundingMode := bits(31 downto 30)
    status.fpaType := bits(29 downto 28)
    status.fpbType := bits(27 downto 26)
    status.fpcType := bits(25 downto 24)
    status.reserved := bits(23 downto 0)
    status
  }
}
class FpStatus extends Bundle {
  val roundingMode = Bits(2 bits) // 00=RTZ, 01=RNE, 10=RUP, 11=RDN
  val fpaType = Bits(2 bits)      // 00=single, 01=double
  val fpbType = Bits(2 bits)
  val fpcType = Bits(2 bits)
  val reserved = Bits(24 bits)

  def toRoundingMode: RoundingMode.C = roundingMode.mux(
    B"00" -> RoundingMode.RTZ,
    B"01" -> RoundingMode.RNE,
    B"10" -> RoundingMode.RUP,
    B"11" -> RoundingMode.RDN
  )
}

// Full Instruction Set (ISM Tables 11.24–11.32, Section 11.12.3) with Precision Variants
object FpuOp extends SpinalEnum(binarySequential) {
  val NOP, FPLDNLSN, FPLDNLDB, FPLDNLSNI, FPLDNLDBI, FPLDZEROSN, FPLDZERODB,
  FPLDNLADDSN, FPLDNLADDDB, FPLDNLMULSN, FPLDNLMULDB, FPSTNLSN, FPSTNLDB,
  FPSTNLI32, FPENTRY, FPREV, FPDUP, FPRN, FPRZ, FPRP, FPRM, FPCHKERR,
  FPTESTERR, FPSETERR, FPCLRERR,
  FPGT_S, FPGT_D, FPEQ_S, FPEQ_D, FPORDERED_S, FPORDERED_D, FPNAN, FPNOTFINITE,
  FPCHKI32, FPCHKI64, FPR321OR64, FPR64TOR32, FPRTOI32, FPI321OR32, FPI321OR64,
  FPB321OR64, FPNOROUND, FPINT,
  FPADD_S, FPADD_D, FPSUB_S, FPSUB_D, FPMUL_S, FPMUL_D, FPDIV_S, FPDIV_D,
  FPABS_S, FPABS_D, FPEXPINC32, FPEXPDEC32, FPMULBY2_S, FPMULBY2_D,
  FPDIVBY2_S, FPDIVBY2_D, FPUSQRTFIRST, FPUSQRTSTEP, FPUSQRTLAST_S, FPUSQRTLAST_D,
  FPREMFIRST, FPREMSTEP, FPREM_S, FPREM_D, FPSQRT_S, FPSQRT_D,
  FPRANGE_S, FPRANGE_D, FPGE_S, FPGE_D, FPLG_S, FPLG_D, FPSTALL, FPLDALL = newElement()
}

// Microcode Definition without isSingle (precision encoded in opcode)
object Microcode {
  // Default instance for Mem initialization (NOP, 0 cycles, no action)
  def apply(): Microcode = Microcode(FpuOp.NOP, 0, B"000", False, False, True)
  
  // Full constructor for specific microcode entries
  def apply(op: FpuOp.E, stepCount: Int, unit: Bits, pushStack: Bool, popStack: Bool, trapEnable: Bool): Microcode = {
    val micro = new Microcode()
    micro.op := op
    micro.stepCount := stepCount
    micro.unit := unit
    micro.pushStack := pushStack
    micro.popStack := popStack
    micro.trapEnable := trapEnable
    micro
  }
}
class Microcode extends Bundle {
  val op = FpuOp()
  val stepCount = UInt(4 bits)
  val unit = Bits(3 bits)
  val pushStack = Bool()
  val popStack = Bool()
  val trapEnable = Bool()
}

// Fully Populated Microcode ROM with Detailed Comments
object MicrocodeRom {
  def apply(): Mem[Microcode] = {
    val rom = Mem(Microcode(), 750)
    def set(op: FpuOp.E, steps: Int, unit: Bits, push: Bool, pop: Bool, trap: Bool = True): Unit = {
      rom.write(
        address = op.asUInt, // Fixed: toUInt -> asUInt
        data = Microcode(op, steps, unit, push, pop, trap)
      )
    }

    // Load/Store Operations (ISM Table 11.24)
    set(FpuOp.FPLDNLSN,     1, B"000", True,  False) // Load non-local single from memory, push to stack (1 cycle, no unit)
    set(FpuOp.FPLDNLDB,     1, B"000", True,  False) // Load non-local double from memory, push to stack (1 cycle, no unit)
    set(FpuOp.FPLDNLSNI,    1, B"000", True,  False) // Load non-local indexed single from memory, push to stack (1 cycle, no unit)
    set(FpuOp.FPLDNLDBI,    1, B"000", True,  False) // Load non-local indexed double from memory, push to stack (1 cycle, no unit)
    set(FpuOp.FPLDZEROSN,   1, B"000", True,  False) // Push single-precision zero (0.0) to stack (1 cycle, no unit)
    set(FpuOp.FPLDZERODB,   1, B"000", True,  False) // Push double-precision zero (0.0) to stack (1 cycle, no unit)
    set(FpuOp.FPLDNLADDSN,  2, B"001", True,  False) // Load non-local single and add to stack top, push result (2 cycles, adder unit)
    set(FpuOp.FPLDNLADDDB,  2, B"001", True,  False) // Load non-local double and add to stack top, push result (2 cycles, adder unit)
    set(FpuOp.FPLDNLMULSN,  2, B"010", True,  False) // Load non-local single and multiply with stack top, push result (2 cycles, multiplier unit)
    set(FpuOp.FPLDNLMULDB,  3, B"010", True,  False) // Load non-local double and multiply with stack top, push result (3 cycles, multiplier unit)
    set(FpuOp.FPSTNLSN,     1, B"000", False, True)  // Store single-precision stack top to memory, pop stack (1 cycle, no unit)
    set(FpuOp.FPSTNLDB,     1, B"000", False, True)  // Store double-precision stack top to memory, pop stack (1 cycle, no unit)
    set(FpuOp.FPSTNLI32,    1, B"000", False, True)  // Store int32 from stack top to memory, pop stack (1 cycle, no unit)

    // General Operations (ISM Table 11.25)
    set(FpuOp.FPENTRY,      1, B"000", True,  False) // FPU entry, push initialization value (1 cycle, no unit)
    set(FpuOp.FPREV,        1, B"000", False, False) // Reverse top two stack elements (FA and FB), no push/pop (1 cycle, no unit)
    set(FpuOp.FPDUP,        1, B"000", True,  False) // Duplicate stack top (FA), push duplicate (1 cycle, no unit)

    // Rounding Operations (ISM Table 11.26)
    set(FpuOp.FPRN,         1, B"000", False, False) // Set rounding mode to nearest (RNE), update status (1 cycle, no unit)
    set(FpuOp.FPRZ,         1, B"000", False, False) // Set rounding mode to zero (RTZ), update status (1 cycle, no unit)
    set(FpuOp.FPRP,         1, B"000", False, False) // Set rounding mode to positive (RUP), update status (1 cycle, no unit)
    set(FpuOp.FPRM,         1, B"000", False, False) // Set rounding mode to minus (RDN), update status (1 cycle, no unit)

    // Error Operations (ISM Table 11.27)
    set(FpuOp.FPCHKERR,     1, B"000", True,  False) // Check FP error, push status flags to stack (1 cycle, no unit)
    set(FpuOp.FPTESTERR,    1, B"000", False, False) // Test FP error and clear flags, no stack change (1 cycle, no unit)
    set(FpuOp.FPSETERR,     1, B"000", False, False) // Set FP error flag, no stack change (1 cycle, no unit)
    set(FpuOp.FPCLRERR,     1, B"000", False, False) // Clear FP error flags, no stack change (1 cycle, no unit)

    // Comparison Operations (ISM Table 11.28)
    set(FpuOp.FPGT_S,       1, B"001", True,  True)  // Single-precision greater than, pop 2, push result (1 cycle, adder unit)
    set(FpuOp.FPGT_D,       1, B"001", True,  True)  // Double-precision greater than, pop 2, push result (1 cycle, adder unit)
    set(FpuOp.FPEQ_S,       1, B"001", True,  True)  // Single-precision equality, pop 2, push result (1 cycle, adder unit)
    set(FpuOp.FPEQ_D,       1, B"001", True,  True)  // Double-precision equality, pop 2, push result (1 cycle, adder unit)
    set(FpuOp.FPORDERED_S,  1, B"001", True,  True)  // Single-precision orderability, pop 2, push result (1 cycle, adder unit)
    set(FpuOp.FPORDERED_D,  1, B"001", True,  True)  // Double-precision orderability, pop 2, push result (1 cycle, adder unit)
    set(FpuOp.FPNAN,        1, B"000", True,  True)  // Check NaN on stack top, pop, push result (1 cycle, no unit)
    set(FpuOp.FPNOTFINITE,  1, B"000", True,  True)  // Check not finite on stack top, pop, push result (1 cycle, no unit)
    set(FpuOp.FPCHKI32,     2, B"001", True,  True)  // Check int32 range, pop, push result (2 cycles, adder unit)
    set(FpuOp.FPCHKI64,     2, B"001", True,  True)  // Check int64 range, pop, push result (2 cycles, adder unit)

    // Conversion Operations (ISM Table 11.29)
    set(FpuOp.FPR321OR64,   2, B"001", True,  True)  // Convert real32 to real64, pop, push result (2 cycles, adder unit)
    set(FpuOp.FPR64TOR32,   2, B"001", True,  True)  // Convert real64 to real32, pop, push result (2 cycles, adder unit)
    set(FpuOp.FPRTOI32,     2, B"001", True,  True)  // Convert real to int32, pop, push result (2 cycles, adder unit)
    set(FpuOp.FPI321OR32,   2, B"001", True,  True)  // Convert int32 to real32, pop, push result (2 cycles, adder unit)
    set(FpuOp.FPI321OR64,   2, B"001", True,  True)  // Convert int32 to real64, pop, push result (2 cycles, adder unit)
    set(FpuOp.FPB321OR64,   2, B"001", True,  True)  // Convert bit32 to real64, pop, push result (2 cycles, adder unit)
    set(FpuOp.FPNOROUND,    2, B"001", True,  True)  // Convert real64 to real32 without rounding, pop, push result (2 cycles, adder unit)
    set(FpuOp.FPINT,        2, B"001", True,  True)  // Truncate to integer, pop, push result (2 cycles, adder unit)

    // Arithmetic Operations (ISM Table 11.30)
    set(FpuOp.FPADD_S,      2, B"001", True,  True)  // Single-precision add, pop 2, push result (2 cycles, adder unit)
    set(FpuOp.FPADD_D,      2, B"001", True,  True)  // Double-precision add, pop 2, push result (2 cycles, adder unit)
    set(FpuOp.FPSUB_S,      2, B"001", True,  True)  // Single-precision subtract, pop 2, push result (2 cycles, adder unit)
    set(FpuOp.FPSUB_D,      2, B"001", True,  True)  // Double-precision subtract, pop 2, push result (2 cycles, adder unit)
    set(FpuOp.FPMUL_S,      2, B"010", True,  True)  // Single-precision multiply, pop 2, push result (2 cycles, multiplier unit)
    set(FpuOp.FPMUL_D,      3, B"010", True,  True)  // Double-precision multiply, pop 2, push result (3 cycles, multiplier unit)
    set(FpuOp.FPDIV_S,      7, B"100", True,  True)  // Single-precision divide, pop 2, push result (7 cycles, divider unit)
    set(FpuOp.FPDIV_D,     15, B"100", True,  True)  // Double-precision divide, pop 2, push result (15 cycles, divider unit)
    set(FpuOp.FPABS_S,      1, B"001", True,  True)  // Single-precision absolute, pop, push result (1 cycle, adder unit)
    set(FpuOp.FPABS_D,      1, B"001", True,  True)  // Double-precision absolute, pop, push result (1 cycle, adder unit)
    set(FpuOp.FPEXPINC32,   2, B"001", True,  True)  // Multiply single-precision by 2^32, pop, push result (2 cycles, adder unit)
    set(FpuOp.FPEXPDEC32,   2, B"001", True,  True)  // Divide single-precision by 2^32, pop, push result (2 cycles, adder unit)
    set(FpuOp.FPMULBY2_S,   2, B"010", True,  True)  // Single-precision multiply by 2.0, pop, push result (2 cycles, multiplier unit)
    set(FpuOp.FPMULBY2_D,   2, B"010", True,  True)  // Double-precision multiply by 2.0, pop, push result (2 cycles, multiplier unit)
    set(FpuOp.FPDIVBY2_S,   2, B"100", True,  True)  // Single-precision divide by 2.0, pop, push result (2 cycles, divider unit)
    set(FpuOp.FPDIVBY2_D,   2, B"100", True,  True)  // Double-precision divide by 2.0, pop, push result (2 cycles, divider unit)

    // Compatibility Operations (ISM Table 11.31)
    set(FpuOp.FPUSQRTFIRST, 2, B"100", False, False) // Square root first step, no stack change (2 cycles, divider unit)
    set(FpuOp.FPUSQRTSTEP,  2, B"100", False, False) // Square root step, no stack change (2 cycles, divider unit)
    set(FpuOp.FPUSQRTLAST_S, 7, B"100", True, True)  // Single-precision square root last step, pop, push result (7 cycles, divider unit)
    set(FpuOp.FPUSQRTLAST_D,15, B"100", True, True)  // Double-precision square root last step, pop, push result (15 cycles, divider unit)
    set(FpuOp.FPREMFIRST,   5, B"100", False, False) // Remainder first step, no stack change (5 cycles, divider unit)
    set(FpuOp.FPREMSTEP,    2, B"100", False, False) // Remainder step, no stack change (2 cycles, divider unit)

    // Additional Operations (ISM Table 11.32)
    set(FpuOp.FPREM_S,      7, B"100", True,  True)  // Single-precision remainder, pop 2, push result (7 cycles, divider unit)
    set(FpuOp.FPREM_D,     15, B"100", True,  True)  // Double-precision remainder, pop 2, push result (15 cycles, divider unit)
    set(FpuOp.FPSQRT_S,     7, B"100", True,  True)  // Single-precision square root, pop, push result (7 cycles, divider unit)
    set(FpuOp.FPSQRT_D,    15, B"100", True,  True)  // Double-precision square root, pop, push result (15 cycles, divider unit)
    set(FpuOp.FPRANGE_S,    5, B"001", True,  True)  // Single-precision range reduce, pop, push result (5 cycles, adder unit)
    set(FpuOp.FPRANGE_D,    5, B"001", True,  True)  // Double-precision range reduce, pop, push result (5 cycles, adder unit)
    set(FpuOp.FPGE_S,       1, B"001", True,  True)  // Single-precision greater or equal, pop 2, push result (1 cycle, adder unit)
    set(FpuOp.FPGE_D,       1, B"001", True,  True)  // Double-precision greater or equal, pop 2, push result (1 cycle, adder unit)
    set(FpuOp.FPLG_S,       1, B"001", True,  True)  // Single-precision less or greater, pop 2, push result (1 cycle, adder unit)
    set(FpuOp.FPLG_D,       1, B"001", True,  True)  // Double-precision less or greater, pop 2, push result (1 cycle, adder unit)

    // State Saving (ISM 11.12.3)
    set(FpuOp.FPSTALL,      1, B"000", False, True)  // Store all state to memory, pop stack (1 cycle, no unit)
    set(FpuOp.FPLDALL,      1, B"000", True,  False) // Load all state from memory, push to stack (1 cycle, no unit)

    rom
  }
}