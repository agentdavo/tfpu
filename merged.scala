package fpu

import spinal.core._
import spinal.core.sim._

object Config {
  def spinal = SpinalConfig(
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    ),
    onlyStdLogicVectorAtTopLevelIo = true
  )
  def sim = SimConfig.withConfig(spinal).withFstWave
}package fpu

import spinal.core._
import spinal.lib._

class DividerRooter extends Component {
  val io = new Bundle {
    val a = in(Fp64())
    val b = in(Fp64())
    val isDiv = in Bool()
    val isSingle = in Bool()
    val roundingMode = in(RoundingMode())
    val result = out(Fp64())
    val flags = out(FpuFlags())
  }

  val dividend = io.a.mant.asUInt.resize(108 bits)
  val divisor = io.b.mant.asUInt.resize(108 bits)
  val quotient = Vec(SInt(2 bits), 54)
  val remainder = Reg(SInt(108 bits)) init(0)
  val root = Reg(UInt(54 bits)) init(0)
  val qDigits = Vec(SInt(2 bits), 54)

  when(io.isDiv) {
    for (i <- 0 to 53) {
      remainder := (remainder << 1) + (dividend(53 - i) ## B(0, 53 bits)).asSInt
      qDigits(i) := remainder / divisor.asSInt
      remainder := remainder - (qDigits(i) * divisor.asSInt).resize(108 bits)
    }
    quotient := qDigits
  } otherwise {
    for (i <- 0 to 13) {
      remainder := (remainder << 4) + (dividend(53 - i * 4, (53 - i * 4) - 3 bits) ## B(0, 60 bits)).asSInt
      qDigits(i) := remainder / (root.asSInt << 1)
      remainder := remainder - (qDigits(i) * (root.asSInt << 1)).resize(108 bits)
      root := (root << 2) + qDigits(i).asUInt
    }
  }

  val mantBits = quotient.asBits.resize(54 bits)
  val sticky = mantBits(0)
  val roundUp = io.roundingMode.mux(
    RoundingMode.RNE -> (mantBits(1) && (sticky || mantBits(2))),
    RoundingMode.RTZ -> False,
    RoundingMode.RUP -> (sticky && !io.a.sign),
    RoundingMode.RDN -> (sticky && io.a.sign)
  )
  val roundedMant = mantBits.asUInt + roundUp.asUInt
  io.result.sign := io.a.sign ^ io.b.sign
  io.result.exp := io.a.exp - io.b.exp + 1023
  io.result.mant := Mux(io.isSingle, roundedMant(51, 29 bits).resize(52 bits), roundedMant(51, 0 bits)).asBits
  io.flags.NV := False
  io.flags.NX := False
  io.flags.OF := False
  io.flags.UF := False
  io.flags.DZ := io.b.isZero
}// SPDX-FileCopyrightText: 2025 David Smith <david.smith@linux.com>
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

  override def asBits: Bits = Cat(sign, exp, mant)  // Added override

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

  override def asBits: Bits = Cat(sign, exp, mant)  // Added override

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

  override def assignFromBits(bits: Bits): Unit = {  // Changed to Unit, added override
    sign := bits(63)
    exp := bits(62 downto 52).asUInt
    mant := bits(51 downto 0)
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

  override def assignFromBits(bits: Bits): Unit = {  // Changed to Unit, added override
    NV := bits(4)
    NX := bits(3)
    OF := bits(2)
    UF := bits(1)
    DZ := bits(0)
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
        address = op.asBits.asUInt,
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
}// SPDX-FileCopyrightText: 2025 David Smith <david.smith@linux.com>
// SPDX-License-Identifier: MIT

package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

class FPU extends Component {
  val io = new Bundle {
    val cmd       = slave Stream(FpuOp())
    val memAddr   = out UInt(32 bits)
    val memDataIn = in Bits(64 bits)
    val memDataOut= out Bits(64 bits)
    val memWrite  = out Bool()
    val result    = master Flow(Bits(64 bits))
    val flags     = out(FpuFlags())
    val trap      = out Bool()
  }

  val fetch   = CtrlLink()
  val decode  = CtrlLink()
  val execute = CtrlLink()
  val f2d     = StageLink(fetch.down, decode.up)
  val d2e     = StageLink(decode.down, execute.up)

  val OPCODE    = Payload(FpuOp())
  val MICROCODE = Payload(Microcode())
  val STEP      = Payload(UInt(4 bits))

  val stack  = Vec.fill(3)(Reg(Fp64()) init { val temp = Fp64(); temp.assignFromBits(0); temp })  // Updated init
  val status = Reg(FpStatus()) init FpStatus(B"01", B"00", B"00", B"00")
  val microcode = MicrocodeRom()

  val fetcher = new fetch.Area {
    io.cmd.ready := False
    when(io.cmd.valid) {
      OPCODE := io.cmd.payload
      STEP := 0
      io.cmd.ready := True
    }
  }

  val decoder = new decode.Area {
    MICROCODE := microcode(OPCODE.asBits.asUInt)
    io.memAddr := 0
    io.memWrite := False
  }

  val executor = new execute.Area {
    val micro = MICROCODE
    val step = STEP
    val resultReg = Reg(Fp64()) init { val temp = Fp64(); temp.assignFromBits(0); temp }  // Updated init
    val flagsReg = Reg(FpuFlags()) init { val temp = FpuFlags(); temp.assignFromBits(0); temp }  // Updated init

    val adder = new DualAdder
    val mul = new Multiplier
    val divRoot = new DividerRooter
    val vcu = new VCU

    val isSingle = status.fpaType === B"00" && status.fpbType === B"00"
    val effectiveOp = micro.op.mux(
      FpuOp.FPADD_S      -> Mux(isSingle, FpuOp.FPADD_S, FpuOp.FPADD_D),
      FpuOp.FPADD_D      -> Mux(isSingle, FpuOp.FPADD_S, FpuOp.FPADD_D),
      FpuOp.FPSUB_S      -> Mux(isSingle, FpuOp.FPSUB_S, FpuOp.FPSUB_D),
      FpuOp.FPSUB_D      -> Mux(isSingle, FpuOp.FPSUB_S, FpuOp.FPSUB_D),
      FpuOp.FPMUL_S      -> Mux(isSingle, FpuOp.FPMUL_S, FpuOp.FPMUL_D),
      FpuOp.FPMUL_D      -> Mux(isSingle, FpuOp.FPMUL_S, FpuOp.FPMUL_D),
      FpuOp.FPDIV_S      -> Mux(isSingle, FpuOp.FPDIV_S, FpuOp.FPDIV_D),
      FpuOp.FPDIV_D      -> Mux(isSingle, FpuOp.FPDIV_S, FpuOp.FPDIV_D),
      FpuOp.FPABS_S      -> Mux(isSingle, FpuOp.FPABS_S, FpuOp.FPABS_D),
      FpuOp.FPABS_D      -> Mux(isSingle, FpuOp.FPABS_S, FpuOp.FPABS_D),
      FpuOp.FPMULBY2_S   -> Mux(isSingle, FpuOp.FPMULBY2_S, FpuOp.FPMULBY2_D),
      FpuOp.FPMULBY2_D   -> Mux(isSingle, FpuOp.FPMULBY2_S, FpuOp.FPMULBY2_D),
      FpuOp.FPDIVBY2_S   -> Mux(isSingle, FpuOp.FPDIVBY2_S, FpuOp.FPDIVBY2_D),
      FpuOp.FPDIVBY2_D   -> Mux(isSingle, FpuOp.FPDIVBY2_S, FpuOp.FPDIVBY2_D),
      FpuOp.FPSQRT_S     -> Mux(isSingle, FpuOp.FPSQRT_S, FpuOp.FPSQRT_D),
      FpuOp.FPSQRT_D     -> Mux(isSingle, FpuOp.FPSQRT_S, FpuOp.FPSQRT_D),
      FpuOp.FPREM_S      -> Mux(isSingle, FpuOp.FPREM_S, FpuOp.FPREM_D),
      FpuOp.FPREM_D      -> Mux(isSingle, FpuOp.FPREM_S, FpuOp.FPREM_D),
      FpuOp.FPUSQRTLAST_S -> Mux(isSingle, FpuOp.FPUSQRTLAST_S, FpuOp.FPUSQRTLAST_D),
      FpuOp.FPUSQRTLAST_D -> Mux(isSingle, FpuOp.FPUSQRTLAST_S, FpuOp.FPUSQRTLAST_D),
      FpuOp.FPRANGE_S    -> Mux(isSingle, FpuOp.FPRANGE_S, FpuOp.FPRANGE_D),
      FpuOp.FPRANGE_D    -> Mux(isSingle, FpuOp.FPRANGE_S, FpuOp.FPRANGE_D),
      FpuOp.FPGT_S       -> Mux(isSingle, FpuOp.FPGT_S, FpuOp.FPGT_D),
      FpuOp.FPGT_D       -> Mux(isSingle, FpuOp.FPGT_S, FpuOp.FPGT_D),
      FpuOp.FPEQ_S       -> Mux(isSingle, FpuOp.FPEQ_S, FpuOp.FPEQ_D),
      FpuOp.FPEQ_D       -> Mux(isSingle, FpuOp.FPEQ_S, FpuOp.FPEQ_D),
      FpuOp.FPORDERED_S  -> Mux(isSingle, FpuOp.FPORDERED_S, FpuOp.FPORDERED_D),
      FpuOp.FPORDERED_D  -> Mux(isSingle, FpuOp.FPORDERED_S, FpuOp.FPORDERED_D),
      FpuOp.FPGE_S       -> Mux(isSingle, FpuOp.FPGE_S, FpuOp.FPGE_D),
      FpuOp.FPGE_D       -> Mux(isSingle, FpuOp.FPGE_S, FpuOp.FPGE_D),
      FpuOp.FPLG_S       -> Mux(isSingle, FpuOp.FPLG_S, FpuOp.FPLG_D),
      FpuOp.FPLG_D       -> Mux(isSingle, FpuOp.FPLG_S, FpuOp.FPLG_D),
      default            -> micro.op
    )
    val effectiveMicro = microcode(effectiveOp.asBits.asUInt)
    val effectiveStepCount = effectiveMicro.stepCount

    adder.io.a := stack(0)
    adder.io.b := stack(1)
    adder.io.isSub := effectiveMicro.op === FpuOp.FPSUB_S || effectiveMicro.op === FpuOp.FPSUB_D
    adder.io.roundingMode := status.toRoundingMode
    adder.io.isSingle := isSingle
    mul.io.a := stack(0)
    mul.io.b := stack(1)
    mul.io.roundingMode := status.toRoundingMode
    mul.io.isSingle := isSingle
    divRoot.io.a := stack(0)
    divRoot.io.b := stack(1)
    divRoot.io.isDiv := effectiveMicro.op === FpuOp.FPDIV_S || effectiveMicro.op === FpuOp.FPDIV_D
    divRoot.io.roundingMode := status.toRoundingMode
    divRoot.io.isSingle := isSingle
    vcu.io.a := stack(0)
    vcu.io.b := stack(1)
    vcu.io.op := effectiveMicro.op
    vcu.io.isSingle := isSingle

    io.trap := effectiveMicro.trapEnable && (flagsReg.NV || flagsReg.DZ || flagsReg.OF || flagsReg.UF || flagsReg.NX)
    when(io.trap) {
      resultReg := stack(0)
    }

    when(isValid && step < effectiveStepCount) {
      val doNorm = vcu.io.needsNorm && step === 0
      val doBypass = vcu.io.bypass && !doNorm

      when(doNorm) {
        when(stack(0).isDenorm) {
          val shift = CountOne(stack(0).mant.asUInt).resize(6)
          stack(0).exp := stack(0).exp - shift
          stack(0).mant := (stack(0).mant << shift)(51 downto 0)
        }
        when(stack(1).isDenorm) {
          val shift = CountOne(stack(1).mant.asUInt).resize(6)
          stack(1).exp := stack(1).exp - shift
          stack(1).mant := (stack(1).mant << shift)(51 downto 0)
        }
      }

      resultReg := Mux(doBypass, vcu.io.result, resultReg)
      flagsReg := Mux(doBypass, vcu.io.flags, flagsReg)

      when(!doNorm && !doBypass) {
        switch(effectiveMicro.op) {
          is(FpuOp.FPLDNLSN) {
            val fp32Temp = Fp32()
            fp32Temp.assignFromBits(io.memDataIn(31 downto 0))
            resultReg := fp32Temp.toFp64
            status.fpaType := B"00"
          }
          is(FpuOp.FPLDNLDB) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(io.memDataIn)
            resultReg := fp64Temp
            status.fpaType := B"01"
          }
          is(FpuOp.FPLDNLSNI) {
            val fp32Temp = Fp32()
            fp32Temp.assignFromBits(io.memDataIn(31 downto 0))
            resultReg := fp32Temp.toFp64
            status.fpaType := B"00"
          }
          is(FpuOp.FPLDNLDBI) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(io.memDataIn)
            resultReg := fp64Temp
            status.fpaType := B"01"
          }
          is(FpuOp.FPLDZEROSN) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(0)
            resultReg := fp64Temp
            status.fpaType := B"00"
          }
          is(FpuOp.FPLDZERODB) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(0)
            resultReg := fp64Temp
            status.fpaType := B"01"
          }
          is(FpuOp.FPLDNLADDSN) {
            when(step === 0) {
              val fp32Temp = Fp32()
              fp32Temp.assignFromBits(io.memDataIn(31 downto 0))
              resultReg := fp32Temp.toFp64
            }.otherwise {
              adder.io.a := resultReg
              resultReg := adder.io.result
              flagsReg := adder.io.flags
            }
            status.fpaType := B"00"
          }
          is(FpuOp.FPLDNLADDDB) {
            when(step === 0) {
              val fp64Temp = Fp64()
              fp64Temp.assignFromBits(io.memDataIn)
              resultReg := fp64Temp
            }.otherwise {
              adder.io.a := resultReg
              resultReg := adder.io.result
              flagsReg := adder.io.flags
            }
            status.fpaType := B"01"
          }
          is(FpuOp.FPLDNLMULSN) {
            when(step === 0) {
              val fp32Temp = Fp32()
              fp32Temp.assignFromBits(io.memDataIn(31 downto 0))
              resultReg := fp32Temp.toFp64
            }.otherwise {
              mul.io.a := resultReg
              resultReg := mul.io.result
              flagsReg := mul.io.flags
            }
            status.fpaType := B"00"
          }
          is(FpuOp.FPLDNLMULDB) {
            when(step === 0) {
              val fp64Temp = Fp64()
              fp64Temp.assignFromBits(io.memDataIn)
              resultReg := fp64Temp
            }.otherwise {
              mul.io.a := resultReg
              resultReg := mul.io.result
              flagsReg := mul.io.flags
            }
            status.fpaType := B"01"
          }
          is(FpuOp.FPSTNLSN) {
            io.memDataOut := stack(0).toFp32.asBits.resize(64)
            io.memWrite := True
          }
          is(FpuOp.FPSTNLDB) {
            io.memDataOut := stack(0).asBits
            io.memWrite := True
          }
          is(FpuOp.FPSTNLI32) {
            io.memDataOut := stack(0).mant(31 downto 0).asSInt.asBits.resize(64)
            io.memWrite := True
          }
          is(FpuOp.FPENTRY) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(io.memDataIn)
            resultReg := fp64Temp
            status.fpaType := B"01"
          }
          is(FpuOp.FPREV) {
            val temp = stack(0)
            stack(0) := stack(1)
            stack(1) := temp
            val tempType = status.fpaType
            status.fpaType := status.fpbType
            status.fpbType := tempType
          }
          is(FpuOp.FPDUP) {
            resultReg := stack(0)
          }
          is(FpuOp.FPRN) {
            status.roundingMode := B"01"
          }
          is(FpuOp.FPRZ) {
            status.roundingMode := B"00"
          }
          is(FpuOp.FPRP) {
            status.roundingMode := B"10"
          }
          is(FpuOp.FPRM) {
            status.roundingMode := B"11"
          }
          is(FpuOp.FPCHKERR) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(Cat(flagsReg.asBits.resize(52), U"0".resize(11), flagsReg.NV))
            resultReg := fp64Temp
          }
          is(FpuOp.FPTESTERR) {
            flagsReg.NV := False
            flagsReg.NX := False
            flagsReg.OF := False
            flagsReg.UF := False
            flagsReg.DZ := False
          }
          is(FpuOp.FPSETERR) {
            flagsReg.NV := True
          }
          is(FpuOp.FPCLRERR) {
            flagsReg.NV := False
            flagsReg.NX := False
            flagsReg.OF := False
            flagsReg.UF := False
            flagsReg.DZ := False
          }
          is(FpuOp.FPGT_S, FpuOp.FPGT_D) {
            adder.io.isSub := True
            val diff = adder.io.result
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(Cat(diff.sign, U"0".resize(11), B"0".resize(52)))
            resultReg := fp64Temp
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPEQ_S, FpuOp.FPEQ_D) {
            adder.io.isSub := True
            val diff = adder.io.result
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(Cat(~diff.isZero, U"0".resize(11), B"0".resize(52)))
            resultReg := fp64Temp
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPORDERED_S, FpuOp.FPORDERED_D) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(Cat(~(stack(0).isNaN || stack(1).isNaN), U"0".resize(11), B"0".resize(52)))
            resultReg := fp64Temp
          }
          is(FpuOp.FPNAN) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(Cat(stack(0).isNaN, U"0".resize(11), B"0".resize(52)))
            resultReg := fp64Temp
          }
          is(FpuOp.FPNOTFINITE) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(Cat(stack(0).isInf || stack(0).isNaN, U"0".resize(11), B"0".resize(52)))
            resultReg := fp64Temp
          }
          is(FpuOp.FPCHKI32) {
            when(step === 0) {
              resultReg := stack(0)
            }.otherwise {
              val intVal = resultReg.mant(31 downto 0).asSInt
              flagsReg.OF := resultReg.exp > 31
              val fp64Temp = Fp64()
              fp64Temp.assignFromBits(Cat(intVal < 0, U"0".resize(11), intVal.abs.asBits.resize(52)))
              resultReg := fp64Temp
            }
          }
          is(FpuOp.FPCHKI64) {
            when(step === 0) {
              resultReg := stack(0)
            }.otherwise {
              val intVal = resultReg.mant.asSInt
              flagsReg.OF := resultReg.exp > 63
              val fp64Temp = Fp64()
              fp64Temp.assignFromBits(Cat(intVal < 0, U"0".resize(11), intVal.abs.asBits.resize(52)))
              resultReg := fp64Temp
            }
          }
          is(FpuOp.FPR321OR64) {
            when(step === 0) {
              resultReg := stack(0)
            }.otherwise {
              resultReg := resultReg.toFp32.toFp64
              flagsReg.NX := stack(0).mant(28 downto 0) =/= 0
            }
            status.fpaType := B"01"
          }
          is(FpuOp.FPR64TOR32) {
            when(step === 0) {
              resultReg := stack(0)
            }.otherwise {
              resultReg := resultReg.toFp32.toFp64
              flagsReg.NX := stack(0).mant(28 downto 0) =/= 0
            }
            status.fpaType := B"00"
          }
          is(FpuOp.FPRTOI32) {
            when(step === 0) {
              resultReg := stack(0)
            }.otherwise {
              val intVal = resultReg.mant(31 downto 0).asSInt
              flagsReg.OF := resultReg.exp > 31
              val fp64Temp = Fp64()
              fp64Temp.assignFromBits(Cat(intVal < 0, U"0".resize(11), intVal.abs.asBits.resize(52)))
              resultReg := fp64Temp
            }
          }
          is(FpuOp.FPI321OR32) {
            when(step === 0) {
              val fp64Temp = Fp64()
              fp64Temp.assignFromBits(Cat(stack(0).sign, U(127 + 31), stack(0).mant))
              resultReg := fp64Temp
            }.otherwise {
              resultReg := resultReg.toFp32.toFp64
              flagsReg.NX := stack(0).mant(28 downto 0) =/= 0
            }
            status.fpaType := B"00"
          }
          is(FpuOp.FPI321OR64) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(Cat(stack(0).sign, U(1023 + 31), stack(0).mant))
            resultReg := fp64Temp
            status.fpaType := B"01"
          }
          is(FpuOp.FPB321OR64) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(stack(0).asBits)
            resultReg := fp64Temp
            status.fpaType := B"01"
          }
          is(FpuOp.FPNOROUND) {
            when(step === 0) {
              resultReg := stack(0)
            }.otherwise {
              val fp64Temp = Fp64()
              fp64Temp.assignFromBits(Cat(stack(0).sign, stack(0).exp(7 downto 0).resize(11), stack(0).mant(51 downto 29) << 29))
              resultReg := fp64Temp
            }
            status.fpaType := B"00"
          }
          is(FpuOp.FPINT) {
            when(step === 0) {
              resultReg := stack(0)
            }.otherwise {
              val shift = S(52) - resultReg.exp.asSInt
              when(shift < 0) {
                resultReg.mant := resultReg.mant << shift.abs
              }.otherwise {
                resultReg.mant := resultReg.mant >> shift.asUInt
              }
              resultReg.exp := U(0, 11 bits)
            }
          }
          is(FpuOp.FPADD_S, FpuOp.FPADD_D) {
            resultReg := adder.io.result
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPSUB_S, FpuOp.FPSUB_D) {
            resultReg := adder.io.result
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPMUL_S, FpuOp.FPMUL_D) {
            resultReg := mul.io.result
            flagsReg := mul.io.flags
          }
          is(FpuOp.FPDIV_S, FpuOp.FPDIV_D) {
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPABS_S, FpuOp.FPABS_D) {
            resultReg := stack(0)
            resultReg.sign := False
          }
          is(FpuOp.FPEXPINC32) {
            resultReg := stack(0)
            resultReg.exp := stack(0).exp + 32
            flagsReg.OF := resultReg.exp > 254
          }
          is(FpuOp.FPEXPDEC32) {
            resultReg := stack(0)
            resultReg.exp := stack(0).exp - 32
            flagsReg.UF := resultReg.exp < 1
          }
          is(FpuOp.FPMULBY2_S, FpuOp.FPMULBY2_D) {
            resultReg := stack(0)
            resultReg.exp := stack(0).exp + 1
            flagsReg.OF := resultReg.exp > Mux(isSingle, U(254), U(2046))
          }
          is(FpuOp.FPDIVBY2_S, FpuOp.FPDIVBY2_D) {
            resultReg := stack(0)
            resultReg.exp := stack(0).exp - 1
            flagsReg.UF := resultReg.exp < 1
          }
          is(FpuOp.FPUSQRTFIRST) {
            divRoot.io.isDiv := False
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPUSQRTSTEP) {
            divRoot.io.isDiv := False
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPUSQRTLAST_S, FpuOp.FPUSQRTLAST_D) {
            divRoot.io.isDiv := False
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPREMFIRST) {
            divRoot.io.isDiv := True
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPREMSTEP) {
            divRoot.io.isDiv := True
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPREM_S, FpuOp.FPREM_D) {
            divRoot.io.isDiv := True
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPSQRT_S, FpuOp.FPSQRT_D) {
            divRoot.io.isDiv := False
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPRANGE_S, FpuOp.FPRANGE_D) {
            when(step === 0) {
              resultReg := stack(0)
            } otherwise {
              when(step === 1) {
                resultReg := Mux(resultReg.exp > Mux(isSingle, U(254), U(2046)),
                  { val temp = Fp64(); temp.assignFromBits(Cat(resultReg.sign, Mux(isSingle, U(254, 11 bits), U(2046)), B"0".resize(52))); temp },
                  resultReg)
                flagsReg.OF := resultReg.exp > Mux(isSingle, U(254), U(2046))
              } otherwise {
                when(step === 2) {
                  resultReg := Mux(resultReg.exp < 1,
                    { val temp = Fp64(); temp.assignFromBits(Cat(resultReg.sign, U(1, 11 bits), B"0".resize(52))); temp },
                    resultReg)
                  flagsReg.UF := resultReg.exp < 1
                }
              }
            }
          }
          is(FpuOp.FPGE_S, FpuOp.FPGE_D) {
            adder.io.isSub := True
            val diff = adder.io.result
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(Cat(~(diff.sign || diff.isZero), U"0".resize(11), B"0".resize(52)))
            resultReg := fp64Temp
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPLG_S, FpuOp.FPLG_D) {
            adder.io.isSub := True
            val diff = adder.io.result
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(Cat(diff.sign && !diff.isZero, U"0".resize(11), B"0".resize(52)))
            resultReg := fp64Temp
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPSTALL) {
            io.memDataOut := stack(0).asBits
            io.memWrite := True
          }
          is(FpuOp.FPLDALL) {
            val fp64Temp = Fp64()
            fp64Temp.assignFromBits(io.memDataIn)
            resultReg := fp64Temp
            status := FpStatus.assignFromBits(io.memDataIn(31 downto 0))
          }
        }
      }

      when(effectiveMicro.pushStack) {
        stack(2) := stack(1)
        stack(1) := stack(0)
        stack(0) := resultReg
        status.fpcType := status.fpbType
        status.fpbType := status.fpaType
        status.fpaType := Mux(isSingle, B"00", B"01")
      }
      when(effectiveMicro.popStack) {
        stack(0) := stack(1)
        stack(1) := stack(2)
        val temp = Fp64()
        temp.assignFromBits(0)
        stack(2) := temp
        status.fpaType := status.fpbType
        status.fpbType := status.fpcType
        status.fpcType := B"00"
      }
      STEP := step + 1
    }

    io.result.valid := isValid && step === effectiveStepCount
    io.result.payload := resultReg.asBits
    io.flags := flagsReg
    status.roundingMode := B"01" // Reset to RNE per ISM 11.12
  }

  Builder(fetch, decode, execute, f2d, d2e)
}package fpu

import spinal.core._
import spinal.lib._

import spinal.lib.{misc, _}
import spinal.lib.misc.plugin.FiberPlugin

class FpuPlugin extends FiberPlugin {
}package fpu

import spinal.core._
import spinal.lib._

// Radix-4 Carry-Skip Adder Component
class DualAdder extends Component {
  val io = new Bundle {
    val a = in(Fp64())
    val b = in(Fp64())
    val isSub = in Bool()
    val cin = in Bool()
    val isSingle = in Bool()
    val roundingMode = in(RoundingMode())
    val result = out(Fp64())
    val flags = out(FpuFlags())
  }

  val expDiff = io.a.exp.asSInt - io.b.exp.asSInt
  val alignA = io.a.mant.asUInt.resize(57 bits) >> expDiff.abs
  val alignB = io.b.mant.asUInt.resize(57 bits) >> (-expDiff).abs
  val mantA = Mux(expDiff >= 0, io.a.mant.asUInt.resize(57 bits), alignA)
  val mantB = Mux(expDiff >= 0, alignB, io.b.mant.asUInt.resize(57 bits))

  val adder1 = SInt(58 bits)
  val adder2 = SInt(58 bits)
  val sum1 = mantA.asSInt + Mux(io.isSub, -mantB.asSInt, mantB.asSInt) + io.cin.asSInt
  val sum2 = sum1 >> 1
  adder1 := sum1
  adder2 := sum2

  val p = Vec(Bool(), 57)
  val g = Vec(Bool(), 57)
  for (i <- 0 until 57) {
    p(i) := mantA(i) ^ mantB(i)
    g(i) := mantA(i) & mantB(i)
  }
  val potentialPoints = Vec(Bool(), 57)
  for (i <- 2 until 57) {
    potentialPoints(i) := g(i) | (p(i) & (g(i - 1) | p(i - 1) & g(i - 2)))
  }
  val normShift = CountOne(potentialPoints.reverse).resize(6 bits)

  val overflow = sum1(57)
  val rawMant = Mux(overflow, sum2, sum1)(56 downto 0)
  val sticky = rawMant(0)
  val roundUp = io.roundingMode.mux(
    RoundingMode.RNE -> (rawMant(1) && (sticky || rawMant(2))),
    RoundingMode.RTZ -> False,
    RoundingMode.RUP -> (sticky && !io.a.sign),
    RoundingMode.RDN -> (sticky && io.a.sign)
  )
  val mantNorm = rawMant.asUInt + roundUp.asUInt
  val finalMant = (mantNorm << normShift).resize(57 bits)

  io.result.sign := io.a.sign ^ io.b.sign
  io.result.exp := Mux(expDiff >= 0, io.a.exp, io.b.exp) + overflow.asUInt - normShift
  io.result.mant := Mux(io.isSingle, finalMant(51, 29 bits).resize(52 bits), finalMant(51, 0 bits)).asBits
  io.flags.NV := False
  io.flags.NX := sticky
  io.flags.OF := False
  io.flags.UF := False
  io.flags.DZ := False
} package fpu

import spinal.core._
import spinal.lib._

class VCU extends Component {
  val io = new Bundle {
    val a, b      = in(Fp64())
    val op        = in(FpuOp())
    val isSingle  = in Bool()
    val result    = out(Fp64())
    val flags     = out(FpuFlags())
    val bypass    = out Bool()
    val needsNorm = out Bool()
  }

  // Detect special values
  val aIsNaN = io.a.exp === U(2047) && io.a.mant =/= 0
  val bIsNaN = io.b.exp === U(2047) && io.b.mant =/= 0
  val aIsInf = io.a.exp === U(2047) && io.a.mant === 0
  val bIsInf = io.b.exp === U(2047) && io.b.mant === 0
  val aIsZero = io.a.exp === 0 && io.a.mant === 0
  val bIsZero = io.b.exp === 0 && io.b.mant === 0
  val aIsDenorm = io.a.exp === 0 && io.a.mant =/= 0
  val bIsDenorm = io.b.exp === 0 && io.b.mant =/= 0

  io.bypass := aIsNaN || bIsNaN || aIsInf || bIsInf || aIsZero || bIsZero
  io.needsNorm := aIsDenorm || bIsDenorm

  // NaN result generation
  val nanResult = Fp64()
  nanResult.sign := False
  nanResult.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
  nanResult.mant := io.op.mux(
    FpuOp.FPDIV_S  -> B"00010000000000000000000".resize(52),  // DivZeroByZeroNaN single
    FpuOp.FPDIV_D  -> B"0000000000100000000000000000000000000000000000000000".resize(52),  // DivZeroByZeroNaN double
    FpuOp.FPMUL_S  -> B"00011010000000000000000".resize(52),  // ZeroMulInfNaN single
    FpuOp.FPMUL_D  -> B"0000000000011010000000000000000000000000000000000000".resize(52),  // ZeroMulInfNaN double
    FpuOp.FPSUB_S  -> B"00011000000000000000000".resize(52),  // AddOpInfsNaN single
    FpuOp.FPSUB_D  -> B"0000000000011000000000000000000000000000000000000000".resize(52),  // AddOpInfsNaN double
    FpuOp.FPSQRT_S -> B"00010110000000000000000".resize(52),  // NegSqrtNaN single
    FpuOp.FPSQRT_D -> B"0000000000010110000000000000000000000000000000000000".resize(52),  // NegSqrtNaN double
    default -> Mux(io.isSingle, B"00010000000000000000000".resize(52), B"0000000000100000000000000000000000000000000000000000".resize(52))
  )

  // Handle special cases
  io.result.assignDontCare()
  io.flags.NV := False
  io.flags.NX := False
  io.flags.OF := False
  io.flags.UF := False
  io.flags.DZ := False

  when(aIsNaN || bIsNaN) {
    io.result := nanResult
    io.flags.NV := True
  } elsewhen(aIsInf && bIsInf) {
    when(io.op === FpuOp.FPSUB_S || io.op === FpuOp.FPSUB_D || io.op === FpuOp.FPADD_S || io.op === FpuOp.FPADD_D) {
      io.result := nanResult
      io.flags.NV := True
    } otherwise {
      io.result.sign := io.a.sign ^ io.b.sign
      io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
      io.result.mant := B(0, 52 bits)
    }
  } elsewhen(aIsInf) {
    io.result.sign := io.a.sign
    io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
    io.result.mant := B(0, 52 bits)
  } elsewhen(bIsInf) {
    io.result.sign := io.b.sign
    io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
    io.result.mant := B(0, 52 bits)
  } elsewhen(bIsZero && (io.op === FpuOp.FPDIV_S || io.op === FpuOp.FPDIV_D)) {
    io.result.sign := io.a.sign
    io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
    io.result.mant := B(0, 52 bits)
    io.flags.DZ := True
  } elsewhen(aIsZero && bIsZero && (io.op === FpuOp.FPDIV_S || io.op === FpuOp.FPDIV_D)) {
    io.result := nanResult
    io.flags.NV := True
  } elsewhen(aIsZero) {
    io.result.sign := io.a.sign
    io.result.exp := U(0, 11 bits)
    io.result.mant := B(0, 52 bits)
  }
}package fpu

import spinal.core._
import spinal.lib._

class Multiplier extends Component {
  val io = new Bundle {
    val a = in(Fp64())
    val b = in(Fp64())
    val isSingle = in Bool()
    val roundingMode = in(RoundingMode())
    val result = out(Fp64())
    val flags = out(FpuFlags())
  }

  // Pipeline Stage 1: Input Handling
  val stage1 = new Area {
    val mantA = Reg(UInt(53 bits)) init(0)  // Implicit '1' + 52-bit mantissa
    val mantB = Reg(UInt(53 bits)) init(0)
    val expA = Reg(SInt(11 bits)) init(0)
    val expB = Reg(SInt(11 bits)) init(0)
    val signA = Reg(Bool()) init(False)
    val signB = Reg(Bool()) init(False)
    val isSingleReg = Reg(Bool()) init(False)
    val roundingModeReg = Reg(RoundingMode()) init(RoundingMode.RNE)

    mantA := Cat(U"1", io.a.mant).asUInt
    mantB := Cat(U"1", io.b.mant).asUInt
    expA := io.a.exp.asSInt
    expB := io.b.exp.asSInt
    signA := io.a.sign
    signB := io.b.sign
    isSingleReg := io.isSingle
    roundingModeReg := io.roundingMode
  }

  // Pipeline Stage 2: Partial Product Generation and Summation
  val stage2 = new Area {
    val prodReg = Reg(UInt(108 bits)) init(0)
    val exp = Reg(SInt(12 bits)) init(0)
    val sign = Reg(Bool()) init(False)
    val isSingle = Reg(Bool()) init(False)
    val roundingMode = Reg(RoundingMode()) init(RoundingMode.RNE)

    // Generate and sum partial products using Booth encoding
    val sum = (0 until 27).map(i => {
      val prevBit = if (i == 0) U(0, 1 bit) else stage1.mantB(2 * i - 1)
      val digitBits = if (i < 26) stage1.mantB(2 * i + 1 downto 2 * i) else U"0" ## stage1.mantB(52)
      val digit = (digitBits ## prevBit).asUInt
      digit.mux(
        U(0) -> U(0, 108 bits),
        U(1) -> stage1.mantA.resize(108 bits),
        U(2) -> (stage1.mantA << 1).resize(108 bits),
        U(3) -> ((stage1.mantA << 1) + stage1.mantA).resize(108 bits),
        U(4) -> (stage1.mantA << 2).resize(108 bits),
        default -> U(0, 108 bits)
      ) << (2 * i)
    }).reduceBalancedTree(_ + _)
    prodReg := sum.resize(108 bits)  // Take lower 108 bits for the product

    exp := stage1.expA + stage1.expB - S(1023, 12 bits)
    sign := stage1.signA ^ stage1.signB
    isSingle := stage1.isSingleReg
    roundingMode := stage1.roundingModeReg
  }

  // Pipeline Stage 3: Normalization and Rounding
  val stage3 = new Area {
    val prod = stage2.prodReg
    val normShift = prod(107 downto 106) === U"01"
    val mantBits = Mux(normShift, prod(105 downto 0), prod(106 downto 0)).resize(54 bits)
    val expAdjust = Mux(normShift, S(-1, 12 bits), S(0, 12 bits))

    val guard = Mux(stage2.isSingle, mantBits(29), mantBits(0))
    val sticky = Mux(stage2.isSingle, mantBits(28 downto 0).orR, False)
    val roundUp = stage2.roundingMode.mux(
      RoundingMode.RNE -> (guard && (sticky || mantBits(30))),
      RoundingMode.RTZ -> False,
      RoundingMode.RUP -> ((guard || sticky) && !stage2.sign),
      RoundingMode.RDN -> ((guard || sticky) && stage2.sign)
    )
    val mantNorm = mantBits.resize(53 bits)
    val roundedMantFull = (mantNorm.resize(54 bits) + roundUp.asUInt)
    val mantOverflow = roundedMantFull(53)
    val roundedMant = roundedMantFull(52 downto 0)

    val finalExp = stage2.exp + expAdjust + mantOverflow.asSInt

    io.result.sign := stage2.sign
    io.result.exp := finalExp.asUInt.resize(11 bits)
    io.result.mant := Mux(stage2.isSingle, roundedMant(51 downto 29), roundedMant(51 downto 0)).asBits
    io.flags.NV := False
    io.flags.NX := False
    io.flags.OF := False
    io.flags.UF := False
    io.flags.DZ := False
  }
}package fpu

import spinal.core._
import spinal.core.sim._

object VcuSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new VCU).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 20)
      dut.clockDomain.waitSampling()

      // Test 1: NaN Input (FPDIV)
      dut.io.a.sign #= false
      dut.io.a.exp #= 2047
      dut.io.a.mant #= BigInt("1" + "0" * 51, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 1023
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.op #= FpuOp.FPDIV_D  // Changed to FPDIV_D
      dut.io.isSingle #= false
      dut.clockDomain.waitSampling()
      val result1 = dut.io.result.asBits.toBigInt  // Changed to asBits
      assert(result1 == BigInt("7FF8000000000000", 16), s"NaN failed: $result1")
      assert(dut.io.flags.NV.toBoolean, "NV flag not set for NaN")

      // Test 2: Inf / Zero (single)
      dut.io.a.sign #= false
      dut.io.a.exp #= 255
      dut.io.a.mant #= BigInt("0" * 52, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 0
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.isSingle #= true
      dut.clockDomain.waitSampling()
      val result2 = dut.io.result.asBits.toBigInt  // Changed to asBits
      assert(result2 == BigInt("7F80000000000000", 16), s"Inf failed: $result2")
      assert(dut.io.flags.DZ.toBoolean, "DZ flag not set for div by zero")

      println("VCU tests passed!")
    }
  }
}package fpu

import spinal.core._
import spinal.core.sim._
import spinal.lib._

object FpuSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new FPU).doSim { dut =>  // Changed Fpu to FPU
      dut.clockDomain.forkStimulus(period = 20)
      dut.clockDomain.waitSampling()

      dut.io.cmd.valid #= false
      dut.io.memDataIn #= 0
      dut.io.memAddr #= 0
      dut.io.memWrite #= false

      def executeOp(op: FpuOp.E, memData: BigInt, cycles: Int): BigInt = {
        dut.io.cmd.valid #= true
        dut.io.cmd.payload #= op
        dut.io.memDataIn #= memData
        dut.clockDomain.waitSampling()
        dut.io.cmd.valid #= false
        dut.clockDomain.waitSampling(cycles - 1)
        assert(dut.io.result.valid.toBoolean, s"$op did not complete")
        dut.io.result.payload.toBigInt  // Flow[Bits] already has payload
      }

      // Test 1: FPADD (1.0 + 2.0 = 3.0, double precision)
      executeOp(FpuOp.FPLDNLDB, BigInt("3FF0000000000000", 16), 1) // Push 1.0
      executeOp(FpuOp.FPLDNLDB, BigInt("4000000000000000", 16), 1) // Push 2.0
      val addResult = executeOp(FpuOp.FPADD_D, 0, 2)                // Add, expect 3.0
      assert(addResult == BigInt("4008000000000000", 16), s"FPADD failed: $addResult")

      // Test 2: FPMUL (2.0 * 3.0 = 6.0, single precision)
      executeOp(FpuOp.FPLDNLSN, BigInt("40000000", 16), 1)          // Push 2.0 single
      executeOp(FpuOp.FPLDNLSN, BigInt("40400000", 16), 1)          // Push 3.0 single
      val mulResult = executeOp(FpuOp.FPMUL_D, 0, 2)                // Mul, expect 6.0 (double output)
      assert(mulResult == BigInt("40C0000000000000", 16), s"FPMUL failed: $mulResult")

      // Test 3: FPDIV (4.0 / 2.0 = 2.0, double precision)
      executeOp(FpuOp.FPLDNLDB, BigInt("4010000000000000", 16), 1)  // Push 4.0
      executeOp(FpuOp.FPLDNLDB, BigInt("4000000000000000", 16), 1)  // Push 2.0
      val divResult = executeOp(FpuOp.FPDIV_D, 0, 15)               // Div, expect 2.0
      assert(divResult == BigInt("4000000000000000", 16), s"FPDIV failed: $divResult")

      // Test 4: NaN Handling (0.0 / 0.0)
      executeOp(FpuOp.FPLDZERODB, 0, 1)                            // Push 0.0
      executeOp(FpuOp.FPLDZERODB, 0, 1)                            // Push 0.0
      val nanResult = executeOp(FpuOp.FPDIV_D, 0, 15)              // Div, expect NaN
      assert(dut.io.flags.NV.toBoolean, "NV flag not set for NaN")
      assert(nanResult == BigInt("7FF8000000000000", 16), s"NaN failed: $nanResult")

      // Test 5: FPSTNLSN (Store single)
      executeOp(FpuOp.FPLDNLSN, BigInt("40400000", 16), 1)         // Push 3.0 single
      executeOp(FpuOp.FPSTNLSN, 0, 1)                             // Store
      assert(dut.io.memDataOut.toBigInt == BigInt("40400000", 16), "FPSTNLSN failed")

      println("All tests passed!")
    }
  }
}package fpu

import spinal.core._
import spinal.core.sim._

object MultiplierSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Multiplier).doSim { dut =>
      // Start the clock with a 10ns period
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling()  // Wait for initial clock edge

      // Helper function to test multiplication with cycle-by-cycle reporting
      def testMul(a: BigInt, b: BigInt, expected: BigInt, testName: String, isSingle: Boolean): Unit = {
        // Assign inputs (assuming Fp64 format: 1-bit sign, 11-bit exp, 52-bit mantissa)
        dut.io.a.sign #= (a & (1L << 63)) != 0
        dut.io.a.exp #= (a >> 52) & 0x7FF
        dut.io.a.mant #= a & 0xFFFFFFFFFFFFFL
        dut.io.b.sign #= (b & (1L << 63)) != 0
        dut.io.b.exp #= (b >> 52) & 0x7FF
        dut.io.b.mant #= b & 0xFFFFFFFFFFFFFL
        dut.io.isSingle #= isSingle
        dut.io.roundingMode #= RoundingMode.RNE  // Round to nearest, ties to even

        // Set expected cycle count based on precision
        val expectedCycles = if (isSingle) 2 else 3

        println(s"--- Starting $testName ---")
        println(s"Inputs: a = ${a.toString(16)}, b = ${b.toString(16)}, isSingle = $isSingle")
        println(s"Expected result: ${expected.toString(16)} after $expectedCycles cycles")

        // Simulate cycle-by-cycle
        for (cycle <- 1 to expectedCycles) {
          dut.clockDomain.waitSampling(1)  // Advance one clock cycle

          // Reconstruct the current output from sign, exp, and mantissa
          val sign = if (dut.io.result.sign.toBoolean) BigInt(1) else BigInt(0)
          val exp = dut.io.result.exp.toBigInt
          val mant = dut.io.result.mant.toBigInt
          val result = (sign << 63) | (exp << 52) | mant

          // Report the result for this cycle
          println(s"Cycle $cycle: result = ${result.toString(16)}")
        }

        // Check the final result after all cycles
        val finalResult = (if (dut.io.result.sign.toBoolean) BigInt(1) else BigInt(0)) << 63 |
                          (dut.io.result.exp.toBigInt << 52) |
                          dut.io.result.mant.toBigInt
        if (finalResult == expected) {
          println(s"$testName passed: ${finalResult.toString(16)}")
        } else {
          println(s"$testName failed: got ${finalResult.toString(16)}, expected ${expected.toString(16)}")
        }
        println("--- End of Test ---\n")
      }

      // Test cases
      // Test 1: 2.0 * 3.0 = 6.0 (double-precision, 3 cycles)
      testMul(
        BigInt("4000000000000000", 16),  // 2.0
        BigInt("4008000000000000", 16),  // 3.0
        BigInt("4018000000000000", 16),  // 6.0
        "Test 1: 2.0 * 3.0 (double)", false
      )

      // Test 2: -2.0 * 3.0 = -6.0 (double-precision, 3 cycles)
      testMul(
        BigInt("c000000000000000", 16),  // -2.0
        BigInt("4008000000000000", 16),  // 3.0
        BigInt("c018000000000000", 16),  // -6.0
        "Test 2: -2.0 * 3.0 (double)", false
      )

      // Test 3: 0.0 * 1.0 = 0.0 (double-precision, 3 cycles)
      testMul(
        BigInt("0000000000000000", 16),  // 0.0
        BigInt("3ff0000000000000", 16),  // 1.0
        BigInt("0000000000000000", 16),  // 0.0
        "Test 3: 0.0 * 1.0 (double)", false
      )

      // Test 4: Infinity * 1.0 = Infinity (double-precision, 3 cycles)
      testMul(
        BigInt("7ff0000000000000", 16),  // Infinity
        BigInt("3ff0000000000000", 16),  // 1.0
        BigInt("7ff0000000000000", 16),  // Infinity
        "Test 4: Infinity * 1.0 (double)", false
      )

      // Test 5: NaN * 1.0 = NaN (double-precision, 3 cycles)
      val a5 = BigInt("7ff8000000000000", 16)  // NaN
      val b5 = BigInt("3ff0000000000000", 16)  // 1.0
      dut.io.a.sign #= (a5 & (1L << 63)) != 0
      dut.io.a.exp #= (a5 >> 52) & 0x7FF
      dut.io.a.mant #= a5 & 0xFFFFFFFFFFFFFL
      dut.io.b.sign #= (b5 & (1L << 63)) != 0
      dut.io.b.exp #= (b5 >> 52) & 0x7FF
      dut.io.b.mant #= b5 & 0xFFFFFFFFFFFFFL
      dut.io.isSingle #= false
      dut.io.roundingMode #= RoundingMode.RNE
      println("--- Starting Test 5: NaN * 1.0 (double) ---")
      println(s"Inputs: a = ${a5.toString(16)}, b = ${b5.toString(16)}, isSingle = false")
      for (cycle <- 1 to 3) {
        dut.clockDomain.waitSampling(1)
        val result = (if (dut.io.result.sign.toBoolean) BigInt(1) else BigInt(0)) << 63 |
                     (dut.io.result.exp.toBigInt << 52) |
                     dut.io.result.mant.toBigInt
        println(s"Cycle $cycle: result = ${result.toString(16)}")
      }
      val finalResult = (if (dut.io.result.sign.toBoolean) BigInt(1) else BigInt(0)) << 63 |
                        (dut.io.result.exp.toBigInt << 52) |
                        dut.io.result.mant.toBigInt
      val isNaN = (finalResult & BigInt("7ff0000000000000", 16)) == BigInt("7ff0000000000000", 16) &&
                  (finalResult & BigInt("000fffffffffffff", 16)) != 0
      println(if (isNaN) "Test 5: NaN * 1.0 passed" else s"Test 5: NaN * 1.0 failed: got ${finalResult.toString(16)}")
      println("--- End of Test ---\n")

      // Test 6: 2.0f * 3.0f = 6.0f (single-precision, 2 cycles)
      testMul(
        BigInt("4000000000000000", 16),  // 2.0 (treated as single-precision)
        BigInt("4040000000000000", 16),  // 3.0 (treated as single-precision)
        BigInt("40c0000000000000", 16),  // 6.0 (treated as single-precision)
        "Test 6: 2.0f * 3.0f (single)", true
      )

      println("Simulation complete!")
    }
  }
}package fpu

import spinal.core._
import spinal.core.sim._

object DividerRooterSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new DividerRooter).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 20)
      dut.clockDomain.waitSampling()

      // Test 1: Div 4.0 / 2.0 (double)
      dut.io.a.sign #= false
      dut.io.a.exp #= 1025
      dut.io.a.mant #= BigInt("0" * 52, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 1024
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.isDiv #= true
      dut.io.isSingle #= false
      dut.io.roundingMode #= RoundingMode.RNE
      dut.clockDomain.waitSampling(15)
      val result1 = dut.io.result.asBits.toBigInt  // Changed to asBits
      assert(result1 == BigInt("4000000000000000", 16), s"Div double failed: $result1")

      // Test 2: Sqrt 16.0 (single)
      dut.io.a.sign #= false
      dut.io.a.exp #= 130
      dut.io.a.mant #= BigInt("0" * 52, 2)
      dut.io.isDiv #= false
      dut.io.isSingle #= true
      dut.clockDomain.waitSampling(7)
      val result2 = dut.io.result.asBits.toBigInt  // Changed to asBits
      assert(result2 == BigInt("4080000000000000", 16), s"Sqrt single failed: $result2")

      println("DividerRooter tests passed!")
    }
  }
}package fpu

import spinal.core._
import spinal.core.sim._

object DualAdderSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new DualAdder).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 20)
      dut.clockDomain.waitSampling()

      // Test 1: Add 1.0 + 2.0 (double)
      dut.io.a.sign #= false
      dut.io.a.exp #= 1023
      dut.io.a.mant #= BigInt("0" * 52, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 1024
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.isSub #= false
      dut.io.roundingMode #= RoundingMode.RNE
      dut.io.isSingle #= false
      dut.clockDomain.waitSampling(2)
      val result1 = dut.io.result.asBits.toBigInt  // Changed to asBits
      assert(result1 == BigInt("4008000000000000", 16), s"Add failed: $result1")

      // Test 2: Sub 3.0 - 1.0 (single)
      dut.io.a.sign #= false
      dut.io.a.exp #= 128
      dut.io.a.mant #= BigInt("0" * 52, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 126
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.isSub #= true
      dut.io.isSingle #= true
      dut.clockDomain.waitSampling(2)
      val result2 = dut.io.result.asBits.toBigInt  // Changed to asBits
      assert(result2 == BigInt("4000000000000000", 16), s"Sub failed: $result2")

      println("DualAdder tests passed!")
    }
  }
}