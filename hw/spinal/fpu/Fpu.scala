package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._

// Configuration
case class FPUConfig(precision: Int = 64) {
  val isSinglePrecision = precision == 32
  val mantissaWidth = if (isSinglePrecision) 23 else 52
  val exponentWidth = if (isSinglePrecision) 8 else 11
  val totalWidth = 1 + exponentWidth + mantissaWidth
  val bias = if (isSinglePrecision) 127 else 1023
  val maxExponent = (1 << exponentWidth) - 1
  val minExponent = -bias + 1
  val clockFreqMHz = 50
}

// IEEE-754 Float Data
case class FloatData(config: FPUConfig) extends Bundle {
  val sign = Bool()
  val exponent = UInt(config.exponentWidth bits)
  val mantissa = UInt(config.mantissaWidth bits)

  def asBits: Bits = Cat(sign, exponent, mantissa)
  def fromBits(bits: Bits): FloatData = {
    val result = FloatData(config)
    result.sign := bits(config.totalWidth - 1)
    result.exponent := bits(config.totalWidth - 2 downto config.mantissaWidth)
    result.mantissa := bits(config.mantissaWidth - 1 downto 0)
    result
  }
  def isZero: Bool = exponent === 0 && mantissa === 0
  def isDenormal: Bool = exponent === 0 && mantissa =/= 0
  def isInfinity: Bool = exponent === U(config.maxExponent) && mantissa === 0
  def isNaN: Bool = exponent === U(config.maxExponent) && mantissa =/= 0
  def isSignalingNaN: Bool = isNaN && !mantissa(config.mantissaWidth - 1)
  def isFinite: Bool = !isInfinity && !isNaN
  def normalizedMantissa: UInt = Cat(True, mantissa)
}

// FPU Status
case class FPUStatus() extends Bundle {
  val roundingMode = UInt(2 bits)  // 0=Zero, 1=Nearest, 2=+Inf, 3=-Inf
  val typeFPAreg, typeFPBreg, typeFPCreg = UInt(2 bits)  // 0=Single, 1=Double
  val reserved = Bits(24 bits)
  val overflow, underflow, inexact, invalid, divideByZero = Bool()
  val trapEnableOverflow, trapEnableUnderflow, trapEnableInexact, trapEnableInvalid, trapEnableDivideByZero = Bool()
  val unalign, accessViolation = Bool()

  def clearAll(): Unit = {
    roundingMode := 1; typeFPAreg := 0; typeFPBreg := 0; typeFPCreg := 0
    reserved := 0; overflow := False; underflow := False; inexact := False
    invalid := False; divideByZero := False; unalign := False; accessViolation := False
    trapEnableOverflow := False; trapEnableUnderflow := False; trapEnableInexact := False
    trapEnableInvalid := False; trapEnableDivideByZero := False
  }
}

// Enums
object FpuOperation extends SpinalEnum {
  val LDLNSN, LDNLDB, LDNLSNI, LDNLDBI, LDZEROSN, LDZERODB, LDNLADDSN, LDNLADDDB, LDNLMULSN, LDNLMULDB,
  STNLSN, STNLDB, STNLI32, STALL, ENTRY, REV, DUP, RN, RZ, RP, RM, CHKERR, TESTERR, SETERR, CLRERR, GT, EQ,
  ORDERED, NAN, NOTFINITE, CHKI32, CHKI64, R32TOR64, R64TOR32, RTOI32, I32TOR32, I32TOR64, B32TOR64,
  NOROUND, INT, ADD, SUB, MUL, DIV, ABS, EXPINC32, EXPDEC32, MULBY2, DIVBY2, SQRTFIRST, SQRTSTEP,
  SQRTLAST, REMFIRST, REMSTEP, REM, SQRT, RANGE, GE, LG, NONE = newElement()
}

object MicrocodeOp extends SpinalEnum {
  val LOAD, STORE, ADD, SUB, MUL, DIV, SQRT, NORM, ROUND, FINALIZE, CONVERT, EXPINC32, REM, DUP, REV,
  SETROUND, CHECKERR, TESTERR, SETERR, CLRERR, CMPGT, CMPEQ, CMPORDER, CMPNAN, CMPNOTFINITE, CHKI,
  ABS, MULBY2, DIVBY2, R32TOR64, R64TOR32, NOROUND, INTOP = newElement()
}

case class MicrocodeInstruction() extends Bundle {
  val op = MicrocodeOp()
  val srcA, srcB, dest = UInt(4 bits)  // 0=unused, 1=FA, 2=FB, 3=FC, 4=TempA, 5=TempB
  val nextPc = UInt(6 bits)
}

// Base Intermediate State (extensible by plugins)
class FpuIntermediateState(config: FPUConfig) extends Bundle {
  val tempA = new FloatData(config)  // General-purpose temp for DIV/SQRT/REM
  val tempB = new FloatData(config)  // General-purpose temp (e.g., counter or quotient)
  val partialProducts = Vec(UInt(config.mantissaWidth * 2 + 4 bits), config.mantissaWidth / 2 + 2)  // For MUL
  val partialSum = Vec(UInt(config.mantissaWidth * 2 + 4 bits), 2)  // For MUL

  def init(): this.type = {
    tempA.assignFromBits(B"0".resize(config.totalWidth))
    tempB.assignFromBits(B"0".resize(config.totalWidth))
    partialProducts.foreach(_ := 0)
    partialSum.foreach(_ := 0)
    this
  }
}

// Trait for plugins to extend intermediate state
trait FpuIntermediateStateExtension[T <: FpuIntermediateState] {
  def extend(config: FPUConfig): T
}

// Updated FpuPayload with parameterized intermediate state
case class FpuPayload[T <: FpuIntermediateState](config: FPUConfig, intermediateFactory: FPUConfig => T) extends Bundle {
  val FA, FB, FC = new FloatData(config)
  val opcode = FpuOperation()
  val microPc = UInt(6 bits)
  val result = new FloatData(config)
  val status = new FPUStatus()
  val intermediate = intermediateFactory(config)
}

// Stack Plugin
class FPUStackPlugin(config: FPUConfig) extends FiberPlugin {
  val logic = during build { new Area {
    val stack = Vec(Reg(new FloatData(config)), 3)  // FA, FB, FC
    val types = Vec(Reg(UInt(2 bits)), 3)  // 0=Single, 1=Double

    def push(data: FloatData, precision: UInt): Unit = {
      stack(1) := stack(0); stack(2) := stack(1); stack(0) := data
      types(1) := types(0); types(2) := types(1); types(0) := precision
    }

    def pop(): Unit = {
      stack(0) := stack(1); stack(1) := stack(2)
      types(0) := types(1); types(1) := types(2)
    }
  }}
}

// VCU Plugin
class VCUPlugin(config: FPUConfig) extends FiberPlugin {
  val logic = during build { new Area {
    val io = new Bundle {
      val operandA, operandB = in(new FloatData(config))
      val opcode = in(FpuOperation())
      val result = out(new FloatData(config))
      val abort = out Bool()
      val status = out(FPUStatus())
    }

    val status = new FPUStatus()
    status.clearAll()
    val floatA = io.operandA
    val floatB = io.operandB
    io.abort := floatA.isNaN || floatB.isNaN || floatA.isInfinity || floatB.isInfinity || floatA.isDenormal || floatB.isDenormal

    io.result := MuxCase(floatA, Seq(
      (floatA.isSignalingNaN) -> floatA.fromBits(floatA.asBits | (B"1" << config.mantissaWidth - 1)),
      (floatB.isSignalingNaN) -> floatB.fromBits(floatB.asBits | (B"1" << config.mantissaWidth - 1)),
      (floatA.isNaN) -> floatA,
      (floatB.isNaN) -> floatB,
      (floatA.isInfinity && floatB.isInfinity && io.opcode === FpuOperation.DIV) -> floatA.fromBits(B"0x7FFC000000000000"),
      (floatA.isInfinity && floatB.isInfinity && io.opcode === FpuOperation.SUB && floatA.sign =/= floatB.sign) -> floatA.fromBits(B"0x7FF8000000000000")
    ))
    status.invalid := floatA.isNaN || floatB.isNaN || (floatA.isInfinity && floatB.isInfinity && io.opcode === FpuOperation.SUB && floatA.sign =/= floatB.sign)
    status.divideByZero := floatA.isFinite && floatB.isZero && io.opcode === FpuOperation.DIV
    io.status := status
  }}
}

// Adder/Subtractor Plugin (Single-Stage)
class AdderSubtractorPlugin(config: FPUConfig) extends FiberPlugin {
  val logic = during build { new Area {
    val io = new Bundle {
      val opA, opB = in(new FloatData(config))
      val isSubtract = in Bool()
      val status = in(FPUStatus())
      val result = out(new FloatData(config))
      val outStatus = out(FPUStatus())
    }

    val expDiff = (io.opA.exponent - io.opB.exponent).asSInt
    val largerExp = Mux(expDiff >= 0, io.opA.exponent, io.opB.exponent)
    val mantAExt = Cat(True, io.opA.mantissa, U"3'b000").asUInt
    val mantBExt = Cat(True, io.opB.mantissa, U"3'b000").asUInt
    val alignedMantB1 = mantBExt >> expDiff.abs
    val alignedMantB2 = mantBExt >> (expDiff.abs + 1)

    val sumNoOverflow = (mantAExt.asSInt + (if (io.isSubtract) -alignedMantB1.asSInt else alignedMantB1.asSInt)).asUInt
    val sumWithOverflow = (mantAExt.asSInt + (if (io.isSubtract) -alignedMantB2.asSInt else alignedMantB2.asSInt)).asUInt >> 1
    val useOverflow = sumNoOverflow >= (2 << config.mantissaWidth)
    val rawMantissa = Mux(useOverflow, sumWithOverflow, sumNoOverflow)

    io.result.sign := io.opA.sign ^ io.opB.sign ^ io.isSubtract
    io.result.exponent := largerExp + (if (useOverflow) 1 else 0)
    io.result.mantissa := rawMantissa(config.mantissaWidth + 2 downto 3)

    io.outStatus := io.status
    io.outStatus.overflow := io.result.exponent >= config.maxExponent
    io.outStatus.underflow := io.result.exponent <= 0
    io.outStatus.inexact := rawMantissa(2 downto 0) =/= 0
  }}
}

// Multiplier Plugin (Retimed)
class MultiplierPlugin[T <: FpuIntermediateState](config: FPUConfig) extends FiberPlugin {
  val logic = during build { new Area {
    val io = new Bundle {
      val opA, opB = in(new FloatData(config))
      val status = in(FPUStatus())
      val result = out(new FloatData(config))
      val outStatus = out(FPUStatus())
      val partialProducts = out(Vec(UInt(config.mantissaWidth * 2 + 4 bits), config.mantissaWidth / 2 + 2))
      val sign = out Bool()
      val exp = out(UInt(config.exponentWidth bits))
      val partialSum = out(Vec(UInt(config.mantissaWidth * 2 + 4 bits), 2))
      val partialProductsIn = in(Vec(UInt(config.mantissaWidth * 2 + 4 bits), config.mantissaWidth / 2 + 2))
      val signIn = in Bool()
      val expIn = in(UInt(config.exponentWidth bits))
      val partialSumIn = in(Vec(UInt(config.mantissaWidth * 2 + 4 bits), 2))
      val stage = in UInt(2 bits)
    }

    def csa(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
      val sum = a ^ b ^ c
      val carry = (a & b) | (b & c) | (a & c)
      (sum, carry << 1)
    }

    when(io.stage === 0) {
      val mantAExt = Cat(True, io.opA.mantissa).asUInt
      val mantBExt = Cat(True, io.opB.mantissa).asUInt
      val boothDigits = (0 until config.mantissaWidth / 2 + 2).map { i =>
        val bits = Cat(
          if (2 * i + 1 < config.mantissaWidth + 1) mantBExt(2 * i + 1) else False,
          if (2 * i < config.mantissaWidth + 1) mantBExt(2 * i) else False,
          if (i == 0) False else mantBExt(2 * i - 1)
        )
        MuxCase(S"2'b00", Seq(
          (bits === U"3'b000") -> S"2'b00",
          (bits === U"3'b001") -> S"2'b01",
          (bits === U"3'b010") -> S"2'b01",
          (bits === U"3'b011") -> S"2'b10",
          (bits === U"3'b100") -> S"-2'b10",
          (bits === U"3'b101") -> S"-2'b01",
          (bits === U"3'b110") -> S"-2'b01",
          (bits === U"3'b111") -> S"2'b00"
        )).asSInt
      }

      io.partialProducts.zipWithIndex.foreach { case (pp, i) =>
        val digit = boothDigits(i)
        pp := MuxCase(U"0", Seq(
          (digit === 0) -> U"0",
          (digit === 1) -> mantAExt,
          (digit === 2) -> (mantAExt << 1),
          (digit === -1) -> (-mantAExt),
          (digit === -2) -> (-(mantAExt << 1))
        )).asUInt << (2 * i)
        pp.resize(config.mantissaWidth * 2 + 4)
      }
      io.sign := io.opA.sign ^ io.opB.sign
      io.exp := io.opA.exponent + io.opB.exponent - config.bias
      io.partialSum(0) := 0
      io.partialSum(1) := 0
      io.result := io.opA
    } elsewhen(io.stage === 1) {
      val grouped = io.partialProductsIn.grouped(3).toSeq.map {
        case Seq(a, b, c) => csa(a, b, c)
        case Seq(a, b) => (a + b, U"0")
        case Seq(a) => (a, U"0")
      }
      io.partialSum(0) := grouped.map(_._1).reduce(_ +_)
      io.partialSum(1) := grouped.map(_._2).reduce(_ +_)
      io.sign := io.signIn
      io.exp := io.expIn
      io.partialProducts := io.partialProductsIn
      io.result := io.opA
    } otherwise {
      val product = io.partialSumIn(0) + io.partialSumIn(1)
      val normalizedProduct = product(config.mantissaWidth * 2 + 2 downto config.mantissaWidth)
      val expResult = io.expIn + (if (product(config.mantissaWidth * 2 + 3)) 1 else 0)

      io.result.sign := io.signIn
      io.result.exponent := expResult
      io.result.mantissa := normalizedProduct(config.mantissaWidth - 1 downto 0)
      io.outStatus := io.status
      io.outStatus.overflow := expResult >= config.maxExponent
      io.outStatus.underflow := expResult <= 0
      io.outStatus.inexact := product(config.mantissaWidth - 1 downto 0) =/= 0
      io.partialProducts := io.partialProductsIn
      io.partialSum := io.partialSumIn
      io.sign := io.signIn
      io.exp := io.expIn
    }

    io.outStatus := io.status
  }}
}

// Divider/Square Root Plugin (Retimed)
class DividerSqrtPlugin[T <: FpuIntermediateState](config: FPUConfig) extends FiberPlugin {
  val logic = during build { new Area {
    val io = new Bundle {
      val dividend, divisor = in(new FloatData(config))
      val isSquareRoot = in Bool()
      val status = in(FPUStatus())
      val result = out(new FloatData(config))
      val outStatus = out(FPUStatus())
      val partialRemainder = out(SInt(config.mantissaWidth + 4 bits))
      val quotient = out(UInt(config.mantissaWidth + 4 bits))
      val counter = out(UInt(log2Up(config.mantissaWidth + 1) bits))
      val partialRemainderIn = in(SInt(config.mantissaWidth + 4 bits))
      val quotientIn = in(UInt(config.mantissaWidth + 4 bits))
      val counterIn = in(UInt(log2Up(config.mantissaWidth + 1) bits))
      val finalize = in Bool()
    }

    val extWidth = config.mantissaWidth + 4
    val partialRemainder = Reg(SInt(extWidth bits))
    val quotient = Reg(UInt(extWidth bits))
    val counter = Reg(UInt(log2Up(config.mantissaWidth + 1) bits))
    val dividendMant = Cat(True, io.dividend.mantissa).asUInt
    val divisorMant = Cat(True, io.divisor.mantissa).asUInt

    when(io.finalize) {
      partialRemainder := io.partialRemainderIn
      quotient := io.quotientIn
      counter := io.counterIn
      io.result.sign := io.dividend.sign ^ (if (io.isSquareRoot) False else io.divisor.sign)
      io.result.exponent := Mux(io.isSquareRoot,
        (io.dividend.exponent + config.bias) >> 1,
        io.dividend.exponent - io.divisor.exponent + config.bias)
      io.result.mantissa := quotient(config.mantissaWidth + 2 downto 3)
    } otherwise {
      when(counter === 0) {
        partialRemainder := (if (io.isSquareRoot) S(dividendMant) else S(dividendMant) << (config.mantissaWidth + 1)).resize(extWidth)
        quotient := 0
      } elsewhen(counter < (if (io.isSquareRoot) config.mantissaWidth / 2 else config.mantissaWidth)) {
        val rShifted = partialRemainder << 2
        val qLast = quotient(counter - 1 downto counter - 2)
        val trialDivisor = Mux(io.isSquareRoot, Cat(U"2'b01", quotient.resize(config.mantissaWidth), U"2'b00").asUInt.resize(extWidth), divisorMant.resize(extWidth))
        val trialSub = rShifted - (if (io.isSquareRoot) Cat(qLast, U"2'b11").asSInt else S(trialDivisor))
        val qDigit = Mux(rShifted >= 0 && trialSub >= 0, S"2'b01", Mux(rShifted < 0 && trialSub < 0, S"2'b11", S"2'b00")).asSInt
        partialRemainder := rShifted - (qDigit.abs.asUInt * trialDivisor)
        quotient := (quotient << 2) | qDigit.abs
      }
      counter := counter + 1
      io.result := io.dividend
    }

    io.partialRemainder := partialRemainder
    io.quotient := quotient
    io.counter := counter
    io.outStatus := io.status
    io.outStatus.divideByZero := io.divisor.isZero && !io.isSquareRoot
    io.outStatus.inexact := quotient(2 downto 0) =/= 0
  }}
}

// Microcode ROM Plugin
class MicrocodeROMPlugin(config: FPUConfig) extends FiberPlugin {
  val logic = during build { new Area {
    val io = new Bundle {
      val operation = in(FpuOperation())
      val address = in UInt(6 bits)
      val instruction = out(MicrocodeInstruction())
    }
    val rom = Mem(MicrocodeInstruction(), 128)
    rom.initialContent = MicrocodeGenerator.generateMicrocode(config, io.operation).map(_.asBits.as(MicrocodeInstruction()))
    io.instruction := rom.readSync(io.address)
  }}

  object MicrocodeGenerator {
    def generateMicrocode(config: FPUConfig, operation: FpuOperation): Seq[Bits] = {
      def instr(op: MicrocodeOp.E, srcA: Int, srcB: Int, dest: Int, next: Int): Bits =
        U(op.id ## B(srcA, 4 bits) ## B(srcB, 4 bits) ## B(dest, 4 bits) ## B(next, 6 bits), 32 bits)

      val base = Seq(instr(MicrocodeOp.LOAD, 1, 0, 0, 1))
      operation match {
        case FpuOperation.LDLNSN => base ++ Seq(instr(MicrocodeOp.LOAD, 0, 0, 1, 0))
        case FpuOperation.LDNLDB => base ++ Seq(instr(MicrocodeOp.LOAD, 0, 0, 1, 0))
        case FpuOperation.LDNLSNI => base ++ Seq(instr(MicrocodeOp.LOAD, 0, 0, 1, 0))
        case FpuOperation.LDNLDBI => base ++ Seq(instr(MicrocodeOp.LOAD, 0, 0, 1, 0))
        case FpuOperation.LDZEROSN => base ++ Seq(instr(MicrocodeOp.LOAD, 0, 0, 1, 0))
        case FpuOperation.LDZERODB => base ++ Seq(instr(MicrocodeOp.LOAD, 0, 0, 1, 0))
        case FpuOperation.LDNLADDSN => base ++ Seq(
          instr(MicrocodeOp.LOAD, 0, 0, 4, 2),
          instr(MicrocodeOp.ADD, 1, 4, 1, 3),
          instr(MicrocodeOp.NORM, 1, 0, 1, 4),
          instr(MicrocodeOp.ROUND, 1, 0, 1, 0)
        )
        case FpuOperation.LDNLADDDB => base ++ Seq(
          instr(MicrocodeOp.LOAD, 0, 0, 4, 2),
          instr(MicrocodeOp.ADD, 1, 4, 1, 3),
          instr(MicrocodeOp.NORM, 1, 0, 1, 4),
          instr(MicrocodeOp.ROUND, 1, 0, 1, 0)
        )
        case FpuOperation.LDNLMULSN => base ++ Seq(
          instr(MicrocodeOp.LOAD, 0, 0, 4, 2),
          instr(MicrocodeOp.MUL, 1, 4, 1, 3),
          instr(MicrocodeOp.NORM, 1, 0, 1, if (config.isSinglePrecision) 0 else 4),
          instr(MicrocodeOp.ROUND, 1, 0, 1, 0)
        )
        case FpuOperation.LDNLMULDB => base ++ Seq(
          instr(MicrocodeOp.LOAD, 0, 0, 4, 2),
          instr(MicrocodeOp.MUL, 1, 4, 1, 3),
          instr(MicrocodeOp.NORM, 1, 0, 1, 4),
          instr(MicrocodeOp.ROUND, 1, 0, 1, 0)
        )
        case FpuOperation.STNLSN => base ++ Seq(instr(MicrocodeOp.STORE, 1, 0, 0, 0))
        case FpuOperation.STNLDB => base ++ Seq(instr(MicrocodeOp.STORE, 1, 0, 0, 0))
        case FpuOperation.STNLI32 => base ++ Seq(
          instr(MicrocodeOp.CONVERT, 1, 0, 4, 2),
          instr(MicrocodeOp.STORE, 4, 0, 0, 0)
        )
        case FpuOperation.STALL => base ++ Seq(
          instr(MicrocodeOp.STORE, 0, 0, 0, 1),
          instr(MicrocodeOp.STORE, 1, 0, 0, 2),
          instr(MicrocodeOp.STORE, 1, 0, 0, 3),
          instr(MicrocodeOp.STORE, 2, 0, 0, 4),
          instr(MicrocodeOp.STORE, 2, 0, 0, 5),
          instr(MicrocodeOp.STORE, 3, 0, 0, 6),
          instr(MicrocodeOp.STORE, 3, 0, 0, 0)
        )
        case FpuOperation.ENTRY => base ++ Seq(instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0))
        case FpuOperation.REV => base ++ Seq(instr(MicrocodeOp.REV, 1, 2, 0, 0))
        case FpuOperation.DUP => base ++ Seq(instr(MicrocodeOp.DUP, 1, 0, 0, 0))
        case FpuOperation.RN => base ++ Seq(instr(MicrocodeOp.SETROUND, 1, 0, 0, 0))
        case FpuOperation.RZ => base ++ Seq(instr(MicrocodeOp.SETROUND, 0, 0, 0, 0))
        case FpuOperation.RP => base ++ Seq(instr(MicrocodeOp.SETROUND, 2, 0, 0, 0))
        case FpuOperation.RM => base ++ Seq(instr(MicrocodeOp.SETROUND, 3, 0, 0, 0))
        case FpuOperation.CHKERR => base ++ Seq(instr(MicrocodeOp.CHECKERR, 0, 0, 0, 0))
        case FpuOperation.TESTERR => base ++ Seq(instr(MicrocodeOp.TESTERR, 0, 0, 0, 0))
        case FpuOperation.SETERR => base ++ Seq(instr(MicrocodeOp.SETERR, 0, 0, 0, 0))
        case FpuOperation.CLRERR => base ++ Seq(instr(MicrocodeOp.CLRERR, 0, 0, 0, 0))
        case FpuOperation.GT => base ++ Seq(instr(MicrocodeOp.CMPGT, 1, 2, 4, 0))
        case FpuOperation.EQ => base ++ Seq(instr(MicrocodeOp.CMPEQ, 1, 2, 4, 0))
        case FpuOperation.ORDERED => base ++ Seq(instr(MicrocodeOp.CMPORDER, 1, 2, 4, 0))
        case FpuOperation.NAN => base ++ Seq(instr(MicrocodeOp.CMPNAN, 1, 0, 4, 0))
        case FpuOperation.NOTFINITE => base ++ Seq(instr(MicrocodeOp.CMPNOTFINITE, 1, 0, 4, 0))
        case FpuOperation.CHKI32 => base ++ Seq(instr(MicrocodeOp.CHKI, 1, 0, 4, 0))
        case FpuOperation.CHKI64 => base ++ Seq(instr(MicrocodeOp.CHKI, 1, 0, 4, 0))
        case FpuOperation.R32TOR64 => base ++ Seq(instr(MicrocodeOp.R32TOR64, 1, 0, 1, 0))
        case FpuOperation.R64TOR32 => base ++ Seq(
          instr(MicrocodeOp.R64TOR32, 1, 0, 1, 2),
          instr(MicrocodeOp.ROUND, 1, 0, 1, 0)
        )
        case FpuOperation.RTOI32 => base ++ Seq(instr(MicrocodeOp.CONVERT, 1, 0, 1, 0))
        case FpuOperation.I32TOR32 => base ++ Seq(instr(MicrocodeOp.CONVERT, 1, 0, 1, 0))
        case FpuOperation.I32TOR64 => base ++ Seq(instr(MicrocodeOp.CONVERT, 1, 0, 1, 0))
        case FpuOperation.B32TOR64 => base ++ Seq(instr(MicrocodeOp.CONVERT, 1, 0, 1, 0))
        case FpuOperation.NOROUND => base ++ Seq(instr(MicrocodeOp.NOROUND, 1, 0, 1, 0))
        case FpuOperation.INT => base ++ Seq(instr(MicrocodeOp.INTOP, 1, 0, 1, 0))
        case FpuOperation.ADD => base ++ Seq(
          instr(MicrocodeOp.ADD, 1, 2, 1, 2),
          instr(MicrocodeOp.NORM, 1, 0, 1, 3),
          instr(MicrocodeOp.ROUND, 1, 0, 1, 0)
        )
        case FpuOperation.SUB => base ++ Seq(
          instr(MicrocodeOp.SUB, 2, 1, 1, 2),
          instr(MicrocodeOp.NORM, 1, 0, 1, 3),
          instr(MicrocodeOp.ROUND, 1, 0, 1, 0)
        )
        case FpuOperation.MUL => base ++ Seq(
          instr(MicrocodeOp.MUL, 1, 2, 1, 2),
          instr(MicrocodeOp.NORM, 1, 0, 1, if (config.isSinglePrecision) 0 else 3),
          instr(MicrocodeOp.ROUND, 1, 0, 1, 0)
        )
        case FpuOperation.DIV => base ++ Seq(
          instr(MicrocodeOp.DIV, 1, 2, 1, 2),
          instr(MicrocodeOp.NORM, 1, 0, 1, 3),
          instr(MicrocodeOp.ROUND, 1, 0, 1, 0)
        )
        case FpuOperation.ABS => base ++ Seq(instr(MicrocodeOp.ABS, 1, 0, 1, 0))
        case FpuOperation.EXPINC32 => base ++ Seq(instr(MicrocodeOp.EXPINC32, 1, 0, 1, 0))
        case FpuOperation.EXPDEC32 => base ++ Seq(instr(MicrocodeOp.EXPDEC32, 1, 0, 1, 0))
        case FpuOperation.MULBY2 => base ++ Seq(instr(MicrocodeOp.MULBY2, 1, 0, 1, 0))
        case FpuOperation.DIVBY2 => base ++ Seq(instr(MicrocodeOp.DIVBY2, 1, 0, 1, 0))
        case FpuOperation.SQRTFIRST => base ++ Seq(instr(MicrocodeOp.SQRT, 1, 0, 1, 0))
        case FpuOperation.SQRTSTEP => base ++ Seq(instr(MicrocodeOp.SQRT, 1, 0, 1, 0))
        case FpuOperation.SQRTLAST => base ++ Seq(instr(MicrocodeOp.SQRT, 1, 0, 1, 0))
        case FpuOperation.REMFIRST => base ++ Seq(instr(MicrocodeOp.REM, 1, 2, 1, 0))
        case FpuOperation.REMSTEP => base ++ Seq(instr(MicrocodeOp.REM, 1, 2, 1, 0))
        case FpuOperation.REM => base ++ Seq(
          instr(MicrocodeOp.REM, 1, 2, 1, 2),
          instr(MicrocodeOp.NORM, 1, 0, 1, 3),
          instr(MicrocodeOp.ROUND, 1, 0, 1, 0)
        )
        case FpuOperation.SQRT => base ++ Seq(
          instr(MicrocodeOp.SQRT, 1, 0, 1, 2),
          instr(MicrocodeOp.NORM, 1, 0, 1, 3),
          instr(MicrocodeOp.ROUND, 1, 0, 1, 0)
        )
        case FpuOperation.RANGE => base ++ Seq(instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0))
        case FpuOperation.GE => base ++ Seq(instr(MicrocodeOp.CMPGT, 1, 2, 4, 0))
        case FpuOperation.LG => base ++ Seq(instr(MicrocodeOp.CMPGT, 2, 1, 4, 0))
        case _ => base ++ Seq(instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0))
      }
    }
  }
}

// Top-Level FPU (Part 1: Setup and Initial Stages)
class T9000Fpu[T <: FpuIntermediateState](config: FPUConfig, intermediateFactory: FPUConfig => T = (c: FPUConfig) => new FpuIntermediateState(c)) extends PluginHost {
  val io = new Bundle {
    val inputA, inputB, inputC = in Bits(config.totalWidth bits)
    val operation = in(FpuOperation())
    val memAddress = in UInt(32 bits)
    val result = out Bits(config.totalWidth bits)
    val ready = out Bool()
    val specialResult = out Bits(config.totalWidth bits)
  }

  val status = Reg(new FPUStatus()).init(FPUStatus().clearAll())
  val stackPlugin = addService(new FPUStackPlugin(config))
  val vcuPlugin = addService(new VCUPlugin(config))
  val adderSubPlugin = addService(new AdderSubtractorPlugin(config))
  val multiplierPlugin = addService(new MultiplierPlugin[T](config))
  val dividerSqrtPlugin = addService(new DividerSqrtPlugin[T](config))
  val microRomPlugin = addService(new MicrocodeROMPlugin(config))

  val logic = during build { new Area {
    val mem = Mem(Bits(32 bits), 1024)
    val pipeline = new StageCtrlPipeline()
    pipeline.build()

    val FETCH = new pipeline.Ctrl(0) {
      val PAYLOAD = insert(FpuPayload(config, intermediateFactory))
      PAYLOAD.FA := stackPlugin.logic.stack(0).fromBits(io.inputA)
      PAYLOAD.FB := stackPlugin.logic.stack(1).fromBits(io.inputB)
      PAYLOAD.FC := stackPlugin.logic.stack(2).fromBits(io.inputC)
      PAYLOAD.opcode := io.operation
      PAYLOAD.microPc := 0
      PAYLOAD.status := status
      PAYLOAD.intermediate.init()
    }

    val DECODE = new pipeline.Ctrl(1) {
      vcuPlugin.logic.io.operandA := FETCH.PAYLOAD.FA
      vcuPlugin.logic.io.operandB := FETCH.PAYLOAD.FB
      vcuPlugin.logic.io.opcode := FETCH.PAYLOAD.opcode
      when(vcuPlugin.logic.io.abort) {
        FETCH.PAYLOAD.result := vcuPlugin.logic.io.result
        FETCH.PAYLOAD.status := vcuPlugin.logic.io.status
        throwIt()
      }
    }

    val MICROCODE = new pipeline.Ctrl(2) {
      microRomPlugin.logic.io.operation := FETCH.PAYLOAD.opcode
      microRomPlugin.logic.io.address := FETCH.PAYLOAD.microPc
      val microInst = microRomPlugin.logic.io.instruction

      FETCH.PAYLOAD.status.unalign := io.memAddress(1 downto 0) =/= 0

      dividerSqrtPlugin.logic.io.dividend := FETCH.PAYLOAD.FA
      dividerSqrtPlugin.logic.io.divisor := FETCH.PAYLOAD.FB
      dividerSqrtPlugin.logic.io.isSquareRoot := FETCH.PAYLOAD.opcode === FpuOperation.SQRT
      dividerSqrtPlugin.logic.io.status := FETCH.PAYLOAD.status
      dividerSqrtPlugin.logic.io.finalize := False
      dividerSqrtPlugin.logic.io.partialRemainderIn := 0
      dividerSqrtPlugin.logic.io.quotientIn := 0
      dividerSqrtPlugin.logic.io.counterIn := 0

      switch(microInst.op) {
        is(MicrocodeOp.LOAD) {
          when(FETCH.PAYLOAD.opcode === FpuOperation.LDLNSN || FETCH.PAYLOAD.opcode === FpuOperation.LDNLSNI) {
            FETCH.PAYLOAD.FA := FloatData(config).fromBits(mem.readSync(io.memAddress).resize(32))
            stackPlugin.logic.push(FETCH.PAYLOAD.FA, 0)
          }
          when(FETCH.PAYLOAD.opcode === FpuOperation.LDNLDB || FETCH.PAYLOAD.opcode === FpuOperation.LDNLDBI) {
            FETCH.PAYLOAD.FA := FloatData(config).fromBits(Cat(mem.readSync(io.memAddress + 1), mem.readSync(io.memAddress)))
            stackPlugin.logic.push(FETCH.PAYLOAD.FA, 1)
          }
          when(FETCH.PAYLOAD.opcode === FpuOperation.LDZEROSN) {
            FETCH.PAYLOAD.FA := FloatData(config).fromBits(B"0".resize(32))
            stackPlugin.logic.push(FETCH.PAYLOAD.FA, 0)
          }
          when(FETCH.PAYLOAD.opcode === FpuOperation.LDZERODB) {
            FETCH.PAYLOAD.FA := FloatData(config).fromBits(B"0".resize(64))
            stackPlugin.logic.push(FETCH.PAYLOAD.FA, 1)
          }
          when(FETCH.PAYLOAD.opcode === FpuOperation.LDNLADDSN || FETCH.PAYLOAD.opcode === FpuOperation.LDNLMULSN) {
            FETCH.PAYLOAD.intermediate.tempA := FloatData(config).fromBits(mem.readSync(io.memAddress).resize(32))
          }
          when(FETCH.PAYLOAD.opcode === FpuOperation.LDNLADDDB || FETCH.PAYLOAD.opcode === FpuOperation.LDNLMULDB) {
            FETCH.PAYLOAD.intermediate.tempA := FloatData(config).fromBits(Cat(mem.readSync(io.memAddress + 1), mem.readSync(io.memAddress)))
          }
        }
        is(MicrocodeOp.STORE) {
          when(FETCH.PAYLOAD.opcode === FpuOperation.STALL) {
            switch(FETCH.PAYLOAD.microPc) {
              is(1) { mem.write(io.memAddress, Cat(FETCH.PAYLOAD.status.reserved, FETCH.PAYLOAD.status.typeFPCreg, FETCH.PAYLOAD.status.typeFPBreg, FETCH.PAYLOAD.status.typeFPAreg, FETCH.PAYLOAD.status.roundingMode)) }
              is(2) { mem.write(io.memAddress + 1, FETCH.PAYLOAD.FA.asBits(31 downto 0)) }
              is(3) { when(FETCH.PAYLOAD.status.typeFPAreg === 1) { mem.write(io.memAddress + 2, FETCH.PAYLOAD.FA.asBits(63 downto 32)) } }
              is(4) { mem.write(io.memAddress + (if (FETCH.PAYLOAD.status.typeFPAreg === 0) 2 else 3), FETCH.PAYLOAD.FB.asBits(31 downto 0)) }
              is(5) { when(FETCH.PAYLOAD.status.typeFPBreg === 1) { mem.write(io.memAddress + (if (FETCH.PAYLOAD.status.typeFPAreg === 0) 3 else 4), FETCH.PAYLOAD.FB.asBits(63 downto 32)) } }
              is(6) { mem.write(io.memAddress + (if (FETCH.PAYLOAD.status.typeFPAreg === 0 && FETCH.PAYLOAD.status.typeFPBreg === 0) 3 else if (FETCH.PAYLOAD.status.typeFPAreg === 1 && FETCH.PAYLOAD.status.typeFPBreg === 1) 5 else 4), FETCH.PAYLOAD.FC.asBits(31 downto 0)) }
              is(0) { when(FETCH.PAYLOAD.status.typeFPCreg === 1) { mem.write(io.memAddress + (if (FETCH.PAYLOAD.status.typeFPAreg === 0 && FETCH.PAYLOAD.status.typeFPBreg === 0) 4 else if (FETCH.PAYLOAD.status.typeFPAreg === 1 && FETCH.PAYLOAD.status.typeFPBreg === 1) 6 else 5), FETCH.PAYLOAD.FC.asBits(63 downto 32)) } }
            }
          }
          when(FETCH.PAYLOAD.opcode === FpuOperation.STNLDB) {
            mem.write(io.memAddress, FETCH.PAYLOAD.FA.asBits(31 downto 0))
            mem.write(io.memAddress + 1, FETCH.PAYLOAD.FA.asBits(63 downto 32))
            stackPlugin.logic.pop()
          }
          when(FETCH.PAYLOAD.opcode === FpuOperation.STNLSN) {
            mem.write(io.memAddress, FETCH.PAYLOAD.FA.asBits(31 downto 0))
            stackPlugin.logic.pop()
          }
          when(FETCH.PAYLOAD.opcode === FpuOperation.STNLI32) {
            mem.write(io.memAddress, FETCH.PAYLOAD.intermediate.tempA.asBits(31 downto 0))
            stackPlugin.logic.pop()
          }
        }
        is(MicrocodeOp.ADD) {
          adderSubPlugin.logic.io.opA := Mux(microInst.srcA === 1, FETCH.PAYLOAD.FA, FETCH.PAYLOAD.intermediate.tempA)
          adderSubPlugin.logic.io.opB := FETCH.PAYLOAD.FB
          adderSubPlugin.logic.io.isSubtract := False
          adderSubPlugin.logic.io.status := FETCH.PAYLOAD.status
          FETCH.PAYLOAD.result := adderSubPlugin.logic.io.result
          FETCH.PAYLOAD.status := adderSubPlugin.logic.io.outStatus
          when(config.isSinglePrecision && FETCH.PAYLOAD.microPc < 1) { haltIt() }  // 2 cycles
          when(!config.isSinglePrecision && FETCH.PAYLOAD.microPc < 2) { haltIt() }  // 3 cycles
        }
        is(MicrocodeOp.SUB) {
          adderSubPlugin.logic.io.opA := FETCH.PAYLOAD.FB
          adderSubPlugin.logic.io.opB := FETCH.PAYLOAD.FA
          adderSubPlugin.logic.io.isSubtract := True
          adderSubPlugin.logic.io.status := FETCH.PAYLOAD.status
          FETCH.PAYLOAD.result := adderSubPlugin.logic.io.result
          FETCH.PAYLOAD.status := adderSubPlugin.logic.io.outStatus
          stackPlugin.logic.pop()
          when(config.isSinglePrecision && FETCH.PAYLOAD.microPc < 1) { haltIt() }  // 2 cycles
          when(!config.isSinglePrecision && FETCH.PAYLOAD.microPc < 2) { haltIt() }  // 3 cycles
        }
        is(MicrocodeOp.MUL) {
          multiplierPlugin.logic.io.opA := Mux(microInst.srcA === 1, FETCH.PAYLOAD.FA, FETCH.PAYLOAD.intermediate.tempA)
          multiplierPlugin.logic.io.opB := FETCH.PAYLOAD.FB
          multiplierPlugin.logic.io.status := FETCH.PAYLOAD.status
          multiplierPlugin.logic.io.stage := 0
          multiplierPlugin.logic.io.partialProductsIn := Vec.fill(config.mantissaWidth / 2 + 2)(U"0")
          multiplierPlugin.logic.io.signIn := False
          multiplierPlugin.logic.io.expIn := 0
          multiplierPlugin.logic.io.partialSumIn := Vec.fill(2)(U"0")
          FETCH.PAYLOAD.intermediate.partialProducts := multiplierPlugin.logic.io.partialProducts
          FETCH.PAYLOAD.result.sign := multiplierPlugin.logic.io.sign
          FETCH.PAYLOAD.result.exponent := multiplierPlugin.logic.io.exp
          FETCH.PAYLOAD.intermediate.partialSum := multiplierPlugin.logic.io.partialSum
          when(config.isSinglePrecision && FETCH.PAYLOAD.microPc < 1) { haltIt() }  // 2 cycles
          when(!config.isSinglePrecision && FETCH.PAYLOAD.microPc < 2) { haltIt() }  // 3 cycles
        }
        is(MicrocodeOp.DIV, MicrocodeOp.SQRT) {
          FETCH.PAYLOAD.result := dividerSqrtPlugin.logic.io.result
          FETCH.PAYLOAD.status := dividerSqrtPlugin.logic.io.outStatus
          FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(dividerSqrtPlugin.logic.io.partialRemainder.asBits.resize(config.totalWidth))
          FETCH.PAYLOAD.intermediate.tempB := FETCH.PAYLOAD.FA.fromBits(dividerSqrtPlugin.logic.io.quotient.asBits.resize(config.totalWidth))
        }
        is(MicrocodeOp.CONVERT) {
          when(FETCH.PAYLOAD.opcode === FpuOperation.STNLI32 || FETCH.PAYLOAD.opcode === FpuOperation.RTOI32) {
            val mantissaShifted = S(FETCH.PAYLOAD.FA.normalizedMantissa) << (FETCH.PAYLOAD.FA.exponent.toSInt - config.bias)
            val int64 = Mux(FETCH.PAYLOAD.FA.sign, -mantissaShifted, mantissaShifted)
            FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(int64.asBits.resize(config.totalWidth))
          }
          when(FETCH.PAYLOAD.opcode === FpuOperation.I32TOR32 || FETCH.PAYLOAD.opcode === FpuOperation.I32TOR64) {
            val int32 = S(FETCH.PAYLOAD.FA.asBits(31 downto 0))
            val absInt32 = Mux(int32 < 0, -int32, int32)
            val shift = countLeadingZeros(absInt32.asUInt)
            FETCH.PAYLOAD.result.sign := int32 < 0
            FETCH.PAYLOAD.result.exponent := U(31 - shift + config.bias, config.exponentWidth bits)
            FETCH.PAYLOAD.result.mantissa := (absInt32.asUInt << shift)(config.mantissaWidth - 1 downto 0)
          }
          when(FETCH.PAYLOAD.opcode === FpuOperation.R32TOR64 || FETCH.PAYLOAD.opcode === FpuOperation.B32TOR64) {
            FETCH.PAYLOAD.result := FloatData(config).fromBits(Cat(FETCH.PAYLOAD.FA.asBits(31 downto 0), B"0".resize(29)))
          }
        }
        is(MicrocodeOp.DUP) { stackPlugin.logic.push(FETCH.PAYLOAD.FA, FETCH.PAYLOAD.status.typeFPAreg) }
        is(MicrocodeOp.REV) {
          val temp = FETCH.PAYLOAD.FA
          FETCH.PAYLOAD.FA := FETCH.PAYLOAD.FB
          FETCH.PAYLOAD.FB := temp
          val tempType = FETCH.PAYLOAD.status.typeFPAreg
          FETCH.PAYLOAD.status.typeFPAreg := FETCH.PAYLOAD.status.typeFPBreg
          FETCH.PAYLOAD.status.typeFPBreg := tempType
        }
        is(MicrocodeOp.SETROUND) { FETCH.PAYLOAD.status.roundingMode := microInst.srcA.asUInt }
        is(MicrocodeOp.CHECKERR) { when(FETCH.PAYLOAD.status.invalid && FETCH.PAYLOAD.status.trapEnableInvalid) { FETCH.PAYLOAD.status.invalid := True } }
        is(MicrocodeOp.TESTERR) { FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(B(FETCH.PAYLOAD.status.invalid, 64 bits)); FETCH.PAYLOAD.status.invalid := False }
        is(MicrocodeOp.SETERR) { FETCH.PAYLOAD.status.invalid := True }
        is(MicrocodeOp.CLRERR) { FETCH.PAYLOAD.status.clearAll() }
        is(MicrocodeOp.CMPGT) {
          val diff = adderSubPlugin.logic.io.opA.exponent - adderSubPlugin.logic.io.opB.exponent
          val mantDiff = S(FETCH.PAYLOAD.FA.normalizedMantissa) - S(FETCH.PAYLOAD.FB.normalizedMantissa)
          val gt = (FETCH.PAYLOAD.FA.sign === FETCH.PAYLOAD.FB.sign) ? (diff > 0 || (diff === 0 && mantDiff > 0)) | (FETCH.PAYLOAD.FA.sign === False && FETCH.PAYLOAD.FB.sign === True)
          FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(B(gt, 64 bits))
        }
        is(MicrocodeOp.CMPEQ) { FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(B(FETCH.PAYLOAD.FA.asBits === FETCH.PAYLOAD.FB.asBits, 64 bits)) }
        is(MicrocodeOp.CMPORDER) { FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(B(!FETCH.PAYLOAD.FA.isNaN && !FETCH.PAYLOAD.FB.isNaN, 64 bits)) }
        is(MicrocodeOp.CMPNAN) { FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(B(FETCH.PAYLOAD.FA.isNaN, 64 bits)) }
        is(MicrocodeOp.CMPNOTFINITE) { FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(B(!FETCH.PAYLOAD.FA.isFinite, 64 bits)) }
        is(MicrocodeOp.CHKI) {
          val intVal = S(FETCH.PAYLOAD.FA.normalizedMantissa) << (FETCH.PAYLOAD.FA.exponent.toSInt - config.bias)
          val inRange = if (FETCH.PAYLOAD.opcode === FpuOperation.CHKI32) intVal >= S(-2147483648) && intVal <= S(2147483647) else intVal >= S(-9223372036854775808L) && intVal <= S(9223372036854775807L)
          FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(B(inRange, 64 bits))
        }
        is(MicrocodeOp.ABS) { FETCH.PAYLOAD.result := FETCH.PAYLOAD.FA; FETCH.PAYLOAD.result.sign := False }
        is(MicrocodeOp.EXPINC32) { FETCH.PAYLOAD.result := FETCH.PAYLOAD.FA; FETCH.PAYLOAD.result.exponent := FETCH.PAYLOAD.FA.exponent + 32; FETCH.PAYLOAD.status.overflow := FETCH.PAYLOAD.result.exponent >= config.maxExponent }
        is(MicrocodeOp.EXPDEC32) { FETCH.PAYLOAD.result := FETCH.PAYLOAD.FA; FETCH.PAYLOAD.result.exponent := FETCH.PAYLOAD.FA.exponent - 32; FETCH.PAYLOAD.status.underflow := FETCH.PAYLOAD.result.exponent <= 0 }
        is(MicrocodeOp.MULBY2) { FETCH.PAYLOAD.result := FETCH.PAYLOAD.FA; FETCH.PAYLOAD.result.exponent := FETCH.PAYLOAD.FA.exponent + 1; FETCH.PAYLOAD.status.overflow := FETCH.PAYLOAD.result.exponent >= config.maxExponent }
        is(MicrocodeOp.DIVBY2) { FETCH.PAYLOAD.result := FETCH.PAYLOAD.FA; FETCH.PAYLOAD.result.exponent := FETCH.PAYLOAD.FA.exponent - 1; FETCH.PAYLOAD.status.underflow := FETCH.PAYLOAD.result.exponent <= 0 }
        is(MicrocodeOp.R32TOR64) { FETCH.PAYLOAD.result := FloatData(config).fromBits(Cat(FETCH.PAYLOAD.FA.asBits(31 downto 0), B"0".resize(29))) }
        is(MicrocodeOp.R64TOR32) { FETCH.PAYLOAD.result := FETCH.PAYLOAD.FA.fromBits(FETCH.PAYLOAD.FA.asBits(63 downto 29)) }
        is(MicrocodeOp.NOROUND) { FETCH.PAYLOAD.result := FETCH.PAYLOAD.FA.fromBits(FETCH.PAYLOAD.FA.asBits(63 downto 29)) }
        is(MicrocodeOp.INTOP) { 
          val intPart = S(FETCH.PAYLOAD.FA.normalizedMantissa) << (FETCH.PAYLOAD.FA.exponent.toSInt - config.bias)
          FETCH.PAYLOAD.result := FETCH.PAYLOAD.FA.fromBits(intPart.asBits.resize(config.totalWidth))
        }
        is(MicrocodeOp.REM) {
          val tempRemainder = Reg(SInt(config.totalWidth bits))
          val counter = Reg(UInt(log2Up(config.mantissaWidth + 1) bits))
          when(FETCH.PAYLOAD.microPc === 1) {
            tempRemainder := S(FETCH.PAYLOAD.FA.normalizedMantissa) << (FETCH.PAYLOAD.FA.exponent.toSInt - config.bias)
            counter := 0
          } elsewhen(counter < (if (config.isSinglePrecision) 3 else config.mantissaWidth - 2)) {
            val divisorShifted = S(FETCH.PAYLOAD.FB.normalizedMantissa) << (FETCH.PAYLOAD.FB.exponent.toSInt - config.bias)
            when(tempRemainder >= divisorShifted) { tempRemainder := tempRemainder - divisorShifted }
            counter := counter + 1
          }
          FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(tempRemainder.asBits.resize(config.totalWidth))
          FETCH.PAYLOAD.intermediate.tempB := FETCH.PAYLOAD.FA.fromBits(counter.asBits.resize(config.totalWidth))
          FETCH.PAYLOAD.result := FETCH.PAYLOAD.FA.fromBits(tempRemainder.asBits.resize(config.totalWidth))
        }
      }
      FETCH.PAYLOAD.microPc := microInst.nextPc
      when(FETCH.PAYLOAD.microPc =/= 0 && !(FETCH.PAYLOAD.opcode === FpuOperation.DIV || FETCH.PAYLOAD.opcode === FpuOperation.SQRT || FETCH.PAYLOAD.opcode === FpuOperation.REM)) { haltIt() }
      when(FETCH.PAYLOAD.opcode === FpuOperation.DIV || FETCH.PAYLOAD.opcode === FpuOperation.SQRT || FETCH.PAYLOAD.opcode === FpuOperation.REM) {
        when(config.isSinglePrecision) {
          when(FETCH.PAYLOAD.opcode === FpuOperation.DIV && dividerSqrtPlugin.logic.io.counter < 3) { haltIt() }  // 5 cycles
          when(FETCH.PAYLOAD.opcode === FpuOperation.SQRT && dividerSqrtPlugin.logic.io.counter < 6) { haltIt() }  // 8 cycles
          when(FETCH.PAYLOAD.opcode === FpuOperation.REM && dividerSqrtPlugin.logic.io.counter < 3) { haltIt() }  // 5 cycles
        } otherwise {
          when(dividerSqrtPlugin.logic.io.counter < (if (FETCH.PAYLOAD.opcode === FpuOperation.SQRT) config.mantissaWidth / 2 - 1 else config.mantissaWidth - 2)) { haltIt() }  // 15 cycles
        }
      }
    }

    val NORMALIZE = new pipeline.Ctrl(3) {
      val shift = countLeadingZeros(FETCH.PAYLOAD.result.mantissa)
      FETCH.PAYLOAD.result.mantissa := (FETCH.PAYLOAD.result.mantissa << shift)(config.mantissaWidth - 1 downto 0)
      FETCH.PAYLOAD.result.exponent := FETCH.PAYLOAD.result.exponent - shift
      FETCH.PAYLOAD.status.underflow := FETCH.PAYLOAD.result.exponent <= 0

      dividerSqrtPlugin.logic.io.dividend := FETCH.PAYLOAD.FA
      dividerSqrtPlugin.logic.io.divisor := FETCH.PAYLOAD.FB
      dividerSqrtPlugin.logic.io.isSquareRoot := FETCH.PAYLOAD.opcode === FpuOperation.SQRT
      dividerSqrtPlugin.logic.io.status := FETCH.PAYLOAD.status
      dividerSqrtPlugin.logic.io.finalize := False
      dividerSqrtPlugin.logic.io.partialRemainderIn := FETCH.PAYLOAD.intermediate.tempA.asBits.asSInt
      dividerSqrtPlugin.logic.io.quotientIn := FETCH.PAYLOAD.intermediate.tempB.asBits.asUInt
      dividerSqrtPlugin.logic.io.counterIn := dividerSqrtPlugin.logic.io.counter

      multiplierPlugin.logic.io.opA := FETCH.PAYLOAD.FA
      multiplierPlugin.logic.io.opB := FETCH.PAYLOAD.FB
      multiplierPlugin.logic.io.status := FETCH.PAYLOAD.status
      multiplierPlugin.logic.io.stage := Mux(config.isSinglePrecision, 2, 1)
      multiplierPlugin.logic.io.partialProductsIn := FETCH.PAYLOAD.intermediate.partialProducts
      multiplierPlugin.logic.io.signIn := FETCH.PAYLOAD.result.sign
      multiplierPlugin.logic.io.expIn := FETCH.PAYLOAD.result.exponent
      multiplierPlugin.logic.io.partialSumIn := FETCH.PAYLOAD.intermediate.partialSum

      when(FETCH.PAYLOAD.opcode === FpuOperation.DIV || FETCH.PAYLOAD.opcode === FpuOperation.SQRT) {
        FETCH.PAYLOAD.result := dividerSqrtPlugin.logic.io.result
        FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(dividerSqrtPlugin.logic.io.partialRemainder.asBits.resize(config.totalWidth))
        FETCH.PAYLOAD.intermediate.tempB := FETCH.PAYLOAD.FA.fromBits(dividerSqrtPlugin.logic.io.quotient.asBits.resize(config.totalWidth))
      }
      when(FETCH.PAYLOAD.opcode === FpuOperation.REM) {
        val tempRemainder = S(FETCH.PAYLOAD.intermediate.tempA.asBits.asSInt)
        val counter = FETCH.PAYLOAD.intermediate.tempB.asBits.asUInt
        val divisorShifted = S(FETCH.PAYLOAD.FB.normalizedMantissa) << (FETCH.PAYLOAD.FB.exponent.toSInt - config.bias)
        when(counter < (if (config.isSinglePrecision) 4 else config.mantissaWidth - 1) && tempRemainder >= divisorShifted) {
          tempRemainder := tempRemainder - divisorShifted
        }
        FETCH.PAYLOAD.intermediate.tempA := FETCH.PAYLOAD.FA.fromBits(tempRemainder.asBits.resize(config.totalWidth))
        FETCH.PAYLOAD.intermediate.tempB := FETCH.PAYLOAD.FA.fromBits((counter + 1).asBits.resize(config.totalWidth))
        FETCH.PAYLOAD.result := FETCH.PAYLOAD.FA.fromBits(tempRemainder.asBits.resize(config.totalWidth))
      }
      when(FETCH.PAYLOAD.opcode === FpuOperation.MUL) {
        when(config.isSinglePrecision) {
          FETCH.PAYLOAD.result := multiplierPlugin.logic.io.result
          FETCH.PAYLOAD.status := multiplierPlugin.logic.io.outStatus
        } otherwise {
          FETCH.PAYLOAD.intermediate.partialSum := multiplierPlugin.logic.io.partialSum
          FETCH.PAYLOAD.result.sign := multiplierPlugin.logic.io.sign
          FETCH.PAYLOAD.result.exponent := multiplierPlugin.logic.io.exp
        }
      }
    }

    val ROUND = new pipeline.Ctrl(4) {
      val guard = FETCH.PAYLOAD.result.mantissa(1)
      val roundBit = FETCH.PAYLOAD.result.mantissa(0)
      val sticky = FETCH.PAYLOAD.result.mantissa(0)
      val roundUp = FETCH.PAYLOAD.status.roundingMode.mux(
        0 -> False,
        1 -> (roundBit && (guard || sticky)),
        2 -> (FETCH.PAYLOAD.result.sign && (guard || roundBit || sticky)),
        3 -> (!FETCH.PAYLOAD.result.sign && (guard || roundBit || sticky))
      )
      FETCH.PAYLOAD.result.mantissa := FETCH.PAYLOAD.result.mantissa(config.mantissaWidth - 1 downto 2) + U(roundUp)
      FETCH.PAYLOAD.status.inexact := guard || roundBit || sticky

      dividerSqrtPlugin.logic.io.dividend := FETCH.PAYLOAD.FA
      dividerSqrtPlugin.logic.io.divisor := FETCH.PAYLOAD.FB
      dividerSqrtPlugin.logic.io.isSquareRoot := FETCH.PAYLOAD.opcode === FpuOperation.SQRT
      dividerSqrtPlugin.logic.io.status := FETCH.PAYLOAD.status
      dividerSqrtPlugin.logic.io.finalize := FETCH.PAYLOAD.opcode === FpuOperation.DIV || FETCH.PAYLOAD.opcode === FpuOperation.SQRT
      dividerSqrtPlugin.logic.io.partialRemainderIn := FETCH.PAYLOAD.intermediate.tempA.asBits.asSInt
      dividerSqrtPlugin.logic.io.quotientIn := FETCH.PAYLOAD.intermediate.tempB.asBits.asUInt
      dividerSqrtPlugin.logic.io.counterIn := dividerSqrtPlugin.logic.io.counter

      multiplierPlugin.logic.io.opA := FETCH.PAYLOAD.FA
      multiplierPlugin.logic.io.opB := FETCH.PAYLOAD.FB
      multiplierPlugin.logic.io.status := FETCH.PAYLOAD.status
      multiplierPlugin.logic.io.stage := Mux(config.isSinglePrecision, 0, 2)
      multiplierPlugin.logic.io.partialProductsIn := FETCH.PAYLOAD.intermediate.partialProducts
      multiplierPlugin.logic.io.signIn := FETCH.PAYLOAD.result.sign
      multiplierPlugin.logic.io.expIn := FETCH.PAYLOAD.result.exponent
      multiplierPlugin.logic.io.partialSumIn := FETCH.PAYLOAD.intermediate.partialSum

      when(FETCH.PAYLOAD.opcode === FpuOperation.DIV || FETCH.PAYLOAD.opcode === FpuOperation.SQRT) {
        FETCH.PAYLOAD.result := dividerSqrtPlugin.logic.io.result
        FETCH.PAYLOAD.status := dividerSqrtPlugin.logic.io.outStatus
      }
      when(FETCH.PAYLOAD.opcode === FpuOperation.REM) {
        val tempRemainder = S(FETCH.PAYLOAD.intermediate.tempA.asBits.asSInt)
        val counter = FETCH.PAYLOAD.intermediate.tempB.asBits.asUInt
        val divisorShifted = S(FETCH.PAYLOAD.FB.normalizedMantissa) << (FETCH.PAYLOAD.FB.exponent.toSInt - config.bias)
        when(counter < config.mantissaWidth && tempRemainder >= divisorShifted) {
          tempRemainder := tempRemainder - divisorShifted
        }
        FETCH.PAYLOAD.result := FETCH.PAYLOAD.FA.fromBits(tempRemainder.asBits.resize(config.totalWidth))
        FETCH.PAYLOAD.status.inexact := True
      }
      when(FETCH.PAYLOAD.opcode === FpuOperation.MUL && !config.isSinglePrecision) {
        FETCH.PAYLOAD.result := multiplierPlugin.logic.io.result
        FETCH.PAYLOAD.status := multiplierPlugin.logic.io.outStatus
      }
    }

    val WRITE = new pipeline.Ctrl(5) {
      io.result := FETCH.PAYLOAD.result.asBits
      io.ready := isValid
      status := FETCH.PAYLOAD.status
    }

    io.specialResult := vcuPlugin.logic.io.result.asBits
  }}
}

// Simulation
object T9000FpuSim extends App {
  def runTests(config: FPUConfig) = SimConfig.withVerilator.compile(new T9000Fpu(config)).doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)
    var cycleCount = 0
    dut.clockDomain.onSamplings { cycleCount += 1 }

    def resetAndRun(op: FpuOperation.E, inputA: Long, inputB: Long, expected: Long, desc: String, cycles: Int): Unit = {
      dut.io.inputA #= inputA
      dut.io.inputB #= inputB
      dut.io.inputC #= 0x0000000000000000L
      dut.io.operation #= op
      dut.io.memAddress #= 0
      cycleCount = 0
      waitUntil(dut.io.ready.toBoolean)
      println(s"$desc: Result = 0x${dut.io.result.toBigInt.toString(16)}, Expected = 0x${expected.toString(16)}, Cycles = $cycleCount")
      assert(dut.io.result.toBigInt == expected, s"$desc failed!")
      assert(cycleCount == cycles, s"$desc cycle count mismatch: expected $cycles, got $cycleCount")
    }

    if (config.isSinglePrecision) {
      resetAndRun(FpuOperation.ADD, 0x3F800000, 0x3F800000, 0x40000000, "ADD 1.0 + 1.0 (single)", 2)
      resetAndRun(FpuOperation.SUB, 0x40000000, 0x3F800000, 0x3F800000, "SUB 2.0 - 1.0 (single)", 2)
      resetAndRun(FpuOperation.MUL, 0x3F800000, 0x40000000, 0x40000000, "MUL 1.0 * 2.0 (single)", 2)
      resetAndRun(FpuOperation.DIV, 0x3F800000, 0x40000000, 0x3F000000, "DIV 1.0 / 2.0 (single)", 5)
      resetAndRun(FpuOperation.SQRT, 0x40800000, 0x00000000, 0x40000000, "SQRT 4.0 (single)", 8)
      resetAndRun(FpuOperation.REM, 0x40C00000, 0x40400000, 0x40000000, "REM 6.0 % 3.0 (single)", 5)
    } else {
      resetAndRun(FpuOperation.ADD, 0x3FF0000000000000L, 0x3FF0000000000000L, 0x4000000000000000L, "ADD 1.0 + 1.0 (double)", 3)
      resetAndRun(FpuOperation.SUB, 0x4000000000000000L, 0x3FF0000000000000L, 0x3FF0000000000000L, "SUB 2.0 - 1.0 (double)", 3)
      resetAndRun(FpuOperation.MUL, 0x3FF0000000000000L, 0x4000000000000000L, 0x4008000000000000L, "MUL 1.0 * 2.0 (double)", 3)
      resetAndRun(FpuOperation.DIV, 0x3FF0000000000000L, 0x4000000000000000L, 0x3FE0000000000000L, "DIV 1.0 / 2.0 (double)", 15)
      resetAndRun(FpuOperation.SQRT, 0x4010000000000000L, 0x0000000000000000L, 0x4000000000000000L, "SQRT 4.0 (double)", 15)
      resetAndRun(FpuOperation.REM, 0x4018000000000000L, 0x4008000000000000L, 0x3FF0000000000000L, "REM 6.0 % 3.0 (double)", 15)
    }
  }

  runTests(FPUConfig(32))  // Single precision
  runTests(FPUConfig(64))  // Double precision
}