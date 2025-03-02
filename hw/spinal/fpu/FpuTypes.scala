package fpu

import spinal.core._
import spinal.lib._

case class FloatData(config: FPUConfig) extends Bundle {
  val sign = Bool()
  val exponent = UInt(config.exponentWidth bits)
  val mantissa = UInt(config.mantissaWidth bits)

  override def asBits: Bits = Cat(sign, exponent, mantissa)
  def fromBits(bits: Bits): FloatData = {
    val result = FloatData(config)
    result.sign := bits(config.totalWidth - 1)
    result.exponent := bits(config.totalWidth - 2, config.mantissaWidth bits).asUInt
    result.mantissa := bits(config.mantissaWidth - 1, 0 bits).asUInt
    result
  }
  def isZero: Bool = exponent === 0 && mantissa === 0
  def isDenormal: Bool = exponent === 0 && mantissa =/= 0
  def isInfinity: Bool = exponent === U(config.maxExponent) && mantissa === 0
  def isNaN: Bool = exponent === U(config.maxExponent) && mantissa =/= 0
  def isSignalingNaN: Bool = isNaN && !mantissa(config.mantissaWidth - 1)
  def isFinite: Bool = !isInfinity && !isNaN
  def normalizedMantissa: UInt = Cat(B"1", mantissa).asUInt
}

case class FPUStatus() extends Bundle {
  val roundingMode = UInt(2 bits)
  val typeFPAreg, typeFPBreg, typeFPCreg = UInt(2 bits)
  val reserved = Bits(24 bits)
  val overflow, underflow, inexact, invalid, divideByZero = Bool()
  val trapEnableOverflow, trapEnableUnderflow, trapEnableInexact, trapEnableInvalid, trapEnableDivideByZero = Bool()
  val unalign, accessViolation = Bool()

  override def clearAll(): this.type = {
    roundingMode := 1
    typeFPAreg := 0
    typeFPBreg := 0
    typeFPCreg := 0
    reserved := B(0, 24 bits)
    overflow := False
    underflow := False
    inexact := False
    invalid := False
    divideByZero := False
    unalign := False
    accessViolation := False
    trapEnableOverflow := False
    trapEnableUnderflow := False
    trapEnableInexact := False
    trapEnableInvalid := False
    trapEnableDivideByZero := False
    this
  }
}

trait FpuExecutionPlugin extends FiberPlugin {
  val config: FPUConfig
  val pipeline: Pipeline

  val io = new Bundle {
    val microInst = in(MicrocodeInstruction())
    val active = out Bool() // Signals this plugin is handling the operation
  }

  def registerOperations(ops: Map[String, FpuOperation.E]): Unit = {
    FpuDatabase.updateCustomOps(this.getName(), ops)
  }

  def registerMicrocode(op: FpuOperation.E, seq: Seq[MicrocodeInstruction]): Unit = {
    FpuDatabase.updateMicrocodeSequences(op, Map(op -> seq))
  }

  def connectPayload(stage: Stage): Unit // Connects plugin logic to pipeline stage
}

object FpuOperation extends SpinalEnum {
  val LDALL, LDLNSN, LDNLDB, LDNLSNI, LDNLDBI, LDZEROSN, LDZERODB, LDNLADDSN, LDNLADDDB, LDNLMULSN, LDNLMULDB,
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
  val srcA, srcB, dest = UInt(4 bits)
  val nextPc = UInt(6 bits)
}

class FpuIntermediateState(config: FPUConfig) extends Bundle {
  val tempA = new FloatData(config)
  val tempB = new FloatData(config)
  val partialProducts = Vec(UInt(config.mantissaWidth * 2 + 4 bits), config.mantissaWidth / 2 + 2)
  val partialSum = Vec(UInt(config.mantissaWidth * 2 + 4 bits), 2)

  def init(): FpuIntermediateState = {
    tempA.assignFromBits(B(0, config.totalWidth bits))
    tempB.assignFromBits(B(0, config.totalWidth bits))
    partialProducts.foreach(_ := 0)
    partialSum.foreach(_ := 0)
    this
  }
}

trait FpuIntermediateStateExtension[T <: FpuIntermediateState] {
  def extend(config: FPUConfig): T
}

case class DefaultFpuIntermediateState(config: FPUConfig) extends FpuIntermediateState(config)

object DefaultFpuIntermediateState extends FpuIntermediateStateExtension[DefaultFpuIntermediateState] {
  def extend(config: FPUConfig): DefaultFpuIntermediateState = new DefaultFpuIntermediateState(config)
}

case class TrapInterface() extends Bundle {
  val trapEnable = Bool() // Indicates whether a trap is enabled
  val trapCause = UInt(8 bits) // 8-bit field to encode the cause of the trap (e.g., unalign, access violation)
}

case class FpuPayload[T <: FpuIntermediateState](config: FPUConfig, intermediateFactory: FPUConfig => T) extends Bundle {
  val FA, FB, FC = new FloatData(config)
  val opcode = FpuOperation()
  val microPc = UInt(6 bits)
  val result = new FloatData(config)
  val status = new FPUStatus()
  val intermediate = intermediateFactory(config)
}