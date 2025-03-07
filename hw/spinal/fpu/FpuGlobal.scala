package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline.Payload
import spinal.core.fiber.Database.blocking

object FpuGlobal extends AreaRoot {
  val MANTISSA_WIDTH = blocking[Int]
  val EXPONENT_WIDTH = blocking[Int]
  val TOTAL_WIDTH = blocking[Int]
  val BIAS = blocking[Int]
  val MAX_EXPONENT = blocking[Int]
  val CLOCK_FREQ_MHZ = blocking[Int]
  val IS_SINGLE_PRECISION = blocking[Boolean]

  val FA = Payload(FloatData(FPUConfig(64)))
  val FB = Payload(FloatData(FPUConfig(64)))
  val FC = Payload(FloatData(FPUConfig(64)))
  val TempA = Payload(FloatData(FPUConfig(64)))
  val TempB = Payload(FloatData(FPUConfig(64)))
  val OPCODE = Payload(FpuOperation())
  val MICRO_PC = Payload(UInt(6 bits))
  val RESULT = Payload(FloatData(FPUConfig(64)))
  val STATUS = Payload(FPUStatus())
  val INTERMEDIATE = Payload(FpuIntermediateState(FPUConfig(64)))
  val MEM_ADDRESS = Payload(UInt(32 bits))

  def configSetup(config: FPUConfig): Unit = {
    MANTISSA_WIDTH.set(config.mantissaWidth)
    EXPONENT_WIDTH.set(config.exponentWidth)
    TOTAL_WIDTH.set(config.totalWidth)
    BIAS.set(config.bias)
    MAX_EXPONENT.set(config.maxExponent)
    CLOCK_FREQ_MHZ.set(config.clockFreqMHz)
    IS_SINGLE_PRECISION.set(config.isSinglePrecision)
    FA.payloadType = FloatData(config)
    FB.payloadType = FloatData(config)
    FC.payloadType = FloatData(config)
    TempA.payloadType = FloatData(config)
    TempB.payloadType = FloatData(config)
    RESULT.payloadType = FloatData(config)
    INTERMEDIATE.payloadType = FpuIntermediateState(config)
  }
}