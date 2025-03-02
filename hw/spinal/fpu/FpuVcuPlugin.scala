package fpu

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._

class VCUPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  val logic = during setup new Area {
    println("VCUPlugin setup: Starting")
    FpuDatabase.updatesComplete()
    awaitBuild()
    println("VCUPlugin setup: Completed")

    val io = new Bundle {
      val result = out(new FloatData(config))
      val abort = out Bool()
      val outStatus = out(new FPUStatus())
      val active = out Bool()
    }

    // Connect to decode stage (stage 1) for special case handling
    val decodeStage = pipeline.stages(1)
    io.result := decodeStage(FpuGlobal.RESULT)
    io.outStatus := decodeStage(FpuGlobal.STATUS)

    io.active := False
    when(decodeStage.isValid && decodeStage(FpuGlobal.MICRO_PC) =/= 0) {
      io.active := True

      // Utility functions for special cases
      def isSignalingNaN(f: FloatData): Bool = f.isNaN && !f.mantissa(config.mantissaWidth - 2) // Quiet bit unset
      def isQuietNaN(f: FloatData): Bool = f.isNaN && f.mantissa(config.mantissaWidth - 2)
      def isZero(f: FloatData): Bool = f.exponent === 0 && f.mantissa === 0
      def isInfinity(f: FloatData): Bool = f.exponent === config.maxExponent && f.mantissa === 0

      val floatA = decodeStage(FpuGlobal.FA)
      val floatB = decodeStage(FpuGlobal.FB)
      val opcode = decodeStage(FpuGlobal.OPCODE)

      // Special case checks (ISM)
      val aSigNaN = isSignalingNaN(floatA)
      val bSigNaN = isSignalingNaN(floatB)
      val aQuietNaN = isQuietNaN(floatA)
      val bQuietNaN = isQuietNaN(floatB)
      val aZero = isZero(floatA)
      val bZero = isZero(floatB)
      val aInf = isInfinity(floatA)
      val bInf = isInfinity(floatB)
      val aDenormal = floatA.isDenormal
      val bDenormal = floatB.isDenormal

      // Abort only on critical special cases (not denormals)
      io.abort := aSigNaN || bSigNaN || aQuietNaN || bQuietNaN || aInf || bInf

      // Result computation (special cases)
      io.result := floatA
      when(aSigNaN) {
        io.result := floatA.fromBits(floatA.asBits | (B"1" << (config.mantissaWidth - 2))) // Quiet signaling NaN
      }.elsewhen(bSigNaN) {
        io.result := floatB.fromBits(floatB.asBits | (B"1" << (config.mantissaWidth - 2)))
      }.elsewhen(aQuietNaN) {
        io.result := floatA
      }.elsewhen(bQuietNaN) {
        io.result := floatB
      }.elsewhen((aZero && bInf) || (aInf && bZero)) {
        io.result := floatA.fromBits(B"0x7FF8000000000000") // ZeroMulInfNaN
      }.elsewhen(aInf && bInf) {
        when(opcode === FpuOperation.DIV) {
          io.result := floatA.fromBits(B"0x7FFC000000000000") // Inf / Inf NaN
        }.elsewhen(opcode === FpuOperation.SUB && floatA.sign =/= floatB.sign) {
          io.result := floatA.fromBits(B"0x7FF8000000000000") // Inf - Inf NaN
        }
      }.elsewhen(aDenormal || bDenormal) {
        // Pass denormals to execution stages for normalization
        io.result := floatA // Let NormalizerPlugin handle denormals
      }

      // Status update (error signals)
      val status = new FPUStatus()
      status.assignFrom(decodeStage(FpuGlobal.STATUS)) // Propagate input status
      status.invalid := aSigNaN || bSigNaN || aQuietNaN || bQuietNaN ||
                        ((aZero && bInf) || (aInf && bZero)) ||
                        (aInf && bInf && (opcode === FpuOperation.DIV ||
                                          (opcode === FpuOperation.SUB && floatA.sign =/= floatB.sign)))
      status.divideByZero := floatA.isFinite && bZero && opcode === FpuOperation.DIV
      decodeStage(FpuGlobal.RESULT) := io.result
      decodeStage(FpuGlobal.STATUS) := status
      when(io.abort || io.microInst.nextPc =/= 0) { decodeStage.throwIt() }
    }

    def connectPayload(stage: Stage): Unit = {
      stage(FpuGlobal.MICRO_PC) := io.microInst.nextPc
    }

    registerOperations(Map.empty) // VCU handles special cases, no direct ops
    registerMicrocode(FpuOperation.NONE, Seq(FpuDatabase.instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0)))
  }
}