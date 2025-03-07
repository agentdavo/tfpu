package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber._

class VCUPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  override def build(): Unit = during setup {
    val io = new Bundle {
      val result = out(new FloatData(config))
      val abort = out Bool()
      val outStatus = out(new FPUStatus())
      val active = out Bool()
    }
    this.io = io

    val decodeStage = pipeline.stages(1)
    io.result := decodeStage(FpuGlobal.RESULT)
    io.outStatus := decodeStage(FpuGlobal.STATUS)

    io.active := False
    when(decodeStage.isValid && decodeStage(FpuGlobal.MICRO_PC) =/= 0) {
      io.active := True

      def isSignalingNaN(f: FloatData): Bool = f.isNaN && !f.mantissa(config.mantissaWidth - 2)
      def isQuietNaN(f: FloatData): Bool = f.isNaN && f.mantissa(config.mantissaWidth - 2)
      def isZero(f: FloatData): Bool = f.exponent === 0 && f.mantissa === 0
      def isInfinity(f: FloatData): Bool = f.exponent === config.maxExponent && f.mantissa === 0

      val floatA = decodeStage(FpuGlobal.FA)
      val floatB = decodeStage(FpuGlobal.FB)
      val opcode = decodeStage(FpuGlobal.OPCODE)

      val aSigNaN = isSignalingNaN(floatA)
      val bSigNaN = isSignalingNaN(floatB)
      val aQuietNaN = isQuietNaN(floatA)
      val bQuietNaN = isQuietNaN(floatB)
      val aZero = isZero(floatA)
      val bZero = isZero(floatB)
      val aInf = isInfinity(floatA)
      val bInf = isInfinity(floatB)

      io.abort := aSigNaN || bSigNaN || aQuietNaN || bQuietNaN || aInf || bInf

      io.result := floatA
      when(aSigNaN) {
        io.result := floatA.fromBits(floatA.asBits | (B"1" << (config.mantissaWidth - 2)))
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
      }

      val status = new FPUStatus()
      status.assignFrom(decodeStage(FpuGlobal.STATUS))
      status.invalid := aSigNaN || bSigNaN || aQuietNaN || bQuietNaN ||
                        ((aZero && bInf) || (aInf && bZero)) ||
                        (aInf && bInf && (opcode === FpuOperation.DIV ||
                                          (opcode === FpuOperation.SUB && floatA.sign =/= floatB.sign)))
      status.divideByZero := floatA.isFinite && bZero && opcode === FpuOperation.DIV
      decodeStage(FpuGlobal.RESULT) := io.result
      decodeStage(FpuGlobal.STATUS) := status
      when(io.abort || io.microInst.nextPc =/= 0) { decodeStage.throwIt() }
    }

    def connectPayload(stage: Node): Unit = {
      stage(FpuGlobal.MICRO_PC) := io.microInst.nextPc
    }

    registerOperations(Map.empty)
    registerMicrocode(FpuOperation.NONE, Seq(FpuDatabase.instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0)))
    awaitBuild()
  }
}