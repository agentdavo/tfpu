package fpu

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._

class AdderSubtractorPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  val logic = during setup new Area {
    println("AdderSubtractorPlugin setup: Starting")
    FpuDatabase.updatesComplete()
    awaitBuild()
    println("AdderSubtractorPlugin setup: Completed")

    val io = new Bundle {
      val result = out(new FloatData(config))
      val outStatus = out(new FPUStatus())
      val active = out Bool()
    }

    // Connect to execute1 (stage 2)
    val execStage = pipeline.stages(2)
    io.result := execStage(FpuGlobal.RESULT)
    io.outStatus := execStage(FpuGlobal.STATUS)

    io.active := False
    when(execStage.isValid && execStage(FpuGlobal.MICRO_PC) =/= 0 && io.microInst.op === MicrocodeOp.ADD) {
      io.active := True

      val expDiff = (execStage(FpuGlobal.FA).exponent - execStage(FpuGlobal.FB).exponent).asSInt
      val largerExp = Mux(expDiff >= 0, execStage(FpuGlobal.FA).exponent, execStage(FpuGlobal.FB).exponent)
      val mantAExt = Cat(B"1", execStage(FpuGlobal.FA).mantissa, U"3'b000").asUInt
      val mantBExt = Cat(B"1", execStage(FpuGlobal.FB).mantissa, U"3'b000").asUInt
      val isSubtract = execStage(FpuGlobal.OPCODE) === FpuOperation.SUB
      val alignedMantB1 = mantBExt >> expDiff.abs
      val alignedMantB2 = mantBExt >> (expDiff.abs + 1)

      val sumNoOverflow = (mantAExt.asSInt + Mux(isSubtract, -alignedMantB1.asSInt, alignedMantB1.asSInt)).asUInt
      val sumWithOverflow = (mantAExt.asSInt + Mux(isSubtract, -alignedMantB2.asSInt, alignedMantB2.asSInt)).asUInt >> 1
      val useOverflow = sumNoOverflow >= (2 << config.mantissaWidth)
      val rawMantissa = Mux(useOverflow, sumWithOverflow, sumNoOverflow)

      io.result.sign := execStage(FpuGlobal.FA).sign ^ execStage(FpuGlobal.FB).sign ^ isSubtract
      io.result.exponent := largerExp + Mux(useOverflow, U(1), U(0))
      io.result.mantissa := rawMantissa(config.mantissaWidth + 2 downto 3)

      io.outStatus.overflow := io.result.exponent >= config.maxExponent
      io.outStatus.underflow := io.result.exponent <= 0
      io.outStatus.inexact := rawMantissa(2 downto 0) =/= 0

      execStage(FpuGlobal.RESULT) := io.result
      execStage(FpuGlobal.STATUS) := io.outStatus
      when(io.microInst.nextPc =/= 0) { execStage.haltIt() }
    }

    def connectPayload(stage: Stage): Unit = {
      stage(FpuGlobal.MICRO_PC) := io.microInst.nextPc
    }

    registerOperations(Map(
      "ADD" -> FpuOperation.ADD, "SUB" -> FpuOperation.SUB,
      "LDNLADDSN" -> FpuOperation.LDNLADDSN, "LDNLADDDB" -> FpuOperation.LDNLADDDB
    ))
    val addSeq = if (config.isSinglePrecision)
      Seq(FpuDatabase.instr(MicrocodeOp.ADD, 1, 2, 1, 1), FpuDatabase.instr(MicrocodeOp.NORM, 1, 0, 1, 0))
    else
      Seq(FpuDatabase.instr(MicrocodeOp.ADD, 1, 2, 1, 2), FpuDatabase.instr(MicrocodeOp.NORM, 1, 0, 1, 3), FpuDatabase.instr(MicrocodeOp.ROUND, 1, 0, 1, 0))
    val subSeq = addSeq
    val ldnlAddSnSeq = Seq(FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 4, 1), FpuDatabase.instr(MicrocodeOp.ADD, 1, 4, 1, 2), FpuDatabase.instr(MicrocodeOp.NORM, 1, 0, 1, 0))
    val ldnlAddDbSeq = Seq(FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 4, 2), FpuDatabase.instr(MicrocodeOp.ADD, 1, 4, 1, 3), FpuDatabase.instr(MicrocodeOp.NORM, 1, 0, 1, 4), FpuDatabase.instr(MicrocodeOp.ROUND, 1, 0, 1, 0))
    registerMicrocode(FpuOperation.ADD, Map(
      FpuOperation.ADD -> addSeq, FpuOperation.SUB -> subSeq,
      FpuOperation.LDNLADDSN -> ldnlAddSnSeq, FpuOperation.LDNLADDDB -> ldnlAddDbSeq
    ))
  }
}