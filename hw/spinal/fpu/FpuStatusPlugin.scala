package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber._

class StatusPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  override def build(): Unit = during setup {
    val io = new Bundle {
      val faIn = in(new FloatData(config))
      val statusIn = in(new FPUStatus())
      val tempA = out(new FloatData(config))
      val statusOut = out(new FPUStatus())
      val active = out Bool()
    }
    this.io = io

    val execStage = pipeline.stages(2)
    io.faIn := execStage(FpuGlobal.FA)
    io.statusIn := execStage(FpuGlobal.STATUS)

    io.active := False
    when(execStage.isValid && execStage(FpuGlobal.MICRO_PC) =/= 0 && io.microInst.op =/= MicrocodeOp.NONE) {
      io.active := True

      io.tempA := io.faIn
      io.statusOut := io.statusIn

      switch(io.microInst.op) {
        is(MicrocodeOp.SETROUND) { io.statusOut.roundingMode := io.microInst.srcA }
        is(MicrocodeOp.CHECKERR) { when(io.statusIn.invalid && io.statusIn.trapEnableInvalid) { io.statusOut.invalid := True } }
        is(MicrocodeOp.TESTERR) { io.tempA := io.faIn.fromBits(B(io.statusIn.invalid, 64 bits)); io.statusOut.invalid := False }
        is(MicrocodeOp.SETERR) { io.statusOut.invalid := True }
        is(MicrocodeOp.CLRERR) { io.statusOut.assignFrom(io.statusOut.clearAll()) }
      }

      execStage(FpuGlobal.TempA) := io.tempA
      execStage(FpuGlobal.STATUS) := io.statusOut
      when(io.microInst.nextPc =/= 0) { execStage.haltIt() }
    }

    def connectPayload(stage: Node): Unit = {
      stage(FpuGlobal.MICRO_PC) := io.microInst.nextPc
    }

    registerOperations(Map(
      "RN" -> FpuOperation.RN, "RZ" -> FpuOperation.RZ,
      "RP" -> FpuOperation.RP, "RM" -> FpuOperation.RM,
      "CHKERR" -> FpuOperation.CHKERR, "TESTERR" -> FpuOperation.TESTERR,
      "SETERR" -> FpuOperation.SETERR, "CLRERR" -> FpuOperation.CLRERR
    ))
    registerMicrocode(FpuOperation.RN, Seq(FpuDatabase.instr(MicrocodeOp.SETROUND, 0, 0, 0, 0)))
    registerMicrocode(FpuOperation.RZ, Seq(FpuDatabase.instr(MicrocodeOp.SETROUND, 1, 0, 0, 0)))
    registerMicrocode(FpuOperation.RP, Seq(FpuDatabase.instr(MicrocodeOp.SETROUND, 2, 0, 0, 0)))
    registerMicrocode(FpuOperation.RM, Seq(FpuDatabase.instr(MicrocodeOp.SETROUND, 3, 0, 0, 0)))
    registerMicrocode(FpuOperation.CHKERR, Seq(FpuDatabase.instr(MicrocodeOp.CHECKERR, 0, 0, 0, 0)))
    registerMicrocode(FpuOperation.TESTERR, Seq(FpuDatabase.instr(MicrocodeOp.TESTERR, 0, 0, 0, 0)))
    registerMicrocode(FpuOperation.SETERR, Seq(FpuDatabase.instr(MicrocodeOp.SETERR, 0, 0, 0, 0)))
    registerMicrocode(FpuOperation.CLRERR, Seq(FpuDatabase.instr(MicrocodeOp.CLRERR, 0, 0, 0, 0)))
    awaitBuild()
  }
}