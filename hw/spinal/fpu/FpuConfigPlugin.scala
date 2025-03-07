package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber._

class FpuConfigPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  override def build(): Unit = during setup {
    val io = new Bundle {
      val active = out Bool()
    }
    this.io = io

    io.active := False
    FpuGlobal.configSetup(config)
    FpuDatabase.updatesComplete()
    awaitBuild()

    def connectPayload(stage: Node): Unit = {
      stage(FpuGlobal.MICRO_PC) := 0
    }

    registerOperations(Map.empty)
    registerMicrocode(FpuOperation.NONE, Seq(FpuDatabase.instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0)))
  }
}