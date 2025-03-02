package fpu

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._

class FpuConfigPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  val logic = during setup new Area {
    println("FpuConfigPlugin setup: Starting")
    FpuGlobal.configSetup(config)
    FpuDatabase.updatesComplete() // Signal initial microcode setup
    awaitBuild()
    println("FpuConfigPlugin setup: Completed")

    val io = new Bundle {
      val active = out Bool()
    }
    io.active := False

    def connectPayload(stage: Stage): Unit = {
      stage(FpuGlobal.MICRO_PC) := 0 // Config plugin doesn't alter PC
    }

    registerOperations(Map.empty) // Config plugin has no direct operations
    registerMicrocode(FpuOperation.NONE, Seq(FpuDatabase.instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0)))
  }
}