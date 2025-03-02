package fpu

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._

class FpuCore(val config: FPUConfig) extends FiberPlugin {
  val io = new Bundle {
    val inputA = in Bits(config.totalWidth bits)
    val inputB = in Bits(config.totalWidth bits)
    val inputC = in Bits(config.totalWidth bits)
    val operation = in(FpuOperation())
    val memAddress = in UInt(32 bits)
    val result = out Bits(config.totalWidth bits)
    val ready = out Bool()
    val specialResult = out Bits(config.totalWidth bits)
  }

  val logic = during setup new Area {
    println("FpuCore setup: Starting")
    awaitBuild()

    val pipeline = new Pipeline {
      val stages = Array.fill(6)(newStage()) // Fetch, Decode, Execute1, Execute2, Normalize, Write
      connect(stages(0), stages(1))(StageLink(_,_)) // Fetch -> Decode
      connect(stages(1), stages(2))(CtrlLink(_,_))  // Decode -> Execute1
      connect(stages(2), stages(3))(CtrlLink(_,_))  // Execute1 -> Execute2
      connect(stages(3), stages(4))(CtrlLink(_,_))  // Execute2 -> Normalize
      connect(stages(4), stages(5))(StageLink(_,_)) // Normalize -> Write

      propagateDown(FpuGlobal.FA, FpuGlobal.FB, FpuGlobal.FC, FpuGlobal.TempA, FpuGlobal.TempB,
                    FpuGlobal.OPCODE, FpuGlobal.MICRO_PC, FpuGlobal.RESULT, FpuGlobal.STATUS, FpuGlobal.INTERMEDIATE)

      val plugins = List(
        new FpuConfigPlugin(config, this),
        new FPUStackPlugin(config, this),
        new VCUPlugin(config, this),
        new AdderSubtractorPlugin(config, this),
        new MultiplierPlugin(config, this),
        new DividerSqrtPlugin(config, this),
        new StatusPlugin(config, this),
        new NormalizerPlugin(config, this)
      ).map(host(_).asInstanceOf[FpuExecutionPlugin])

      val rom = host[MicrocodeROMPlugin](new MicrocodeROMPlugin(config, this))

      // Add master-slave latches for each stage to minimize peak current
      for (stage <- stages) {
        stage.addLatch() // Master-slave latch for clock edge distribution
      }

      val fetch = new Area {
        stages(0)(FpuGlobal.FA) := io.inputA.as(FloatData(config))
        stages(0)(FpuGlobal.FB) := io.inputB.as(FloatData(config))
        stages(0)(FpuGlobal.FC) := io.inputC.as(FloatData(config))
        stages(0)(FpuGlobal.TempA).assignFromBits(B(0, config.totalWidth bits))
        stages(0)(FpuGlobal.TempB).assignFromBits(B(0, config.totalWidth bits))
        stages(0)(FpuGlobal.OPCODE) := io.operation
        stages(0)(FpuGlobal.MICRO_PC) := 0
        stages(0)(FpuGlobal.STATUS) := FPUStatus().clearAll()
        stages(0)(FpuGlobal.INTERMEDIATE).init()
      }

      val decode = new Area {
        val vcu = plugins.find(_.isInstanceOf[VCUPlugin]).get
        vcu.io.microInst := rom.logic.io.instruction
        vcu.connectPayload(stages(1))
        when(vcu.io.active) {
          stages(1)(FpuGlobal.RESULT) := vcu.io.result
          stages(1)(FpuGlobal.STATUS) := vcu.io.outStatus
          when(vcu.io.abort.toBoolean) { stages(1).throwIt() }
        }
      }

      val execute1 = new Area {
        rom.logic.io.operation := stages(2)(FpuGlobal.OPCODE)
        rom.logic.io.address := stages(2)(FpuGlobal.MICRO_PC)
        plugins.foreach(_.io.microInst := rom.logic.io.instruction)
        plugins.foreach(_.connectPayload(stages(2)))
      }

      val execute2 = new Area {
        rom.logic.io.operation := stages(3)(FpuGlobal.OPCODE)
        rom.logic.io.address := stages(3)(FpuGlobal.MICRO_PC)
        plugins.foreach(_.io.microInst := rom.logic.io.instruction)
        plugins.foreach(_.connectPayload(stages(3)))
      }

      val normalize = new Area {
        val norm = plugins.find(_.isInstanceOf[NormalizerPlugin]).get
        norm.io.microInst := rom.logic.io.instruction
        norm.connectPayload(stages(4))
        when(norm.io.active) {
          stages(4)(FpuGlobal.RESULT) := norm.io.resultOut
          stages(4)(FpuGlobal.STATUS) := norm.io.statusOut
        }
      }

      val write = new Area {
        io.result := stages(5)(FpuGlobal.RESULT).asBits
        io.ready := stages(5)(FpuGlobal.MICRO_PC) === 0
        io.specialResult := plugins.find(_.isInstanceOf[VCUPlugin]).get.io.result.asBits
      }
    }

    pipeline.build()
    println("FpuCore setup: Completed")
  }
}