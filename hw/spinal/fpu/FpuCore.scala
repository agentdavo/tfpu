package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber._

class FpuCore(val config: FPUConfig) extends FiberPlugin with Hostable {
  override def build(): Unit = during setup {
    val io = new Bundle {
      val inputA = in Bits(config.totalWidth bits)
      val inputB = in Bits(config.totalWidth bits)
      val inputC = in Bits(config.totalWidth bits)
      val operation = in(FpuOperation())
      val memAddress = in UInt(32 bits)
      val result = out Bits(config.totalWidth bits)
      val ready = out Bool()
      val specialResult = out Bits(config.totalWidth bits)
      val trapInterface = out(TrapInterface())
    }
    this.io = io

    val pipeline = new Pipeline {
      val stages = Array.fill(6)(newStage()) // Fetch, Decode, Execute1, Execute2, Normalize, Write
      connect(stages(0), stages(1))(StageLink(_,_)) // Fetch -> Decode
      connect(stages(1), stages(2))(CtrlLink(_,_))  // Decode -> Execute1
      connect(stages(2), stages(3))(CtrlLink(_,_))  // Execute1 -> Execute2
      connect(stages(3), stages(4))(CtrlLink(_,_))  // Execute2 -> Normalize
      connect(stages(4), stages(5))(StageLink(_,_)) // Normalize -> Write
      propagateDown(FpuGlobal.FA, FpuGlobal.FB, FpuGlobal.FC, FpuGlobal.TempA, FpuGlobal.TempB,
                    FpuGlobal.OPCODE, FpuGlobal.MICRO_PC, FpuGlobal.RESULT, FpuGlobal.STATUS,
                    FpuGlobal.INTERMEDIATE, FpuGlobal.MEM_ADDRESS)
    }

    val plugins = Seq(
      new FpuConfigPlugin(config, pipeline),
      new FPUStackPlugin(config, pipeline),
      new VCUPlugin(config, pipeline),
      new AdderSubtractorPlugin(config, pipeline),
      new MultiplierPlugin(config, pipeline), // Fixed null issue
      new DividerSqrtPlugin(config, pipeline),
      new StatusPlugin(config, pipeline),
      new NormalizerPlugin(config, pipeline)
    ).map(host(_).asInstanceOf[FpuExecutionPlugin])

    val rom = host[MicrocodeROMPlugin](new MicrocodeROMPlugin(config, pipeline))

    new Area {
      stages(0)(FpuGlobal.FA).assignFromBits(io.inputA)
      stages(0)(FpuGlobal.FB).assignFromBits(io.inputB)
      stages(0)(FpuGlobal.FC).assignFromBits(io.inputC)
      stages(0)(FpuGlobal.TempA).assignFromBits(B(0, config.totalWidth bits))
      stages(0)(FpuGlobal.TempB).assignFromBits(B(0, config.totalWidth bits))
      stages(0)(FpuGlobal.OPCODE) := io.operation
      stages(0)(FpuGlobal.MICRO_PC) := 0
      stages(0)(FpuGlobal.STATUS) := FPUStatus().clearAll()
      stages(0)(FpuGlobal.INTERMEDIATE).init()
      stages(0)(FpuGlobal.MEM_ADDRESS) := io.memAddress
    }

    val vcu = plugins.find(_.isInstanceOf[VCUPlugin]).get
    vcu.io.microInst := rom.io.instruction
    vcu.connectPayload(stages(1))
    when(vcu.io.active) {
      stages(1)(FpuGlobal.RESULT) := vcu.io.result
      stages(1)(FpuGlobal.STATUS) := vcu.io.outStatus
      when(vcu.io.abort) { stages(1).throwIt() }
    }

    for (stage <- Seq(2, 3)) {
      rom.io.operation := stages(stage)(FpuGlobal.OPCODE)
      rom.io.address := stages(stage)(FpuGlobal.MICRO_PC)
      plugins.foreach(_.io.microInst := rom.io.instruction)
      plugins.foreach(_.connectPayload(stages(stage)))
    }

    val norm = plugins.find(_.isInstanceOf[NormalizerPlugin]).get
    norm.io.microInst := rom.io.instruction
    norm.connectPayload(stages(4))
    when(norm.io.active) {
      stages(4)(FpuGlobal.RESULT) := norm.io.resultOut
      stages(4)(FpuGlobal.STATUS) := norm.io.statusOut
    }

    io.result := stages(5)(FpuGlobal.RESULT).asBits
    io.ready := stages(5)(FpuGlobal.MICRO_PC) === 0
    io.specialResult := vcu.io.result.asBits
    io.trapInterface.trapEnable := stages(5).isValid && stages(5)(FpuGlobal.STATUS).unalign
    io.trapInterface.trapCause := Mux(stages(5)(FpuGlobal.STATUS).unalign, U(1), U(0))

    pipeline.build()
    awaitBuild()
  }
}