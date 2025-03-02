package fpu

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._

class MicrocodeROMPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  val logic = during setup new Area {
    println("MicrocodeROMPlugin setup: Starting")
    FpuDatabase.updatesComplete.await()
    println("MicrocodeROMPlugin setup: Completed")

    val io = new Bundle {
      val operation = in(FpuOperation())
      val address = in UInt(6 bits)
      val instruction = out(MicrocodeInstruction())
    }

    // Connect to all stages via pipeline
    val rom = Mem(MicrocodeInstruction(), 128)
    val sequences = try {
      val seq = FpuDatabase.microcodeSequences.get
      println(s"MicrocodeROMPlugin setup: sequences = $seq")
      if (seq.nonEmpty) seq else throw new Exception("Sequences empty")
    } catch {
      case e: Exception =>
        val default = Map(FpuOperation.NONE -> Seq(FpuDatabase.instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0)))
        println(s"MicrocodeROMPlugin setup: Using default sequences = $default due to $e")
        default
    }
    val sequenceVec = Vec(sequences.values.map(seq => Vec(seq.padTo(128, init()))).toSeq)
    val opKeys = sequences.keys.toSeq
    val opIndex = opKeys.indexOf(io.operation) match {
      case -1 => 0
      case idx => idx
    }
    val selectedSequence = sequenceVec(opIndex)
    rom.init(selectedSequence)

    // Propagate instruction to all stages
    for (stage <- pipeline.stages) {
      when(stage.isValid) {
        io.operation := stage(FpuGlobal.OPCODE)
        io.address := stage(FpuGlobal.MICRO_PC)
        stage(FpuGlobal.MICRO_INSTRUCTION) := rom.readSync(io.address)
      }
    }
    io.instruction := rom.readSync(io.address)

    println("MicrocodeROMPlugin setup: ROM initialized")

    def connectPayload(stage: Stage): Unit = {
      stage(FpuGlobal.MICRO_PC) := io.microInst.nextPc
    }

    registerOperations(Map.empty) // ROM is a utility, no direct ops
    registerMicrocode(FpuOperation.NONE, Seq(FpuDatabase.instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0)))
  }

  def init(): MicrocodeInstruction = {
    val i = MicrocodeInstruction()
    i.op := MicrocodeOp.FINALIZE
    i.srcA := 0
    i.srcB := 0
    i.dest := 0
    i.nextPc := 0
    i
  }
}