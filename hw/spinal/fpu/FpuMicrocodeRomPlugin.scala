package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber._

class MicrocodeROMPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  override def build(): Unit = during setup {
    val io = new Bundle {
      val operation = in(FpuOperation())
      val address = in UInt(6 bits)
      val instruction = out(MicrocodeInstruction())
    }
    this.io = io

    println("MicrocodeROMPlugin setup: Starting")
    FpuDatabase.updatesComplete.await()
    println("MicrocodeROMPlugin setup: Completed")

    val rom = Mem(MicrocodeInstruction(), 128)
    val sequences = FpuDatabase.microcodeSequences.get
    val sequenceVec = Vec(sequences.values.map(seq => Vec(seq.padTo(128, instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0)))).toSeq)
    val opKeys = sequences.keys.toSeq
    val opIndex = opKeys.indexOf(io.operation) match {
      case -1 => 0
      case idx => idx
    }
    val selectedSequence = sequenceVec(opIndex)
    rom.init(selectedSequence)

    for (stage <- pipeline.stages) {
      when(stage.isValid) {
        io.operation := stage(FpuGlobal.OPCODE)
        io.address := stage(FpuGlobal.MICRO_PC)
        stage(FpuGlobal.MICRO_INSTRUCTION) := rom.readSync(io.address)
      }
    }
    io.instruction := rom.readSync(io.address)

    def connectPayload(stage: Node): Unit = {
      stage(FpuGlobal.MICRO_PC) := io.microInst.nextPc
    }

    registerOperations(Map.empty)
    registerMicrocode(FpuOperation.NONE, Seq(instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0)))
    awaitBuild()
  }

  def instr(op: MicrocodeOp.E, srcA: Int, srcB: Int, dest: Int, next: Int): MicrocodeInstruction = {
    val i = MicrocodeInstruction()
    i.op := op
    i.srcA := srcA
    i.srcB := srcB
    i.dest := dest
    i.nextPc := next
    i
  }
}