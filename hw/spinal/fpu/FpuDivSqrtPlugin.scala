package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber._

class DividerSqrtPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  override def build(): Unit = during setup {
    val io = new Bundle {
      val result = out(new FloatData(config))
      val outStatus = out(new FPUStatus())
      val partialRemainder = out(SInt(config.mantissaWidth + 4 bits))
      val quotient = out(UInt(config.mantissaWidth + 4 bits))
      val counter = out(UInt(log2Up(config.mantissaWidth + 1) bits))
      val active = out Bool()
    }
    this.io = io

    val exec1Stage = pipeline.stages(2)
    val exec2Stage = pipeline.stages(3)
    io.result := exec2Stage(FpuGlobal.RESULT)
    io.outStatus := exec2Stage(FpuGlobal.STATUS)

    val extWidth = config.mantissaWidth + 4
    val partialRemainder = Reg(SInt(extWidth bits))
    val quotient = Reg(UInt(extWidth bits))
    val counter = Reg(UInt(log2Up(config.mantissaWidth + 1) bits))
    val dividendMant = Cat(B"1", exec1Stage(FpuGlobal.FA).mantissa).asUInt
    val divisorMant = Cat(B"1", exec1Stage(FpuGlobal.FB).mantissa).asUInt
    val isSquareRoot = exec1Stage(FpuGlobal.OPCODE) === FpuOperation.SQRT

    io.active := False
    when(exec1Stage.isValid && exec1Stage(FpuGlobal.MICRO_PC) =/= 0 && (io.microInst.op === MicrocodeOp.DIV || io.microInst.op === MicrocodeOp.SQRT)) {
      io.active := True

      when(counter === 0) {
        partialRemainder := Mux(isSquareRoot, S(dividendMant), S(dividendMant) << (config.mantissaWidth + 1)).resize(extWidth)
        quotient := 0
        exec1Stage(FpuGlobal.TempA).assignFromBits(partialRemainder.asBits.resize(config.totalWidth))
      } elsewhen(counter < Mux(isSquareRoot, U(config.mantissaWidth / 2), U(config.mantissaWidth))) {
        val rShifted = partialRemainder << 2
        val qLastBits = quotient.resize(counter.getWidth bits)
        val qLast = qLastBits(qLastBits.high - 1 downto qLastBits.high - 2)
        val trialDivisor = Mux(isSquareRoot, Cat(B"01", quotient.resize(config.mantissaWidth), B"00").asUInt.resize(extWidth), divisorMant.resize(extWidth))
        val trialSub = rShifted - Mux(isSquareRoot, Cat(qLast, B"11").asSInt, S(trialDivisor))
        val qDigit = Mux(rShifted >= 0 && trialSub >= 0, S"2'b01", Mux(rShifted < 0 && trialSub < 0, S"2'b11", S"2'b00"))
        val product = (qDigit.abs * trialDivisor).asSInt
        partialRemainder := rShifted - product
        quotient := (quotient << 2) | qDigit.abs
        exec2Stage(FpuGlobal.TempB).assignFromBits(quotient.asBits.resize(config.totalWidth))
      }
      counter := counter + 1
      exec1Stage(FpuGlobal.INTERMEDIATE).partialRemainder := partialRemainder
      exec1Stage(FpuGlobal.INTERMEDIATE).quotient := quotient
      exec1Stage(FpuGlobal.INTERMEDIATE).counter := counter
      when(io.microInst.nextPc =/= 0) { exec1Stage.haltIt() }
    }

    when(exec2Stage.isValid && exec2Stage(FpuGlobal.MICRO_PC) =/= 0 && io.microInst.op === MicrocodeOp.DIV) {
      when(exec2Stage(FpuGlobal.INTERMEDIATE).counter === Mux(isSquareRoot, U(config.mantissaWidth / 2), U(config.mantissaWidth))) {
        io.result.sign := exec1Stage(FpuGlobal.FA).sign ^ Mux(isSquareRoot, False, exec1Stage(FpuGlobal.FB).sign)
        io.result.exponent := Mux(isSquareRoot,
          (exec1Stage(FpuGlobal.FA).exponent + config.bias) >> 1,
          exec1Stage(FpuGlobal.FA).exponent - exec1Stage(FpuGlobal.FB).exponent + config.bias)
        io.result.mantissa := quotient(config.mantissaWidth + 2 downto 3)
        io.outStatus.divideByZero := exec1Stage(FpuGlobal.FB).isZero && !isSquareRoot
        io.outStatus.inexact := quotient(2 downto 0) =/= 0
      }
      exec2Stage(FpuGlobal.RESULT) := io.result
      exec2Stage(FpuGlobal.STATUS) := io.outStatus
      when(io.microInst.nextPc =/= 0) { exec2Stage.haltIt() }
    }

    io.partialRemainder := partialRemainder
    io.quotient := quotient
    io.counter := counter

    def connectPayload(stage: Node): Unit = {
      stage(FpuGlobal.MICRO_PC) := io.microInst.nextPc
    }

    registerOperations(Map("DIV" -> FpuOperation.DIV, "SQRT" -> FpuOperation.SQRT, "REM" -> FpuOperation.REM))
    val divSeq = if (config.isSinglePrecision)
      Seq(
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 1),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 2),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 3),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 4),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 5),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 6),
        FpuDatabase.instr(MicrocodeOp.NORM, 1, 0, 1, 0)
      )
    else
      Seq(
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 1),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 2),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 3),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 4),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 5),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 6),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 7),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 8),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 9),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 10),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 11),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 12),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 13),
        FpuDatabase.instr(MicrocodeOp.DIV, 1, 2, 1, 14),
        FpuDatabase.instr(MicrocodeOp.NORM, 1, 0, 1, 0)
      )
    val sqrtSeq = divSeq
    val remSeq = divSeq
    registerMicrocode(FpuOperation.DIV, divSeq)
    registerMicrocode(FpuOperation.SQRT, sqrtSeq)
    registerMicrocode(FpuOperation.REM, remSeq)
    awaitBuild()
  }
}