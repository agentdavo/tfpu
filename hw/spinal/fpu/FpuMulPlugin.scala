package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber._

class MultiplierPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  override def build(): Unit = during setup {
    val io = new Bundle {
      val active = out Bool()
    }
    this.io = io

    val stageReg = Reg(UInt(2 bits)) init(0)
    val partialProducts = Reg(Vec(UInt(config.mantissaWidth * 2 + 4 bits), config.mantissaWidth / 2 + 2))
    val partialSum = Reg(Vec(UInt(config.mantissaWidth * 2 + 4 bits), 2))
    val sign = Reg(Bool())
    val exp = Reg(UInt(config.exponentWidth bits))

    def csa(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
      val sum = a ^ b ^ c
      val carry = (a & b) | (b & c) | (a & c)
      (sum, carry << 1)
    }

    io.active := io.microInst.op === MicrocodeOp.MUL
    when(io.active) {
      switch(stageReg) {
        is(0) {
          val exec1Stage = pipeline.stages(2)
          val mantAExt = Cat(B"1", exec1Stage(FpuGlobal.FA).mantissa).asUInt
          val mantBExt = Cat(B"1", exec1Stage(FpuGlobal.FB).mantissa).asUInt
          val boothDigits = Vec(SInt(2 bits), config.mantissaWidth / 2 + 1)
          for (i <- 0 until config.mantissaWidth / 2 + 1) {
            val bits = Cat(
              if (2 * i + 1 < config.mantissaWidth + 1) mantBExt(2 * i + 1) else False,
              if (2 * i < config.mantissaWidth + 1) mantBExt(2 * i) else False,
              if (i == 0) False else mantBExt(2 * i - 1)
            ).asBits
            boothDigits(i) := bits.mux(
              B"000" -> S"2'b00", B"001" -> S"2'b01", B"010" -> S"2'b01", B"011" -> S"2'b10",
              B"100" -> S"-2'b10", B"101" -> S"-2'b01", B"110" -> S"-2'b01", B"111" -> S"2'b00"
            )
          }

          partialProducts.zipWithIndex.foreach { case (pp, i) =>
            if (i < config.mantissaWidth / 2 + 1) {
              val digit = boothDigits(i)
              pp := (digit.mux(
                S(0) -> U(0), S(1) -> mantAExt, S(2) -> (mantAExt << 1),
                S(-1) -> (mantAExt.asSInt.unary_-), S(-2) -> ((mantAExt << 1).asSInt.unary_-)
              ) << (2 * i)).asBits.resize(config.mantissaWidth * 2 + 4).asUInt
            } else {
              pp := U(boothDigits.map(d => d < 0).reduce(_ | _)).resize(config.mantissaWidth * 2 + 4)
            }
          }

          sign := exec1Stage(FpuGlobal.FA).sign ^ exec1Stage(FpuGlobal.FB).sign
          exp := exec1Stage(FpuGlobal.FA).exponent + exec1Stage(FpuGlobal.FB).exponent - config.bias
          partialSum(0) := 0
          partialSum(1) := 0
          exec1Stage(FpuGlobal.TempA).assignFromBits(B(0, config.totalWidth bits))
          stageReg := 1
        }
        is(1) {
          val exec2Stage = pipeline.stages(3)
          val maxArraySize = 7
          val array1 = partialProducts.take(maxArraySize).toSeq
          val array2 = partialProducts.drop(maxArraySize).take(maxArraySize).toSeq

          def reduceArray(arr: Seq[UInt]): (UInt, UInt) = {
            val padded = arr ++ Seq.fill(7 - arr.length)(U(0, config.mantissaWidth * 2 + 4 bits))
            val row1 = csa(padded(0), padded(1), padded(2))
            val row2 = csa(row1._1, row1._2, padded(3))
            val row3 = csa(row2._1, row2._2, padded(4))
            val row4 = csa(row3._1, row3._2, padded(5))
            val row5 = csa(row4._1, row4._2, padded(6))
            (row5._1, row5._2)
          }

          val red1 = reduceArray(array1)
          val red2 = reduceArray(array2)
          val (sum4, carry4) = csa(red1._1, red1._2, red2._1)
          val (sum2, carry2) = csa(sum4, carry4, red2._2)
          val (finalSum, finalCarry) = csa(sum2, carry2, partialSum(0))

          partialSum(0) := finalSum
          partialSum(1) := finalCarry
          exec2Stage(FpuGlobal.TempB).assignFromBits(finalSum.asBits.resize(config.totalWidth))
          stageReg := 2
        }
        is(2) {
          val normalizeStage = pipeline.stages(4)
          val pFull = partialSum(0) + partialSum(1)
          normalizeStage(FpuGlobal.RESULT).sign := sign
          normalizeStage(FpuGlobal.RESULT).exponent := exp
          normalizeStage(FpuGlobal.RESULT).mantissa := pFull(config.mantissaWidth * 2 + 1 downto config.mantissaWidth + 2).asUInt
          normalizeStage(FpuGlobal.INTERMEDIATE).partialProducts := partialProducts
          normalizeStage(FpuGlobal.INTERMEDIATE).partialSum := partialSum
          normalizeStage(FpuGlobal.STATUS) := normalizeStage(FpuGlobal.STATUS)
          stageReg := 0
        }
      }
    }

    def connectPayload(stage: Node): Unit = {
      stage(FpuGlobal.MICRO_PC) := io.microInst.nextPc
      when(io.active && io.microInst.nextPc =/= 0) { stage.haltIt() }
    }

    registerOperations(Map("MUL" -> FpuOperation.MUL, "LDNLMULSN" -> FpuOperation.LDNLMULSN, "LDNLMULDB" -> FpuOperation.LDNLMULDB))
    val mulSeq = if (config.isSinglePrecision)
      Seq(
        FpuDatabase.instr(MicrocodeOp.MUL, 1, 2, 1, 1),
        FpuDatabase.instr(MicrocodeOp.NORM, 1, 0, 1, 0)
      )
    else
      Seq(
        FpuDatabase.instr(MicrocodeOp.MUL, 1, 2, 1, 1),
        FpuDatabase.instr(MicrocodeOp.MUL, 1, 2, 1, 2),
        FpuDatabase.instr(MicrocodeOp.NORM, 1, 0, 1, 0)
      )
    registerMicrocode(FpuOperation.MUL, mulSeq)
    registerMicrocode(FpuOperation.LDNLMULSN, Seq(
      FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 4, 1),
      FpuDatabase.instr(MicrocodeOp.MUL, 1, 4, 1, 1),
      FpuDatabase.instr(MicrocodeOp.NORM, 1, 0, 1, 0)
    ))
    registerMicrocode(FpuOperation.LDNLMULDB, Seq(
      FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 4, 1),
      FpuDatabase.instr(MicrocodeOp.MUL, 1, 4, 1, 1),
      FpuDatabase.instr(MicrocodeOp.MUL, 1, 4, 1, 2),
      FpuDatabase.instr(MicrocodeOp.NORM, 1, 0, 1, 0)
    ))
    awaitBuild()
  }
}