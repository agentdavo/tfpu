package fpu

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._

class NormalizerPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  val logic = during setup new Area {
    println("NormalizerPlugin setup: Starting")
    FpuDatabase.updatesComplete()
    awaitBuild()
    println("NormalizerPlugin setup: Completed")

    val io = new Bundle {
      val resultIn = in(new FloatData(config)) // Raw product or intermediate result
      val statusIn = in(new FPUStatus())       // Includes roundingMode
      val resultOut = out(new FloatData(config))
      val statusOut = out(new FPUStatus())
      val active = out Bool()
    }

    // Connect to the normalize stage (stage 4)
    val normalizeStage = pipeline.stages(4)
    io.resultIn := normalizeStage(FpuGlobal.RESULT)
    io.statusIn := normalizeStage(FpuGlobal.STATUS)

    io.active := False
    when(normalizeStage.isValid && normalizeStage(FpuGlobal.MICRO_PC) =/= 0 && io.microInst.op === MicrocodeOp.NORM) {
      io.active := True

      // Denormal handling: Shift mantissa to normalize, adjust exponent
      val isDenormal = io.resultIn.isDenormal
      val shiftCount = Reg(UInt(log2Up(config.mantissaWidth + 1) bits)) init(0)
      val denormalState = Reg(Bool()) init(False)

      when(isDenormal && !denormalState) {
        // First pass: Calculate leading zeros and store intermediate in TempA
        val leadingZeros = OHToUInt(OHMasking.first(io.resultIn.mantissa)) // Count leading zeros
        shiftCount := leadingZeros
        normalizeStage(FpuGlobal.TempA).mantissa := io.resultIn.mantissa |<< leadingZeros
        normalizeStage(FpuGlobal.TempA).exponent := io.resultIn.exponent - leadingZeros
        normalizeStage(FpuGlobal.TempA).sign := io.resultIn.sign
        denormalState := True
        normalizeStage.haltIt() // Wait for next cycle
      } otherwise {
        // Either normalized number or second pass of denormal handling
        val inputMantissa = Mux(denormalState, normalizeStage(FpuGlobal.TempA).mantissa, io.resultIn.mantissa)
        val inputExponent = Mux(denormalState, normalizeStage(FpuGlobal.TempA).exponent, io.resultIn.exponent)
        val inputSign = Mux(denormalState, normalizeStage(FpuGlobal.TempA).sign, io.resultIn.sign)

        // Interpolative Rounding and Post-Normalization
        val rawMantissa = Cat(B"1", inputMantissa, U"0").asUInt // Extend for rounding precision
        val rawProduct = Cat(rawMantissa, U(0, config.mantissaWidth + 2 bits)).asUInt // Full product width

        // Compute P and P++ (parallel adders)
        val pFull = rawProduct
        val pPlusPlusFull = pFull + U(2)

        // Extract mantissa candidates
        val pNoShiftFull = pFull(config.mantissaWidth * 2 + 2 downto config.mantissaWidth - 1)
        val pShiftedFull = pFull(config.mantissaWidth * 2 + 1 downto config.mantissaWidth - 2)
        val pPlusPlusNoShiftFull = pPlusPlusFull(config.mantissaWidth * 2 + 2 downto config.mantissaWidth - 1)
        val pPlusPlusShiftedFull = pPlusPlusFull(config.mantissaWidth * 2 + 1 downto config.mantissaWidth - 2)

        // Interpolation for P+ (table: P'.1 if P_lsb=0, P++.0 if P_lsb=1)
        val pLsb = pFull(config.mantissaWidth - 1)
        val pPlusNoShiftFull = Mux(pLsb,
          Cat(pPlusPlusNoShiftFull(config.mantissaWidth downto 1), False),
          Cat(pNoShiftFull(config.mantissaWidth downto 1), True)
        )
        val pPlusShiftedFull = Mux(pLsb,
          Cat(pPlusPlusShiftedFull(config.mantissaWidth downto 1), False),
          Cat(pShiftedFull(config.mantissaWidth downto 1), True)
        )

        // Detailed Rounding Logic (IEEE-754 compliant)
        val guardNoShift = pFull(config.mantissaWidth - 1)
        val roundNoShift = pFull(config.mantissaWidth - 2)
        val stickyNoShift = pFull(config.mantissaWidth - 3 downto 0) =/= 0
        val guardShifted = pFull(config.mantissaWidth - 2)
        val roundShifted = pFull(config.mantissaWidth - 3)
        val stickyShifted = pFull(config.mantissaWidth - 4 downto 0) =/= 0

        val roundUpNoShift = io.statusIn.roundingMode.mux(
          0 -> (guardNoShift && (roundNoShift || stickyNoShift) || // Nearest-Even
                (guardNoShift && roundNoShift && !stickyNoShift && pNoShiftFull(0))), // Tie to even
          1 -> False, // Toward Zero
          2 -> (guardNoShift && !inputSign), // Toward +Infinity
          3 -> (guardNoShift && inputSign) // Toward -Infinity
        )

        val roundUpShifted = io.statusIn.roundingMode.mux(
          0 -> (guardShifted && (roundShifted || stickyShifted) || // Nearest-Even
                (guardShifted && roundShifted && !stickyShifted && pShiftedFull(0))), // Tie to even
          1 -> False, // Toward Zero
          2 -> (guardShifted && !inputSign), // Toward +Infinity
          3 -> (guardShifted && inputSign) // Toward -Infinity
        )

        // Select rounded values
        val mantNoShift = Mux(roundUpNoShift, pPlusNoShiftFull, pNoShiftFull)(config.mantissaWidth downto 0)
        val mantShifted = Mux(roundUpShifted, pPlusShiftedFull, pShiftedFull)(config.mantissaWidth downto 0)

        // Final normalization shift (third mux)
        val needsShift = pFull(config.mantissaWidth * 2 + 3)
        val mantissa = Mux(needsShift, mantShifted, mantNoShift)
        val expAdjust = Mux(needsShift, U(1), U(0))
        val expResult = inputExponent + expAdjust

        normalizeStage(FpuGlobal.RESULT).sign := inputSign
        normalizeStage(FpuGlobal.RESULT).exponent := expResult
        normalizeStage(FpuGlobal.RESULT).mantissa := mantissa(config.mantissaWidth - 1 downto 0)
        normalizeStage(FpuGlobal.STATUS).overflow := expResult >= config.maxExponent
        normalizeStage(FpuGlobal.STATUS).underflow := expResult <= 0
        normalizeStage(FpuGlobal.STATUS).inexact := Mux(needsShift, guardShifted | stickyShifted, guardNoShift | stickyNoShift)

        // Reset denormal state after processing
        when(!isDenormal || denormalState) {
          denormalState := False
          when(io.microInst.nextPc =/= 0) { normalizeStage.haltIt() }
        }
      }
    }

    def connectPayload(stage: Stage): Unit = {
      stage(FpuGlobal.MICRO_PC) := io.microInst.nextPc
      io.resultOut := stage(FpuGlobal.RESULT)
      io.statusOut := stage(FpuGlobal.STATUS)
    }

    registerOperations(Map("NORM" -> FpuOperation.NONE)) // NORM is a supporting op, tied to arithmetic ops
    registerMicrocode(FpuOperation.NONE, Seq(FpuDatabase.instr(MicrocodeOp.NORM, 1, 0, 1, 0)))
  }
}