package fpu

import spinal.core._
import spinal.lib._

class Multiplier extends Component {
  val io = new Bundle {
    val a = in(Fp64())
    val b = in(Fp64())
    val isSingle = in Bool()
    val roundingMode = in(RoundingMode())
    val result = out(Fp64())
    val flags = out(FpuFlags())
  }

  // Pipeline Stage 1: Input Handling
  val stage1 = new Area {
    val mantA = Reg(UInt(53 bits)) init(0)  // Implicit '1' + 52-bit mantissa
    val mantB = Reg(UInt(53 bits)) init(0)
    val expA = Reg(SInt(11 bits)) init(0)
    val expB = Reg(SInt(11 bits)) init(0)
    val signA = Reg(Bool()) init(False)
    val signB = Reg(Bool()) init(False)
    val isSingleReg = Reg(Bool()) init(False)
    val roundingModeReg = Reg(RoundingMode()) init(RoundingMode.RNE)

    mantA := Cat(U"1", io.a.mant).asUInt
    mantB := Cat(U"1", io.b.mant).asUInt
    expA := io.a.exp.asSInt
    expB := io.b.exp.asSInt
    signA := io.a.sign
    signB := io.b.sign
    isSingleReg := io.isSingle
    roundingModeReg := io.roundingMode
  }

  // Pipeline Stage 2: Partial Product Generation and Summation
  val stage2 = new Area {
    val prodReg = Reg(UInt(108 bits)) init(0)
    val exp = Reg(SInt(12 bits)) init(0)
    val sign = Reg(Bool()) init(False)
    val isSingle = Reg(Bool()) init(False)
    val roundingMode = Reg(RoundingMode()) init(RoundingMode.RNE)

    // Generate and sum partial products using Booth encoding
    val sum = (0 until 27).map(i => {
      val prevBit = if (i == 0) U(0, 1 bit) else stage1.mantB(2 * i - 1)
      val digitBits = if (i < 26) stage1.mantB(2 * i + 1 downto 2 * i) else U"0" ## stage1.mantB(52)
      val digit = (digitBits ## prevBit).asUInt
      digit.mux(
        U(0) -> U(0, 108 bits),
        U(1) -> stage1.mantA.resize(108 bits),
        U(2) -> (stage1.mantA << 1).resize(108 bits),
        U(3) -> ((stage1.mantA << 1) + stage1.mantA).resize(108 bits),
        U(4) -> (stage1.mantA << 2).resize(108 bits),
        default -> U(0, 108 bits)
      ) << (2 * i)
    }).reduceBalancedTree(_ + _)
    prodReg := sum.resize(108 bits)  // Take lower 108 bits for the product

    exp := stage1.expA + stage1.expB - S(1023, 12 bits)
    sign := stage1.signA ^ stage1.signB
    isSingle := stage1.isSingleReg
    roundingMode := stage1.roundingModeReg
  }

  // Pipeline Stage 3: Normalization and Rounding
  val stage3 = new Area {
    val prod = stage2.prodReg
    val normShift = prod(107 downto 106) === U"01"
    val mantBits = Mux(normShift, prod(105 downto 0), prod(106 downto 0)).resize(54 bits)
    val expAdjust = Mux(normShift, S(-1, 12 bits), S(0, 12 bits))

    val guard = Mux(stage2.isSingle, mantBits(29), mantBits(0))
    val sticky = Mux(stage2.isSingle, mantBits(28 downto 0).orR, False)
    val roundUp = stage2.roundingMode.mux(
      RoundingMode.RNE -> (guard && (sticky || mantBits(30))),
      RoundingMode.RTZ -> False,
      RoundingMode.RUP -> ((guard || sticky) && !stage2.sign),
      RoundingMode.RDN -> ((guard || sticky) && stage2.sign)
    )
    val mantNorm = mantBits.resize(53 bits)
    val roundedMantFull = (mantNorm.resize(54 bits) + roundUp.asUInt)
    val mantOverflow = roundedMantFull(53)
    val roundedMant = roundedMantFull(52 downto 0)

    val finalExp = stage2.exp + expAdjust + mantOverflow.asSInt

    io.result.sign := stage2.sign
    io.result.exp := finalExp.asUInt.resize(11 bits)
    io.result.mant := Mux(stage2.isSingle, roundedMant(51 downto 29), roundedMant(51 downto 0)).asBits
    io.flags.NV := False
    io.flags.NX := False
    io.flags.OF := False
    io.flags.UF := False
    io.flags.DZ := False
  }
}