package fpu

import spinal.core._
import spinal.lib._

// Radix-4 Carry-Skip Adder Component
class DualAdder extends Component {
  val io = new Bundle {
    val a = in(Fp64())
    val b = in(Fp64())
    val isSub = in Bool()
    val cin = in Bool()
    val isSingle = in Bool()
    val roundingMode = in(RoundingMode())
    val result = out(Fp64())
    val flags = out(FpuFlags())
  }

  val expDiff = io.a.exp.asSInt - io.b.exp.asSInt
  val alignA = io.a.mant.asUInt.resize(57 bits) >> expDiff.abs
  val alignB = io.b.mant.asUInt.resize(57 bits) >> (-expDiff).abs
  val mantA = Mux(expDiff >= 0, io.a.mant.asUInt.resize(57 bits), alignA)
  val mantB = Mux(expDiff >= 0, alignB, io.b.mant.asUInt.resize(57 bits))

  val adder1 = SInt(58 bits)
  val adder2 = SInt(58 bits)
  val sum1 = mantA.asSInt + Mux(io.isSub, -mantB.asSInt, mantB.asSInt) + io.cin.asSInt
  val sum2 = sum1 >> 1
  adder1 := sum1
  adder2 := sum2

  val p = Vec(Bool(), 57)
  val g = Vec(Bool(), 57)
  for (i <- 0 until 57) {
    p(i) := mantA(i) ^ mantB(i)
    g(i) := mantA(i) & mantB(i)
  }
  val potentialPoints = Vec(Bool(), 57)
  for (i <- 2 until 57) {
    potentialPoints(i) := g(i) | (p(i) & (g(i - 1) | p(i - 1) & g(i - 2)))
  }
  val normShift = CountOne(potentialPoints.reverse).resize(6 bits)

  val overflow = sum1(57)
  val rawMant = Mux(overflow, sum2, sum1)(56 downto 0)
  val sticky = rawMant(0)
  val roundUp = io.roundingMode.mux(
    RoundingMode.RNE -> (rawMant(1) && (sticky || rawMant(2))),
    RoundingMode.RTZ -> False,
    RoundingMode.RUP -> (sticky && !io.a.sign),
    RoundingMode.RDN -> (sticky && io.a.sign)
  )
  val mantNorm = rawMant.asUInt + roundUp.asUInt
  val finalMant = (mantNorm << normShift).resize(57 bits)

  io.result.sign := io.a.sign ^ io.b.sign
  io.result.exp := Mux(expDiff >= 0, io.a.exp, io.b.exp) + overflow.asUInt - normShift
  io.result.mant := Mux(io.isSingle, finalMant(51, 29 bits).resize(52 bits), finalMant(51, 0 bits)).asBits
  io.flags.NV := False
  io.flags.NX := sticky
  io.flags.OF := False
  io.flags.UF := False
  io.flags.DZ := False
} 