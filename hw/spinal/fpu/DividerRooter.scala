package fpu

import spinal.core._
import spinal.lib._

class DividerRooter extends Component {
  val io = new Bundle {
    val a = in(Fp64())
    val b = in(Fp64())
    val isDiv = in Bool()
    val isSingle = in Bool()
    val roundingMode = in(RoundingMode())
    val result = out(Fp64())
    val flags = out(FpuFlags())
  }

  val dividend = io.a.mant.asUInt.resize(108 bits)
  val divisor = io.b.mant.asUInt.resize(108 bits)
  val quotient = Vec(SInt(2 bits), 54)
  val remainder = Reg(SInt(108 bits)) init(0)
  val root = Reg(UInt(54 bits)) init(0)
  val qDigits = Vec(SInt(2 bits), 54)

  when(io.isDiv) {
    for (i <- 0 to 53) {
      remainder := (remainder << 1) + (dividend(53 - i) ## B(0, 53 bits)).asSInt
      qDigits(i) := remainder / divisor.asSInt
      remainder := remainder - (qDigits(i) * divisor.asSInt).resize(108 bits)
    }
    quotient := qDigits
  } otherwise {
    for (i <- 0 to 13) {
      remainder := (remainder << 4) + (dividend(53 - i * 4, (53 - i * 4) - 3 bits) ## B(0, 60 bits)).asSInt
      qDigits(i) := remainder / (root.asSInt << 1)
      remainder := remainder - (qDigits(i) * (root.asSInt << 1)).resize(108 bits)
      root := (root << 2) + qDigits(i).asUInt
    }
  }

  val mantBits = quotient.asBits.resize(54 bits)
  val sticky = mantBits(0)
  val roundUp = io.roundingMode.mux(
    RoundingMode.RNE -> (mantBits(1) && (sticky || mantBits(2))),
    RoundingMode.RTZ -> False,
    RoundingMode.RUP -> (sticky && !io.a.sign),
    RoundingMode.RDN -> (sticky && io.a.sign)
  )
  val roundedMant = mantBits.asUInt + roundUp.asUInt
  io.result.sign := io.a.sign ^ io.b.sign
  io.result.exp := io.a.exp - io.b.exp + 1023
  io.result.mant := Mux(io.isSingle, roundedMant(51, 29 bits).resize(52 bits), roundedMant(51, 0 bits)).asBits
  io.flags.NV := False
  io.flags.NX := False
  io.flags.OF := False
  io.flags.UF := False
  io.flags.DZ := io.b.isZero
}