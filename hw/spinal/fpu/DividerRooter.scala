package fpu

import spinal.core._
import spinal.lib._

// Divider/Rooter with Radix-2 SRT and PCA
class DividerRooter extends Component {
  val io = new Bundle {
    val a, b = in(Fp64())
    val isDiv = in Bool()
    val isSingle = in Bool()
    val roundingMode = in(RoundingMode())
    val result = out(Fp64())
    val flags = out(FpuFlags())
  }

  val quotient = Reg(Bits(56 bits)) init(0)
  val remainder = Reg(Bits(108 bits)) init(Cat(B"1", io.a.mant, B(0, 53 bits)))
  val divisor = Cat(B"1", io.b.mant).asUInt
  val root = Reg(Bits(56 bits)) init(0)
  val step = Reg(UInt(4 bits)) init(0)
  val maxSteps = Mux(io.isSingle, U(7), U(14)) // 28 bits / 4 = 7 cycles single, 56 bits / 4 = 14 cycles double

  when(step < maxSteps) {
    val pUpper = remainder(107 downto 104).asSInt
    val qDigits = Vec(SInt(2 bits), 4)
    for (i <- 0 until 4) {
      qDigits(i) := Mux(pUpper >= divisor.asSInt, S(1, 2 bits), Mux(pUpper <= -divisor.asSInt, S(-1, 2 bits), S(0, 2 bits)))
      remainder := (remainder << 1) - (qDigits(i) * divisor.asSInt).resize(108 bits)
    }
    quotient := Cat(quotient(51 downto 0), qDigits.reverse.asBits)
    when(!io.isDiv) {
      root := Cat(root(51 downto 0), qDigits.reverse.asBits)
      remainder := (remainder << 4) - (qDigits.asSInt * (root.asSInt << 1)).resize(108 bits)
    }
    step := step + 1
  }

  val finalMant = Mux(io.isDiv, quotient(55 downto 3), root(55 downto 3))
  val finalExp = Mux(io.isDiv, io.a.exp - io.b.exp + (io.isSingle ? U(127) | U(1023)), io.a.exp >> 1 + (io.isSingle ? U(63) | U(511)))
  val guard = Mux(io.isSingle, quotient(2), quotient(1))
  val sticky = Mux(io.isSingle, quotient(1 downto 0).orR, quotient(0))
  val roundUp = io.roundingMode.mux(
    RoundingMode.RNE -> (guard && (sticky || finalMant(0))),
    RoundingMode.RTZ -> False,
    RoundingMode.RDN -> (io.result.sign && (guard || sticky)),
    RoundingMode.RUP -> (!io.result.sign && (guard || sticky))
  )
  val roundedMant = finalMant.asUInt + roundUp.asUInt

  io.result.sign := io.a.sign ^ (io.isDiv ? io.b.sign | False)
  io.result.exp := finalExp
  io.result.mant := Mux(io.isSingle, Cat(roundedMant(22 downto 0), B(0, 29 bits)), roundedMant(51 downto 0))
  io.flags.NV := io.a.isNaN || io.b.isNaN || (!io.isDiv && io.a.sign)
  io.flags.NX := guard || sticky
  io.flags.OF := finalExp > (io.isSingle ? U(254) | U(2046))
  io.flags.UF := finalExp < 1 && !io.result.isZero
  io.flags.DZ := io.isDiv && io.b.isZero
}