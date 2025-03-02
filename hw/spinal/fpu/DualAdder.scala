package fpu

import spinal.core._
import spinal.lib._

// Radix-4 Carry-Skip Adder Component
class CarrySkipAdder(width: Int, blockSize: Int) extends Component {
  val io = new Bundle {
    val a, b = in SInt(width bits)
    val cin = in Bool()
    val result = out SInt(width bits)
  }

  val blocks = width / blockSize
  val carries = Vec(SInt(blockSize + 1 bits), blocks)
  val p = Vec(Bool(), blocks) // Propagate signals

  for (i <- 0 until blocks) {
    val start = i * blockSize
    val end = Math.min(start + blockSize, width)
    val aBlock = io.a(end - 1 downto start)
    val bBlock = io.b(end - 1 downto start)
    val sum = aBlock + bBlock + Mux(i === 0, io.cin.asSInt.resize(blockSize + 1), carries(i - 1))
    carries(i) := sum
    p(i) := (aBlock ^ bBlock).andR // Propagate if all bits propagate carry
  }

  io.result := Cat(carries.reverse).resize(width).asSInt
}

// Dual Adder with Speculative Paths and NDP
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

  val blockSize = 8
  val aBlocks = Vec(UInt(blockSize bits), 108 / blockSize)
  val bBlocks = Vec(UInt(blockSize bits), 108 / blockSize)
  val carries = Vec(SInt(blockSize + 1 bits), 108 / blockSize)
  val sums = Vec(Bits(blockSize bits), 108 / blockSize)
  for (i <- 0 until 108 / blockSize) {
    aBlocks(i) := io.a.mant.asUInt.resize(108 bits)(i * blockSize + blockSize - 1 downto i * blockSize)
    bBlocks(i) := io.b.mant.asUInt.resize(108 bits)(i * blockSize + blockSize - 1 downto i * blockSize)
    val sum = aBlocks(i).asSInt + bBlocks(i).asSInt + Mux(U(i) === U(0), io.cin.asSInt.resize(blockSize + 1), carries(i - 1)) // Fixed: i === 0
    carries(i) := sum(blockSize downto blockSize - 1).asSInt
    sums(i) := sum(blockSize - 1 downto 0)
  }
  val rawResult = Cat(sums.reverse).asUInt

  val p = Vec(Bool(), sums.length)
  val g = Vec(Bool(), sums.length)
  for (i <- 0 until sums.length) {
    p(i) := (aBlocks(i) ^ bBlocks(i)).orR
    g(i) := (aBlocks(i) & bBlocks(i)).orR
  }

  val potentialPoints = Vec(Bool(), sums.length)
  for (i <- 0 until sums.length) {
    potentialPoints(i) := g(i) | (p(i) & (U(i) < U(sums.length - 1) && (g(i + 1) | p(i + 1)))) // Fixed: hardware condition
  }
  val carryPoint = OHMasking.first(potentialPoints)

  val mantBits = rawResult.resize(54 bits)
  val sticky = Mux(io.isSingle, rawResult(29 downto 0).orR, rawResult(0)) // Fixed: rawResult(0) as Bool
  val roundUp = io.roundingMode.mux(
    RoundingMode.RNE -> (mantBits(1) && (mantBits(0) || mantBits(2))),
    RoundingMode.RTZ -> False,
    RoundingMode.RUP -> (sticky && !io.a.sign),
    RoundingMode.RDN -> (sticky && io.a.sign)
  )
  val roundedMant = mantBits.asUInt + roundUp.asUInt // Fixed: mantBits.asUInt
  io.result.sign := io.a.sign ^ io.b.sign
  io.result.exp := io.a.exp + io.b.exp - 1023
  io.result.mant := Mux(io.isSingle, roundedMant(51 downto 29).resize(52 bits), roundedMant(51 downto 0))
  io.flags.NV := False
  io.flags.NX := False
  io.flags.OF := False
  io.flags.UF := False
  io.flags.DZ := False
}