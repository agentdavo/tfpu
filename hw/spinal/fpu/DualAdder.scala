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
    val a, b = in(Fp64())
    val isSub = in Bool()
    val roundingMode = in(RoundingMode())
    val isSingle = in Bool()
    val result = out(Fp64())
    val flags = out(FpuFlags())
  }

  // Exponent alignment
  val expDiff = io.a.exp.asSInt - io.b.exp.asSInt
  val largerExp = Mux(expDiff >= 0, io.a.exp, io.b.exp)
  val shiftAmount = expDiff.abs.resize(6 bits)
  val mantA = Cat(True, io.a.mant).asUInt
  val mantB = Cat(True, io.b.mant).asUInt
  val alignedMantA = Mux(expDiff >= 0, mantA, mantA >> shiftAmount)
  val alignedMantB = Mux(expDiff < 0, mantB, mantB >> shiftAmount)

  // Speculative addition/subtraction with radix-4 carry-skip
  val effSub = io.a.sign ^ io.b.sign ^ io.isSub
  val adder1 = new CarrySkipAdder(57, 4)
  val adder2 = new CarrySkipAdder(57, 4)
  adder1.io.a := Cat(io.a.sign.asSInt.resize(4), alignedMantA).asSInt
  adder1.io.b := Cat((io.b.sign ^ io.isSub).asSInt.resize(4), alignedMantB).asSInt
  adder1.io.cin := effSub
  adder2.io.a := Cat(io.a.sign.asSInt.resize(4), alignedMantA).asSInt
  adder2.io.b := Cat((io.b.sign ^ io.isSub).asSInt.resize(4), alignedMantB).asSInt + 1
  adder2.io.cin := effSub
  val sumNoOv = adder1.io.result
  val sumOv = adder2.io.result

  // Normalization Distance Predictor (NDP)
  val tripletsA = alignedMantA.asBits.subdivideIn(3 bits)
  val tripletsB = alignedMantB.asBits.subdivideIn(3 bits)
  val g = tripletsA.zip(tripletsB).map { case (a, b) => a & b }
  val p = tripletsA.zip(tripletsB).map { case (a, b) => a ^ b }
  val k = tripletsA.zip(tripletsB).map { case (a, b) => ~a & ~b }
  val potentialPoints = Vec(Bool(), g.length)
  for (i <- 0 until g.length) {
    potentialPoints(i) := g(i) | (p(i) & (i + 1 < g.length && (g(i + 1) | p(i + 1))))
  }
  val normShift = OHToUInt(OHMasking.first(potentialPoints.reverse.padTo(19, False)))
  val needsNorm = sumNoOv.msb && !effSub

  // Result normalization
  val rawResult = Mux(needsNorm, sumNoOv << normShift, sumOv)
  val finalExp = largerExp + (needsNorm ? U(0) | U(1)) - normShift
  val mantBits = Mux(io.isSingle, rawResult(54 downto 31), rawResult(54 downto 2))

  // Rounding
  val guard = Mux(io.isSingle, rawResult(30), rawResult(1))
  val sticky = Mux(io.isSingle, rawResult(29 downto 0).orR, rawResult(0).orR)
  val roundBit = Mux(io.isSingle, rawResult(31), rawResult(2))
  val roundUp = io.roundingMode.mux(
    RoundingMode.RNE -> (guard && (sticky || roundBit)),
    RoundingMode.RTZ -> False,
    RoundingMode.RDN -> (rawResult.msb && (guard || sticky)),
    RoundingMode.RUP -> (!rawResult.msb && (guard || sticky))
  )
  val roundedMant = mantBits + roundUp.asUInt

  io.result.sign := rawResult.msb
  io.result.exp := finalExp
  io.result.mant := Mux(io.isSingle, Cat(roundedMant(22 downto 0), B(0, 29 bits)), roundedMant(51 downto 0))
  io.flags.NV := io.a.isNaN || io.b.isNaN || (io.a.isInf && io.b.isInf && io.isSub)
  io.flags.NX := guard || sticky
  io.flags.OF := finalExp > (io.isSingle ? U(254) | U(2046))
  io.flags.UF := finalExp < 1 && !io.result.isZero
  io.flags.DZ := False
}