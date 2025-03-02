// SPDX-FileCopyrightText: 2025 David Smith <david.smith@linux.com>
// SPDX-License-Identifier: MIT

package fpu

import spinal.core._
import spinal.lib._

// Multiplier implementing T9000 FPU radix-4 Booth recoding and twin 7:2 carry-save arrays
// Supports single (32-bit) and double (64-bit) precision per Knowles Section 5
class Multiplier extends Component {
  // Input/Output interface
  val io = new Bundle {
    val a, b         = in(Fp64())         // Operands A and B (64-bit IEEE-754)
    val isSingle     = in Bool()          // Precision selector: True for single, False for double
    val roundingMode = in(RoundingMode()) // IEEE-754 rounding mode (RTZ, RNE, RUP, RDN)
    val result       = out(Fp64())        // Result output (64-bit IEEE-754)
    val flags        = out(FpuFlags())    // Exception flags (NV, NX, OF, UF, DZ)
  }

  // Booth Recoding (Radix-4, Knowles Section 5.1)
  val mantA = Cat(True, io.a.mant).asUInt // 53-bit mantissa A with leading 1
  val mantB = Cat(True, io.b.mant).asUInt // 53-bit mantissa B with leading 1
  val boothDigits = Vec(SInt(3 bits), 27) // 27 digits for 53-bit mantissa (ceil(53/2) + 1)

  for (i <- 0 until 27) {
    val highBit = (2 * i + 1) min 52
    val lowBit = (2 * i) min 52
    val prevBit = if (i == 0) False else mantB((2 * i - 1) min 52)
    val bits = Cat(mantB(highBit), mantB(lowBit), prevBit).asUInt
    boothDigits(i) := bits.mux(
      U"000" -> S(0, 3 bits), U"001" -> S(1, 3 bits), U"010" -> S(1, 3 bits), U"011" -> S(2, 3 bits),
      U"100" -> S(-2, 3 bits), U"101" -> S(-1, 3 bits), U"110" -> S(-1, 3 bits), U"111" -> S(0, 3 bits)
    )
  }

  // Partial Products (27 for double precision, Knowles Section 5.1)
  val partials = Vec(Bits(108 bits), 27)
  for (i <- 0 until 27) {
    val pp = boothDigits(i).mux(
      S(0, 3 bits)  -> U(0, 108 bits),
      S(1, 3 bits)  -> mantA.resize(108),
      S(2, 3 bits)  -> (mantA << 1).resize(108),
      S(-1, 3 bits) -> (~mantA).resize(108),
      S(-2, 3 bits) -> (~(mantA << 1)).resize(108)
    )
    partials(i) := (pp << (2 * i)).asBits
  }

  // Twin 7:2 Carry-Save Arrays (5 rows deep each, Knowles Section 5.1)
  def csa3to2(a: Bits, b: Bits, c: Bits): (Bits, Bits) = {
    val sum   = a ^ b ^ c
    val carry = (a & b) | (b & c) | (a & c)
    (sum, carry << 1)
  }
  val array1 = Vec(partials.take(14)) // First 14 partials
  val array2 = Vec(partials.drop(14) ++ Vec.fill(13)(B(0, 108 bits))) // Last 13 + padding
  val csa1   = Vec(Bits(108 bits), 5)
  val csa2   = Vec(Bits(108 bits), 5)
  
  for (i <- 0 until 5) {
    val (s1, c1) = csa3to2(array1(3 * i), array1(3 * i + 1), array1(3 * i + 2))
    val (s2, c2) = csa3to2(array2(3 * i), array2(3 * i + 1), if (3 * i + 2 < 13) array2(3 * i + 2) else B(0, 108 bits))
    csa1(i) := s1 + c1
    csa2(i) := s2 + c2
  }
  val sum1 = csa1.reduceBalancedTree(_ + _) // Reduce 5 sums to 1
  val sum2 = csa2.reduceBalancedTree(_ + _) // Reduce 5 sums to 1
  val prod = sum1 + sum2                    // Final product

  // Normalization and Rounding (Knowles Section 5)
  val needsShift = prod(106)
  val prodMant = Mux(needsShift, prod(106 downto 54), prod(105 downto 53))
  val expBase = io.a.exp + io.b.exp - Mux(io.isSingle, U(127, 11 bits), U(1023, 11 bits))
  val finalExp = expBase + needsShift.asUInt
  val guard = Mux(needsShift, prod(53), prod(52))
  val sticky = Mux(needsShift, prod(52 downto 0).orR, prod(51 downto 0).orR)
  val roundBit = Mux(io.isSingle, prodMant(23), prodMant(52))
  val roundUp = io.roundingMode.mux(
    RoundingMode.RNE -> (guard && (sticky || roundBit)),
    RoundingMode.RTZ -> False,
    RoundingMode.RDN -> (io.a.sign ^ io.b.sign && (guard || sticky)),
    RoundingMode.RUP -> (!(io.a.sign ^ io.b.sign) && (guard || sticky))
  )
  val roundedMant = prodMant + roundUp.asUInt

  // Final result
  io.result.sign := io.a.sign ^ io.b.sign
  io.result.exp := finalExp
  io.result.mant := Mux(io.isSingle, roundedMant(51 downto 29), roundedMant(51 downto 0))

  // Exception flags
  io.flags.NV := io.a.isNaN || io.b.isNaN || (io.a.isZero && io.b.isInf)
  io.flags.NX := guard || sticky
  io.flags.OF := finalExp > Mux(io.isSingle, U(254, 11 bits), U(2046, 11 bits))
  io.flags.UF := finalExp < 1 && !io.result.isZero
  io.flags.DZ := False
}