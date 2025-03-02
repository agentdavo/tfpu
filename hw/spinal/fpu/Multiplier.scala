// SPDX-FileCopyrightText: 2025 David Smith <david.smith@linux.com>
// SPDX-License-Identifier: MIT

package fpu

import spinal.core._
import spinal.lib._

class Multiplier extends Component {
  val io = new Bundle {
    val a = in(Fp64())           // First operand (FA)
    val b = in(Fp64())           // Second operand (FB)
    val isSingle = in Bool()     // True for single-precision (FPMUL_S), False for double (FPMUL_D)
    val roundingMode = in(RoundingMode()) // T9000 rounding mode (RNE, RTZ, RUP, RDN)
    val result = out(Fp64())     // Result pushed to stack
    val flags = out(FpuFlags())  // T9000 exception flags
  }

  // Booth multiplier for raw product
  val booth = new BoothMultiplier(io.a.asBits, io.b.asBits)
  val mantA = booth.io.mantA.resize(108 bits) // Extended for precision
  val mantB = booth.io.mantB.resize(108 bits)

  // CSA reduction for multiplication
  val csa1 = Vec(UInt(108 bits), 5)
  val csa2 = Vec(UInt(108 bits), 5)
  for (i <- 0 to 4) {
    val s1 = mantA(107 - i * 2 downto 0).asUInt
    val c1 = (mantA(107 - i * 2 - 1 downto 0) & mantB).resize(108 bits).asUInt
    val s2 = mantB(107 - i * 2 downto 0).asUInt
    val c2 = (mantB(107 - i * 2 - 1 downto 0) & mantA).resize(108 bits).asUInt
    csa1(i) := s1 + c1
    csa2(i) := s2 + c2
  }
  val sum1 = csa1.reduceBalancedTree(_ + _)
  val sum2 = csa2.reduceBalancedTree(_ + _)
  val rawProd = sum1 + sum2 // Raw product (108 bits)

  // Normalize and extract mantissa
  val normShift = rawProd(107 downto 106) === U"01" // Check if normalized (1.xxxx or 0.1xxx)
  val mantBits = Mux(normShift, rawProd(105 downto 0), rawProd(106 downto 0)).resize(54 bits)
  val expAdjust = Mux(normShift, S(-1, 11 bits), S(0, 11 bits))

  // Rounding per T9000 (ISM 11.12.1)
  val guard = Mux(io.isSingle, mantBits(29), mantBits(0)) // Guard bit
  val sticky = Mux(io.isSingle, mantBits(28 downto 0).orR, False) // Sticky bit for double
  val roundUp = io.roundingMode.mux(
    RoundingMode.RNE -> (guard && (sticky || mantBits(30))),
    RoundingMode.RTZ -> False,
    RoundingMode.RUP -> (guard || sticky) && !io.a.sign,
    RoundingMode.RDN -> (guard || sticky) && io.a.sign
  )
  val mantNorm = mantBits.asUInt.resize(53 bits) // Include implicit bit
  val roundedMant = mantNorm + roundUp.asUInt
  val mantOverflow = roundedMant(53) // Check for mantissa overflow (shifts exponent)

  // Exponent calculation
  val rawExp = io.a.exp.asSInt + io.b.exp.asSInt - S(1023, 11 bits) + expAdjust
  val finalExp = rawExp + mantOverflow.asSInt

  // Exception flags (ISM 11.13)
  val isNan = io.a.isNaN || io.b.isNaN || (io.a.isInf && io.b.isZero) || (io.a.isZero && io.b.isInf)
  val isInf = (io.a.isInf && !io.b.isZero) || (io.b.isInf && !io.a.isZero)
  val isZero = io.a.isZero || io.b.isZero
  val overflow = finalExp > S(2046, 11 bits) && !isNan && !isInf
  val underflow = finalExp < S(0, 11 bits) && !isNan && !isZero
  val inexact = guard || sticky

  // Result assembly
  io.result.sign := io.a.sign ^ io.b.sign
  io.result.exp := Mux(overflow, U(2047, 11 bits), Mux(underflow || isZero, U(0, 11 bits), finalExp.asUInt.resize(11 bits)))
  io.result.mant := Mux(isNan || isInf, B(0, 52 bits), // NaN/Inf have zero mantissa per T9000
                        Mux(io.isSingle, roundedMant(51 downto 29).resize(52 bits), roundedMant(51 downto 0)))

  // Flags per T9000 spec
  io.flags.NV := isNan
  io.flags.NX := inexact && !isNan && !isInf
  io.flags.OF := overflow
  io.flags.UF := underflow
  io.flags.DZ := False // Multiplication doesn’t set DZ
}