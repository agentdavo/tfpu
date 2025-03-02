package fpu

import spinal.core._
import spinal.lib._

case class FPUConfig(precision: Int = 64) {
  val isSinglePrecision = precision == 32
  val mantissaWidth = if (isSinglePrecision) 23 else 52
  val exponentWidth = if (isSinglePrecision) 8 else 11
  val totalWidth = 1 + exponentWidth + mantissaWidth
  val bias = if (isSinglePrecision) 127 else 1023
  val maxExponent = (1 << exponentWidth) - 1
  val minExponent = -bias + 1
  val clockFreqMHz = 50
}