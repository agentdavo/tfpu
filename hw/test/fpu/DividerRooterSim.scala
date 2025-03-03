package fpu

import spinal.core._
import spinal.core.sim._

object DividerRooterSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new DividerRooter).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 20)
      dut.clockDomain.waitSampling()

      // Test 1: Div 4.0 / 2.0 (double)
      dut.io.a.sign #= false
      dut.io.a.exp #= 1025
      dut.io.a.mant #= BigInt("0" * 52, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 1024
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.isDiv #= true
      dut.io.isSingle #= false
      dut.io.roundingMode #= RoundingMode.RNE
      dut.clockDomain.waitSampling(15)
      val result1 = dut.io.result.asBits.toBigInt  // Changed to asBits
      assert(result1 == BigInt("4000000000000000", 16), s"Div double failed: $result1")

      // Test 2: Sqrt 16.0 (single)
      dut.io.a.sign #= false
      dut.io.a.exp #= 130
      dut.io.a.mant #= BigInt("0" * 52, 2)
      dut.io.isDiv #= false
      dut.io.isSingle #= true
      dut.clockDomain.waitSampling(7)
      val result2 = dut.io.result.asBits.toBigInt  // Changed to asBits
      assert(result2 == BigInt("4080000000000000", 16), s"Sqrt single failed: $result2")

      println("DividerRooter tests passed!")
    }
  }
}