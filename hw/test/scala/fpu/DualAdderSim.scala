package fpu

import spinal.core._
import spinal.core.sim._

object DualAdderSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new DualAdder).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 20)
      dut.clockDomain.waitSampling()

      // Test 1: Add 1.0 + 2.0 (double)
      dut.io.a.sign #= false
      dut.io.a.exp #= 1023
      dut.io.a.mant #= BigInt("0" * 52, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 1024
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.isSub #= false
      dut.io.roundingMode #= RoundingMode.RNE
      dut.io.isSingle #= false
      dut.clockDomain.waitSampling(2)
      val result1 = dut.io.result.payload.toBigInt
      assert(result1 == BigInt("4008000000000000", 16), s"Add failed: $result1")

      // Test 2: Sub 3.0 - 1.0 (single)
      dut.io.a.sign #= false
      dut.io.a.exp #= 128
      dut.io.a.mant #= BigInt("0" * 52, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 126
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.isSub #= true
      dut.io.isSingle #= true
      dut.clockDomain.waitSampling(2)
      val result2 = dut.io.result.payload.toBigInt
      assert(result2 == BigInt("4000000000000000", 16), s"Sub failed: $result2")

      println("DualAdder tests passed!")
    }
  }
}