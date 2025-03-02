package fpu

import spinal.core._
import spinal.core.sim._

object MultiplierSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Multiplier).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 20)
      dut.clockDomain.waitSampling()

      // Test 1: Mul 2.0 * 3.0 (double)
      dut.io.a.sign #= false
      dut.io.a.exp #= 1024
      dut.io.a.mant #= BigInt("0" * 52, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 1025
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.isSingle #= false
      dut.io.roundingMode #= RoundingMode.RNE
      dut.clockDomain.waitSampling(3)
      val result1 = dut.io.result.toBigInt
      assert(result1 == BigInt("4018000000000000", 16), s"Mul double failed: $result1")

      // Test 2: Mul 1.5 * 2.0 (single)
      dut.io.a.sign #= false
      dut.io.a.exp #= 127
      dut.io.a.mant #= BigInt("1" + "0" * 51, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 128
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.isSingle #= true
      dut.clockDomain.waitSampling(2)
      val result2 = dut.io.result.toBigInt
      assert(result2 == BigInt("4040000000000000", 16), s"Mul single failed: $result2")

      println("Multiplier tests passed!")
    }
  }
}