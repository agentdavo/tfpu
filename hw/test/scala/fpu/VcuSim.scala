package fpu

import spinal.core._
import spinal.core.sim._

object VCUSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new VCU).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 20)
      dut.clockDomain.waitSampling()

      // Test 1: NaN Input (FPDIV)
      dut.io.a.sign #= false
      dut.io.a.exp #= 2047
      dut.io.a.mant #= BigInt("1" + "0" * 51, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 1023
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.op #= FpuOp.FPDIV
      dut.io.isSingle #= false
      dut.clockDomain.waitSampling()
      val result1 = dut.io.result.payload.toBigInt
      assert(result1 == BigInt("7FF8000000000000", 16), s"NaN failed: $result1")
      assert(dut.io.flags.NV.toBoolean, "NV flag not set for NaN")

      // Test 2: Inf / Zero (single)
      dut.io.a.sign #= false
      dut.io.a.exp #= 255
      dut.io.a.mant #= BigInt("0" * 52, 2)
      dut.io.b.sign #= false
      dut.io.b.exp #= 0
      dut.io.b.mant #= BigInt("0" * 52, 2)
      dut.io.isSingle #= true
      dut.clockDomain.waitSampling()
      val result2 = dut.io.result.payload.toBigInt
      assert(result2 == BigInt("7F80000000000000", 16), s"Inf failed: $result2")
      assert(dut.io.flags.DZ.toBoolean, "DZ flag not set for div by zero")

      println("VCU tests passed!")
    }
  }
}