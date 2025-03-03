package fpu

import spinal.core._
import spinal.core.sim._
import spinal.lib._

object FpuSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new FPU).doSim { dut =>  // Changed Fpu to FPU
      dut.clockDomain.forkStimulus(period = 20)
      dut.clockDomain.waitSampling()

      dut.io.cmd.valid #= false
      dut.io.memDataIn #= 0
      dut.io.memAddr #= 0
      dut.io.memWrite #= false

      def executeOp(op: FpuOp.E, memData: BigInt, cycles: Int): BigInt = {
        dut.io.cmd.valid #= true
        dut.io.cmd.payload #= op
        dut.io.memDataIn #= memData
        dut.clockDomain.waitSampling()
        dut.io.cmd.valid #= false
        dut.clockDomain.waitSampling(cycles - 1)
        assert(dut.io.result.valid.toBoolean, s"$op did not complete")
        dut.io.result.payload.toBigInt  // Flow[Bits] already has payload
      }

      // Test 1: FPADD (1.0 + 2.0 = 3.0, double precision)
      executeOp(FpuOp.FPLDNLDB, BigInt("3FF0000000000000", 16), 1) // Push 1.0
      executeOp(FpuOp.FPLDNLDB, BigInt("4000000000000000", 16), 1) // Push 2.0
      val addResult = executeOp(FpuOp.FPADD_D, 0, 2)                // Add, expect 3.0
      assert(addResult == BigInt("4008000000000000", 16), s"FPADD failed: $addResult")

      // Test 2: FPMUL (2.0 * 3.0 = 6.0, single precision)
      executeOp(FpuOp.FPLDNLSN, BigInt("40000000", 16), 1)          // Push 2.0 single
      executeOp(FpuOp.FPLDNLSN, BigInt("40400000", 16), 1)          // Push 3.0 single
      val mulResult = executeOp(FpuOp.FPMUL_D, 0, 2)                // Mul, expect 6.0 (double output)
      assert(mulResult == BigInt("40C0000000000000", 16), s"FPMUL failed: $mulResult")

      // Test 3: FPDIV (4.0 / 2.0 = 2.0, double precision)
      executeOp(FpuOp.FPLDNLDB, BigInt("4010000000000000", 16), 1)  // Push 4.0
      executeOp(FpuOp.FPLDNLDB, BigInt("4000000000000000", 16), 1)  // Push 2.0
      val divResult = executeOp(FpuOp.FPDIV_D, 0, 15)               // Div, expect 2.0
      assert(divResult == BigInt("4000000000000000", 16), s"FPDIV failed: $divResult")

      // Test 4: NaN Handling (0.0 / 0.0)
      executeOp(FpuOp.FPLDZERODB, 0, 1)                            // Push 0.0
      executeOp(FpuOp.FPLDZERODB, 0, 1)                            // Push 0.0
      val nanResult = executeOp(FpuOp.FPDIV_D, 0, 15)              // Div, expect NaN
      assert(dut.io.flags.NV.toBoolean, "NV flag not set for NaN")
      assert(nanResult == BigInt("7FF8000000000000", 16), s"NaN failed: $nanResult")

      // Test 5: FPSTNLSN (Store single)
      executeOp(FpuOp.FPLDNLSN, BigInt("40400000", 16), 1)         // Push 3.0 single
      executeOp(FpuOp.FPSTNLSN, 0, 1)                             // Store
      assert(dut.io.memDataOut.toBigInt == BigInt("40400000", 16), "FPSTNLSN failed")

      println("All tests passed!")
    }
  }
}