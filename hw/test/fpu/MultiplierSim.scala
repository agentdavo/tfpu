package fpu

import spinal.core._
import spinal.core.sim._

object MultiplierSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Multiplier).doSim { dut =>
      // Start the clock with a 10ns period
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling()  // Wait for initial clock edge

      // Helper function to test multiplication with cycle-by-cycle reporting
      def testMul(a: BigInt, b: BigInt, expected: BigInt, testName: String, isSingle: Boolean): Unit = {
        // Assign inputs (assuming Fp64 format: 1-bit sign, 11-bit exp, 52-bit mantissa)
        dut.io.a.sign #= (a & (1L << 63)) != 0
        dut.io.a.exp #= (a >> 52) & 0x7FF
        dut.io.a.mant #= a & 0xFFFFFFFFFFFFFL
        dut.io.b.sign #= (b & (1L << 63)) != 0
        dut.io.b.exp #= (b >> 52) & 0x7FF
        dut.io.b.mant #= b & 0xFFFFFFFFFFFFFL
        dut.io.isSingle #= isSingle
        dut.io.roundingMode #= RoundingMode.RNE  // Round to nearest, ties to even

        // Set expected cycle count based on precision
        val expectedCycles = if (isSingle) 2 else 3

        println(s"--- Starting $testName ---")
        println(s"Inputs: a = ${a.toString(16)}, b = ${b.toString(16)}, isSingle = $isSingle")
        println(s"Expected result: ${expected.toString(16)} after $expectedCycles cycles")

        // Simulate cycle-by-cycle
        for (cycle <- 1 to expectedCycles) {
          dut.clockDomain.waitSampling(1)  // Advance one clock cycle

          // Reconstruct the current output from sign, exp, and mantissa
          val sign = if (dut.io.result.sign.toBoolean) BigInt(1) else BigInt(0)
          val exp = dut.io.result.exp.toBigInt
          val mant = dut.io.result.mant.toBigInt
          val result = (sign << 63) | (exp << 52) | mant

          // Report the result for this cycle
          println(s"Cycle $cycle: result = ${result.toString(16)}")
        }

        // Check the final result after all cycles
        val finalResult = (if (dut.io.result.sign.toBoolean) BigInt(1) else BigInt(0)) << 63 |
                          (dut.io.result.exp.toBigInt << 52) |
                          dut.io.result.mant.toBigInt
        if (finalResult == expected) {
          println(s"$testName passed: ${finalResult.toString(16)}")
        } else {
          println(s"$testName failed: got ${finalResult.toString(16)}, expected ${expected.toString(16)}")
        }
        println("--- End of Test ---\n")
      }

      // Test cases
      // Test 1: 2.0 * 3.0 = 6.0 (double-precision, 3 cycles)
      testMul(
        BigInt("4000000000000000", 16),  // 2.0
        BigInt("4008000000000000", 16),  // 3.0
        BigInt("4018000000000000", 16),  // 6.0
        "Test 1: 2.0 * 3.0 (double)", false
      )

      // Test 2: -2.0 * 3.0 = -6.0 (double-precision, 3 cycles)
      testMul(
        BigInt("c000000000000000", 16),  // -2.0
        BigInt("4008000000000000", 16),  // 3.0
        BigInt("c018000000000000", 16),  // -6.0
        "Test 2: -2.0 * 3.0 (double)", false
      )

      // Test 3: 0.0 * 1.0 = 0.0 (double-precision, 3 cycles)
      testMul(
        BigInt("0000000000000000", 16),  // 0.0
        BigInt("3ff0000000000000", 16),  // 1.0
        BigInt("0000000000000000", 16),  // 0.0
        "Test 3: 0.0 * 1.0 (double)", false
      )

      // Test 4: Infinity * 1.0 = Infinity (double-precision, 3 cycles)
      testMul(
        BigInt("7ff0000000000000", 16),  // Infinity
        BigInt("3ff0000000000000", 16),  // 1.0
        BigInt("7ff0000000000000", 16),  // Infinity
        "Test 4: Infinity * 1.0 (double)", false
      )

      // Test 5: NaN * 1.0 = NaN (double-precision, 3 cycles)
      val a5 = BigInt("7ff8000000000000", 16)  // NaN
      val b5 = BigInt("3ff0000000000000", 16)  // 1.0
      dut.io.a.sign #= (a5 & (1L << 63)) != 0
      dut.io.a.exp #= (a5 >> 52) & 0x7FF
      dut.io.a.mant #= a5 & 0xFFFFFFFFFFFFFL
      dut.io.b.sign #= (b5 & (1L << 63)) != 0
      dut.io.b.exp #= (b5 >> 52) & 0x7FF
      dut.io.b.mant #= b5 & 0xFFFFFFFFFFFFFL
      dut.io.isSingle #= false
      dut.io.roundingMode #= RoundingMode.RNE
      println("--- Starting Test 5: NaN * 1.0 (double) ---")
      println(s"Inputs: a = ${a5.toString(16)}, b = ${b5.toString(16)}, isSingle = false")
      for (cycle <- 1 to 3) {
        dut.clockDomain.waitSampling(1)
        val result = (if (dut.io.result.sign.toBoolean) BigInt(1) else BigInt(0)) << 63 |
                     (dut.io.result.exp.toBigInt << 52) |
                     dut.io.result.mant.toBigInt
        println(s"Cycle $cycle: result = ${result.toString(16)}")
      }
      val finalResult = (if (dut.io.result.sign.toBoolean) BigInt(1) else BigInt(0)) << 63 |
                        (dut.io.result.exp.toBigInt << 52) |
                        dut.io.result.mant.toBigInt
      val isNaN = (finalResult & BigInt("7ff0000000000000", 16)) == BigInt("7ff0000000000000", 16) &&
                  (finalResult & BigInt("000fffffffffffff", 16)) != 0
      println(if (isNaN) "Test 5: NaN * 1.0 passed" else s"Test 5: NaN * 1.0 failed: got ${finalResult.toString(16)}")
      println("--- End of Test ---\n")

      // Test 6: 2.0f * 3.0f = 6.0f (single-precision, 2 cycles)
      testMul(
        BigInt("4000000000000000", 16),  // 2.0 (treated as single-precision)
        BigInt("4040000000000000", 16),  // 3.0 (treated as single-precision)
        BigInt("40c0000000000000", 16),  // 6.0 (treated as single-precision)
        "Test 6: 2.0f * 3.0f (single)", true
      )

      println("Simulation complete!")
    }
  }
}