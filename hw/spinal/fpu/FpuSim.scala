package fpu

import spinal.core._
import spinal.core.sim._
import spinal.lib._

object FpuSim extends App {
  def runTests(config: FPUConfig) = SimConfig.withWave.compile {
    Fpu(config)
  }.doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)
    var cycleCount = 0
    dut.clockDomain.onSamplings { cycleCount += 1 }

    // Initialize memory with test values
    val mem = Array.fill(1024)(0L)
    mem(0) = 0x3F800000 // 1.0 single precision
    mem(4) = 0x00000000 // Low word of 1.0 double precision
    mem(5) = 0x3FF00000 // High word of 1.0 double precision
    mem(8) = 0x7FC00000 // Quiet NaN single precision
    mem(12) = 0x00000000 // Low word of quiet NaN double precision
    mem(13) = 0x7FF80000 // High word of quiet NaN double precision
    // LDALL/STALL test data (single precision stack)
    mem(20) = 0x00000004 // Status: roundingMode=0 (nearest), typeFPAreg=0, typeFPBreg=0, typeFPCreg=0
    mem(21) = 0x3F800000 // FPAreg: 1.0 (single)
    mem(22) = 0x40000000 // FPBreg: 2.0 (single)
    mem(23) = 0x40400000 // FPCreg: 3.0 (single)
    // LDALL/STALL test data (double precision stack)
    mem(30) = 0x0000001C // Status: roundingMode=0, typeFPAreg=1, typeFPBreg=1, typeFPCreg=1
    mem(31) = 0x00000000 // FPAreg: 1.0 (double, low)
    mem(32) = 0x3FF00000 // FPAreg: 1.0 (double, high)
    mem(33) = 0x00000000 // FPBreg: 2.0 (double, low)
    mem(34) = 0x40000000 // FPBreg: 2.0 (double, high)
    mem(35) = 0x00000000 // FPCreg: 3.0 (double, low)
    mem(36) = 0x40080000 // FPCreg: 3.0 (double, high)

    def resetAndRun(op: FpuOperation.E, inputA: Long, inputB: Long, expected: Long, desc: String, cycles: Int, memAddr: Long = 0): Unit = {
      dut.io.inputA #= inputA
      dut.io.inputB #= inputB
      dut.io.inputC #= 0L
      dut.io.operation #= op
      dut.io.memAddress #= memAddr
      cycleCount = 0
      waitUntil(dut.io.ready.toBoolean || dut.io.trapInterface.trapEnable.toBoolean)
      val resultHex = dut.io.result.toBigInt.toString(16)
      val expectedHex = BigInt(expected).toString(16)
      val trapCause = if (dut.io.trapInterface.trapEnable.toBoolean) dut.io.trapInterface.trapCause.toInt else 0
      println(s"$desc: Result = 0x$resultHex, Expected = 0x$expectedHex, Cycles = $cycleCount, Trap = $trapCause")
      if (!dut.io.trapInterface.trapEnable.toBoolean) {
        assert(dut.io.result.toBigInt == expected, s"$desc failed!")
        assert(cycleCount == cycles, s"$desc cycle count mismatch: expected $cycles, got $cycleCount")
      }
    }

    if (config.isSinglePrecision) {
      resetAndRun(FpuOperation.LDLNSN, 0, 0, 0x3F800000, "LDLNSN 1.0 (single)", 1, memAddr = 0)
      resetAndRun(FpuOperation.LDNLSNI, 0, 2, 0x3F800000, "LDNLSNI 1.0 (single, offset 2)", 1, memAddr = 0)
      resetAndRun(FpuOperation.LDNLDB, 0, 0, 0x3FF0000000000000L, "LDNLDB 1.0 (double)", 1, memAddr = 4)
      resetAndRun(FpuOperation.LDNLDBI, 0, 2, 0x3FF0000000000000L, "LDNLDBI 1.0 (double, offset 2)", 1, memAddr = 0)
      resetAndRun(FpuOperation.LDZEROSN, 0, 0, 0x00000000, "LDZEROSN 0.0 (single)", 1)
      resetAndRun(FpuOperation.LDZERODB, 0, 0, 0x0000000000000000L, "LDZERODB 0.0 (double)", 1)
      resetAndRun(FpuOperation.LDNLMULSN, 0x3F800000, 0, 0x40000000, "LDNLMULSN 1.0 * 1.0 (single)", 2, memAddr = 0)
      resetAndRun(FpuOperation.LDNLMULDB, 0x3FF0000000000000L, 0, 0x4000000000000000L, "LDNLMULDB 1.0 * 1.0 (double)", 3, memAddr = 4)
      // STALL and LDALL tests
      resetAndRun(FpuOperation.STALL, 0x3F800000, 0x40000000, 0x40400000, "STALL Save Stack (single)", 7, memAddr = 20) // 1.0, 2.0, 3.0
      resetAndRun(FpuOperation.LDALL, 0, 0, 0x3F800000, "LDALL Load Stack (single)", 7, memAddr = 20) // Expect FPAreg = 1.0
      resetAndRun(FpuOperation.LDALL, 1, 0, 0, "LDALL Unaligned (single)", 1, memAddr = 21) // Should trap
    } else {
      resetAndRun(FpuOperation.LDLNSN, 0, 0, 0x3F800000, "LDLNSN 1.0 (double)", 1, memAddr = 0)
      resetAndRun(FpuOperation.LDNLSNI, 0, 2, 0x3F800000, "LDNLSNI 1.0 (double, offset 2)", 1, memAddr = 0)
      resetAndRun(FpuOperation.LDNLDB, 0, 0, 0x3FF0000000000000L, "LDNLDB 1.0 (double)", 1, memAddr = 4)
      resetAndRun(FpuOperation.LDNLDBI, 0, 2, 0x3FF0000000000000L, "LDNLDBI 1.0 (double, offset 2)", 1, memAddr = 0)
      resetAndRun(FpuOperation.LDZEROSN, 0, 0, 0x00000000, "LDZEROSN 0.0 (double)", 1)
      resetAndRun(FpuOperation.LDZERODB, 0, 0, 0x0000000000000000L, "LDZERODB 0.0 (double)", 1)
      resetAndRun(FpuOperation.LDNLMULSN, 0x3F800000, 0, 0x40000000, "LDNLMULSN 1.0 * 1.0 (double)", 2, memAddr = 0)
      resetAndRun(FpuOperation.LDNLMULDB, 0x3FF0000000000000L, 0, 0x4000000000000000L, "LDNLMULDB 1.0 * 1.0 (double)", 3, memAddr = 4)
      // STALL and LDALL tests
      resetAndRun(FpuOperation.STALL, 0x3FF0000000000000L, 0x4000000000000000L, 0x4008000000000000L, "STALL Save Stack (double)", 7, memAddr = 30) // 1.0, 2.0, 3.0
      resetAndRun(FpuOperation.LDALL, 0, 0, 0x3FF0000000000000L, "LDALL Load Stack (double)", 7, memAddr = 30) // Expect FPAreg = 1.0
      resetAndRun(FpuOperation.LDALL, 1, 0, 0, "LDALL Unaligned (double)", 1, memAddr = 31) // Should trap
    }
  }

  runTests(FPUConfig(32))
  runTests(FPUConfig(64))
}