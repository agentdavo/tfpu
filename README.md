# FPU – A Modular Floating-Point Unit

This project implements an IEEE‑754 compliant 64‑bit floating‑point unit (FPU). Our design uses Scala and SpinalHDL for elaboration and hardware description, while employing a modular plugin architecture. This modularity is achieved with plugins, fibers, retainers, a shared database, and a dynamic pipeline API.

> **Note:** This README assumes you have basic familiarity with HDL, Scala, and SpinalHDL.

---

## Overview

The FPU design is decomposed into self‑contained modules (plugins), each responsible for a specific function. The main components include:

- **Configuration & Data Types:** Define precision, mantissa/exponent widths, and IEEE‑754 formats.
- **Functional Units:** Adder/Subtractor, Multiplier, and Divider/Square Root units implemented as plugins.
- **Microcode Control:** A microcode ROM and shared database control the operation flow.
- **Status and Error Handling:** A dedicated plugin monitors rounding modes and error conditions.
- **Pipeline Infrastructure:** A dynamic multi‑stage pipeline manages valid/ready handshaking and retiming.
- **Shared Database:** Provides global configuration parameters during elaboration.

---

## Key Technologies

- **Scala:** Drives elaboration and parameterization.
- **SpinalHDL:** Provides a high-level API for hardware description.
- **Plugins:** Modular units that inject hardware functionality into the FPU.
- **Fibers & Retainers:** Allow asynchronous elaboration and synchronization between plugins.
- **Database:** Holds shared elaboration‑time variables (e.g., precision, widths).
- **Pipeline API:** Dynamically pipelines data between stages.
- **Logic Synthesis Tools:** Generate optimal decoders using algorithms like Quine–McCluskey.

---

## Code Snippets

### Configuration and Data Types

```scala
case class FPUConfig(precision: Int = 64) {
  val isSinglePrecision = precision == 32
  val mantissaWidth    = if (isSinglePrecision) 23 else 52
  val exponentWidth    = if (isSinglePrecision) 8  else 11
  val totalWidth       = 1 + exponentWidth + mantissaWidth
  val bias             = if (isSinglePrecision) 127 else 1023
  val maxExponent      = (1 << exponentWidth) - 1
  val minExponent      = -bias + 1
  val clockFreqMHz     = 50
}

case class FloatData(config: FPUConfig) extends Bundle {
  val sign     = Bool()
  val exponent = UInt(config.exponentWidth bits)
  val mantissa = UInt(config.mantissaWidth bits)

  def asBits: Bits = Cat(sign, exponent, mantissa)
  def fromBits(bits: Bits): FloatData = {
    val result = FloatData(config)
    result.sign     := bits(config.totalWidth - 1)
    result.exponent := bits(config.totalWidth - 2 downto config.mantissaWidth)
    result.mantissa := bits(config.mantissaWidth - 1 downto 0)
    result
  }
  def isZero: Bool = exponent === 0 && mantissa === 0
  def isNaN: Bool = exponent === U(config.maxExponent) && mantissa =/= 0
  def normalizedMantissa: UInt = Cat(True, mantissa)
}
```

### Plugin Architecture

Each functional unit is encapsulated in a plugin. For example, a fixed‑output plugin:

```scala
class FixedOutputPlugin extends FiberPlugin {
  val logic = during build new Area {
    val port = out UInt(8 bits)
    port := 42
  }
}
```

And a plugin that uses a shared database:

```scala
object Global extends AreaObject {
  // A blocking elaboration-time variable that plugins can access.
  val VIRTUAL_WIDTH = Database.blocking[Int]
}

class MmuPlugin extends FiberPlugin {
  val logic = during build new Area {
    Global.VIRTUAL_WIDTH.set(39)
  }
}
```

### Pipeline Integration

Our design leverages a dynamic pipeline API. Here’s a simplified example that sums two inputs, squares the sum, and outputs the result:

```scala
class PipelineExample extends Component {
  val a, b   = in UInt(8 bits)
  val result = out UInt(16 bits)
  val pip    = new StagePipeline

  // Stage 0: Insert inputs
  val A = pip(0).insert(a)
  val B = pip(0).insert(b)

  // Stage 1: Compute sum
  val SUM = pip(1).insert(pip(1)(A) + pip(1)(B))

  // Stage 2: Square the sum
  val onSquare = new pip.Area(2) {
    val VALUE = insert(SUM * SUM)
  }

  // Stage 3: Output the result
  result := pip(3)(onSquare.VALUE)
  pip.build()
}

object PipelineExampleGen extends App {
  SpinalVerilog(new PipelineExample)
}
```

### Microcode Control

The microcode ROM plugin fetches micro‑instructions that drive FPU operations:

```scala
class MicrocodeROMPlugin(config: FPUConfig) extends FiberPlugin {
  val logic = during build new Area {
    val io = new Bundle {
      val operation   = in(FpuOperation())
      val address     = in UInt(6 bits)
      val instruction = out(MicrocodeInstruction())
    }
    
    val rom = Mem(MicrocodeInstruction(), 128)
    // Load sequences from a shared microcode database
    rom.initialContent = FpuDatabase.microcodeSequences.getOrElse(io.operation, Seq()).padTo(128, MicrocodeInstruction().init()).toArray
    io.instruction := rom.readSync(io.address)
  }
}
```

---

## Elaboration Flow

Our FPU design uses a multi‑stage pipeline controlled by the following stages:

1. **FETCH:** Collects operands, operation code, memory addresses, and status.
2. **DECODE:** The VCU checks operands for special IEEE‑754 cases.
3. **MICROCODE:** Micro‑instructions are fetched from the ROM to direct the next operations.
4. **NORMALIZE:** The normalizer plugin aligns the mantissa and adjusts the exponent.
5. **ROUND:** Rounding logic applies the current rounding mode.
6. **WRITE:** Final result is output, updating status and the evaluation stack.

Data is passed between stages via payload structures that encapsulate both data and status. This distributed approach simplifies retiming and valid/ready arbitration.

---

## Testing and Simulation

We use SpinalHDL’s built‑in simulation features with SBT to verify FPU operations. For example:

```scala
object FpuSim extends App {
  def runTests(config: FPUConfig) = SimConfig.withVerilator.compile(new Fpu(config)).doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)
    var cycleCount = 0
    dut.clockDomain.onSamplings { cycleCount += 1 }

    def resetAndRun(op: FpuOperation.E, inputA: Long, inputB: Long, expected: Long, desc: String, cycles: Int): Unit = {
      dut.io.inputA #= inputA
      dut.io.inputB #= inputB
      dut.io.inputC #= 0L
      dut.io.operation #= op
      dut.io.memAddress #= 0
      cycleCount = 0
      waitUntil(dut.io.ready.toBoolean)
      println(s"$desc: Result = 0x${dut.io.result.toBigInt.toString(16)}, Expected = 0x${expected.toString(16)}, Cycles = $cycleCount")
      assert(dut.io.result.toBigInt == expected, s"$desc failed!")
      assert(cycleCount == cycles, s"$desc cycle count mismatch: expected $cycles, got $cycleCount")
    }

    if (config.isSinglePrecision) {
      resetAndRun(FpuOperation.ADD, 0x3F800000, 0x3F800000, 0x40000000, "ADD 1.0 + 1.0 (single)", 2)
      // Additional tests...
    } else {
      resetAndRun(FpuOperation.ADD, 0x3FF0000000000000L, 0x3FF0000000000000L, 0x4000000000000000L, "ADD 1.0 + 1.0 (double)", 3)
      // Additional tests...
    }
  }
  runTests(FPUConfig(32))  // Single precision
  runTests(FPUConfig(64))  // Double precision
}
```

This test harness applies vectors to the FPU, waits for a valid result, and checks the output and cycle count.

---

## Conclusion

Our FPU is built on a modular architecture that uses:

- **Scala** for high‑level elaboration and parameterization.
- **SpinalHDL** for concise and maintainable RTL generation.
- **Plugins** for modular functional units (adder, multiplier, divider/sqrt, status, etc.).
- **Fibers & Retainers** to coordinate asynchronous elaboration.
- **A Shared Database** for global configuration.
- **A Dynamic Pipeline API** to manage data propagation and retiming.
- **Logic Synthesis Tools** to generate optimal decoders and control logic.

This approach leads to an extensible, decoupled design where new functionality (e.g., alternative arithmetic methods or error‑handling strategies) can be integrated without modifying the core FPU. Our design methodology promotes rapid iteration and flexibility while meeting the performance and IEEE‑754 compliance requirements.

---
