// SPDX-FileCopyrightText: 2025 David Smith <david.smith@linux.com>
// SPDX-License-Identifier: MIT

package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

// Top-level FPU component implementing the T9000 Floating-Point Unit
class FPU extends Component {
  // Input/Output interface for command stream, memory access, and result output
  val io = new Bundle {
    val cmd       = slave Stream(FpuOp())      // Command input stream for FPU operations
    val memAddr   = out UInt(32 bits)          // Memory address for load/store operations
    val memDataIn = in Bits(64 bits)           // Data input from memory
    val memDataOut= out Bits(64 bits)          // Data output to memory
    val memWrite  = out Bool()                 // Write enable signal for memory
    val result    = master Flow(Bits(64 bits)) // Result output flow (64-bit result)
    val flags     = out(FpuFlags())            // Exception flags output
    val trap      = out Bool()                 // Trap signal for exceptions
  }

  // Three-stage pipeline: Fetch, Decode, Execute
  val fetch   = CtrlLink()    // Fetch stage for command reception
  val decode  = CtrlLink()    // Decode stage for microcode lookup
  val execute = CtrlLink()    // Execute stage for operation execution
  val f2d     = StageLink(fetch.down, decode.up)   // Link between Fetch and Decode
  val d2e     = StageLink(decode.down, execute.up) // Link between Decode and Execute

  // Pipeline payloads for passing data between stages
  val OPCODE    = Payload(FpuOp())      // Operation code payload
  val MICROCODE = Payload(Microcode())  // Microcode entry payload
  val STEP      = Payload(UInt(4 bits)) // Step counter for multi-cycle ops

  // FPU state registers
  val stack  = Vec.fill(3)(Reg(Fp64()) init Fp64().assignFromBits(0)) // Three-element evaluation stack (FA, FB, FC)
  val status = Reg(FpStatus()) init FpStatus(B"01", B"00", B"00", B"00") // Status word (RNE, all single initially)

  // Microcode ROM instance
  val microcode = MicrocodeRom()

  // Fetch Stage: Receives commands from the input stream
  val fetcher = new fetch.Area {
    io.cmd.ready := False
    when(io.cmd.valid) {
      OPCODE := io.cmd.payload
      STEP := 0
      io.cmd.ready := True
    }
  }

  // Decode Stage: Looks up microcode from ROM
  val decoder = new decode.Area {
    MICROCODE := microcode(OPCODE.asUInt) // Fetch microcode entry using asUInt
    io.memAddr := 0
    io.memWrite := False
  }

  // Execute Stage: Performs operations based on microcode
  val executor = new execute.Area {
    val micro = MICROCODE
    val step = STEP
    val resultReg = Reg(Fp64()) init Fp64().assignFromBits(0)
    val flagsReg = Reg(FpuFlags()) init FpuFlags().assignFromBits(0)

    // Hardware unit instances
    val adder   = new DualAdder
    val mul     = new Multiplier
    val divRoot = new DividerRooter
    val vcu     = new VCU

    // Precision detection based on stack operand types
    val isSingle = status.fpaType === B"00" && status.fpbType === B"00"
    val effectiveOp = micro.op.mux(
      FpuOp.FPADD_S      -> Mux(isSingle, FpuOp.FPADD_S, FpuOp.FPADD_D),
      FpuOp.FPADD_D      -> Mux(isSingle, FpuOp.FPADD_S, FpuOp.FPADD_D),
      FpuOp.FPSUB_S      -> Mux(isSingle, FpuOp.FPSUB_S, FpuOp.FPSUB_D),
      FpuOp.FPSUB_D      -> Mux(isSingle, FpuOp.FPSUB_S, FpuOp.FPSUB_D),
      FpuOp.FPMUL_S      -> Mux(isSingle, FpuOp.FPMUL_S, FpuOp.FPMUL_D),
      FpuOp.FPMUL_D      -> Mux(isSingle, FpuOp.FPMUL_S, FpuOp.FPMUL_D),
      FpuOp.FPDIV_S      -> Mux(isSingle, FpuOp.FPDIV_S, FpuOp.FPDIV_D),
      FpuOp.FPDIV_D      -> Mux(isSingle, FpuOp.FPDIV_S, FpuOp.FPDIV_D),
      FpuOp.FPABS_S      -> Mux(isSingle, FpuOp.FPABS_S, FpuOp.FPABS_D),
      FpuOp.FPABS_D      -> Mux(isSingle, FpuOp.FPABS_S, FpuOp.FPABS_D),
      FpuOp.FPMULBY2_S   -> Mux(isSingle, FpuOp.FPMULBY2_S, FpuOp.FPMULBY2_D),
      FpuOp.FPMULBY2_D   -> Mux(isSingle, FpuOp.FPMULBY2_S, FpuOp.FPMULBY2_D),
      FpuOp.FPDIVBY2_S   -> Mux(isSingle, FpuOp.FPDIVBY2_S, FpuOp.FPDIVBY2_D),
      FpuOp.FPDIVBY2_D   -> Mux(isSingle, FpuOp.FPDIVBY2_S, FpuOp.FPDIVBY2_D),
      FpuOp.FPSQRT_S     -> Mux(isSingle, FpuOp.FPSQRT_S, FpuOp.FPSQRT_D),
      FpuOp.FPSQRT_D     -> Mux(isSingle, FpuOp.FPSQRT_S, FpuOp.FPSQRT_D),
      FpuOp.FPREM_S      -> Mux(isSingle, FpuOp.FPREM_S, FpuOp.FPREM_D),
      FpuOp.FPREM_D      -> Mux(isSingle, FpuOp.FPREM_S, FpuOp.FPREM_D),
      FpuOp.FPUSQRTLAST_S -> Mux(isSingle, FpuOp.FPUSQRTLAST_S, FpuOp.FPUSQRTLAST_D),
      FpuOp.FPUSQRTLAST_D -> Mux(isSingle, FpuOp.FPUSQRTLAST_S, FpuOp.FPUSQRTLAST_D),
      FpuOp.FPRANGE_S    -> Mux(isSingle, FpuOp.FPRANGE_S, FpuOp.FPRANGE_D),
      FpuOp.FPRANGE_D    -> Mux(isSingle, FpuOp.FPRANGE_S, FpuOp.FPRANGE_D),
      FpuOp.FPGT_S       -> Mux(isSingle, FpuOp.FPGT_S, FpuOp.FPGT_D),
      FpuOp.FPGT_D       -> Mux(isSingle, FpuOp.FPGT_S, FpuOp.FPGT_D),
      FpuOp.FPEQ_S       -> Mux(isSingle, FpuOp.FPEQ_S, FpuOp.FPEQ_D),
      FpuOp.FPEQ_D       -> Mux(isSingle, FpuOp.FPEQ_S, FpuOp.FPEQ_D),
      FpuOp.FPORDERED_S  -> Mux(isSingle, FpuOp.FPORDERED_S, FpuOp.FPORDERED_D),
      FpuOp.FPORDERED_D  -> Mux(isSingle, FpuOp.FPORDERED_S, FpuOp.FPORDERED_D),
      FpuOp.FPGE_S       -> Mux(isSingle, FpuOp.FPGE_S, FpuOp.FPGE_D),
      FpuOp.FPGE_D       -> Mux(isSingle, FpuOp.FPGE_S, FpuOp.FPGE_D),
      FpuOp.FPLG_S       -> Mux(isSingle, FpuOp.FPLG_S, FpuOp.FPLG_D),
      FpuOp.FPLG_D       -> Mux(isSingle, FpuOp.FPLG_S, FpuOp.FPLG_D),
      default            -> micro.op
    )
    val effectiveMicro = microcode(effectiveOp.asUInt)
    val effectiveStepCount = effectiveMicro.stepCount

    // Connect hardware units
    adder.io.a := stack(0)
    adder.io.b := stack(1)
    adder.io.isSub := effectiveMicro.op === FpuOp.FPSUB_S || effectiveMicro.op === FpuOp.FPSUB_D
    adder.io.roundingMode := status.toRoundingMode
    adder.io.isSingle := isSingle
    mul.io.a := stack(0)
    mul.io.b := stack(1)
    mul.io.roundingMode := status.toRoundingMode
    mul.io.isSingle := isSingle
    divRoot.io.a := stack(0)
    divRoot.io.b := stack(1)
    divRoot.io.isDiv := effectiveMicro.op === FpuOp.FPDIV_S || effectiveMicro.op === FpuOp.FPDIV_D
    divRoot.io.roundingMode := status.toRoundingMode
    divRoot.io.isSingle := isSingle
    vcu.io.a := stack(0)
    vcu.io.b := stack(1)
    vcu.io.op := effectiveMicro.op
    vcu.io.isSingle := isSingle

    // Trap logic
    io.trap := effectiveMicro.trapEnable && (flagsReg.NV || flagsReg.DZ || flagsReg.OF || flagsReg.UF || flagsReg.NX)
    when(io.trap) {
      resultReg := stack(0)
    }

    // Execute multi-cycle operations
    when(isValid && step < effectiveStepCount) {
      when(vcu.io.needsNorm && step === 0) {
        when(stack(0).isDenorm) {
          val shift = stack(0).mant.asUInt.leadingZerosCount.resize(6)
          stack(0).exp := stack(0).exp - shift
          stack(0).mant := (stack(0).mant << shift)(51 downto 0)
        }
        when(stack(1).isDenorm) {
          val shift = stack(1).mant.asUInt.leadingZerosCount.resize(6)
          stack(1).exp := stack(1).exp - shift
          stack(1).mant := (stack(1).mant << shift)(51 downto 0)
        }
      } else when(vcu.io.bypass) {
        resultReg := vcu.io.result
        flagsReg := vcu.io.flags
      } else {
        switch(effectiveMicro.op) {
          is(FpuOp.FPLDNLSN) {
            resultReg := Fp32().assignFromBits(io.memDataIn(31 downto 0)).toFp64
            status.fpaType := B"00"
          }
          is(FpuOp.FPLDNLDB) {
            resultReg := Fp64().assignFromBits(io.memDataIn)
            status.fpaType := B"01"
          }
          is(FpuOp.FPLDNLSNI) {
            resultReg := Fp32().assignFromBits(io.memDataIn(31 downto 0)).toFp64
            status.fpaType := B"00"
          }
          is(FpuOp.FPLDNLDBI) {
            resultReg := Fp64().assignFromBits(io.memDataIn)
            status.fpaType := B"01"
          }
          is(FpuOp.FPLDZEROSN) {
            resultReg := Fp64().assignFromBits(0)
            status.fpaType := B"00"
          }
          is(FpuOp.FPLDZERODB) {
            resultReg := Fp64().assignFromBits(0)
            status.fpaType := B"01"
          }
          is(FpuOp.FPLDNLADDSN) {
            when(step === 0) {
              resultReg := Fp32().assignFromBits(io.memDataIn(31 downto 0)).toFp64
            } else {
              adder.io.a := resultReg
              resultReg := adder.io.result
              flagsReg := adder.io.flags
            }
            status.fpaType := B"00"
          }
          is(FpuOp.FPLDNLADDDB) {
            when(step === 0) {
              resultReg := Fp64().assignFromBits(io.memDataIn)
            } else {
              adder.io.a := resultReg
              resultReg := adder.io.result
              flagsReg := adder.io.flags
            }
            status.fpaType := B"01"
          }
          is(FpuOp.FPLDNLMULSN) {
            when(step === 0) {
              resultReg := Fp32().assignFromBits(io.memDataIn(31 downto 0)).toFp64
            } else {
              mul.io.a := resultReg
              resultReg := mul.io.result
              flagsReg := mul.io.flags
            }
            status.fpaType := B"00"
          }
          is(FpuOp.FPLDNLMULDB) {
            when(step === 0) {
              resultReg := Fp64().assignFromBits(io.memDataIn)
            } else {
              mul.io.a := resultReg
              resultReg := mul.io.result
              flagsReg := mul.io.flags
            }
            status.fpaType := B"01"
          }
          is(FpuOp.FPSTNLSN) {
            io.memDataOut := stack(0).toFp32.asBits.resize(64)
            io.memWrite := True
          }
          is(FpuOp.FPSTNLDB) {
            io.memDataOut := stack(0).asBits
            io.memWrite := True
          }
          is(FpuOp.FPSTNLI32) {
            io.memDataOut := stack(0).mant(31 downto 0).asSInt.asBits.resize(64)
            io.memWrite := True
          }
          is(FpuOp.FPENTRY) {
            resultReg := Fp64().assignFromBits(io.memDataIn)
            status.fpaType := B"01"
          }
          is(FpuOp.FPREV) {
            val temp = stack(0)
            stack(0) := stack(1)
            stack(1) := temp
            val tempType = status.fpaType
            status.fpaType := status.fpbType
            status.fpbType := tempType
          }
          is(FpuOp.FPDUP) {
            resultReg := stack(0)
          }
          is(FpuOp.FPRN) {
            status.roundingMode := B"01"
          }
          is(FpuOp.FPRZ) {
            status.roundingMode := B"00"
          }
          is(FpuOp.FPRP) {
            status.roundingMode := B"10"
          }
          is(FpuOp.FPRM) {
            status.roundingMode := B"11"
          }
          is(FpuOp.FPCHKERR) {
            resultReg := Fp64().assignFromBits(Cat(flagsReg.asBits.resize(52), U"0".resize(11), flagsReg.NV).asBits)
          }
          is(FpuOp.FPTESTERR) {
            flagsReg.NV := False
            flagsReg.NX := False
            flagsReg.OF := False
            flagsReg.UF := False
            flagsReg.DZ := False
          }
          is(FpuOp.FPSETERR) {
            flagsReg.NV := True
          }
          is(FpuOp.FPCLRERR) {
            flagsReg.NV := False
            flagsReg.NX := False
            flagsReg.OF := False
            flagsReg.UF := False
            flagsReg.DZ := False
          }
          is(FpuOp.FPGT_S, FpuOp.FPGT_D) {
            adder.io.isSub := True
            val diff = adder.io.result
            resultReg := Fp64().assignFromBits(Cat(diff.sign, U"0".resize(11), B"0".resize(52)))
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPEQ_S, FpuOp.FPEQ_D) {
            adder.io.isSub := True
            val diff = adder.io.result
            resultReg := Fp64().assignFromBits(Cat(~diff.isZero, U"0".resize(11), B"0".resize(52)))
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPORDERED_S, FpuOp.FPORDERED_D) {
            resultReg := Fp64().assignFromBits(Cat(~(stack(0).isNaN || stack(1).isNaN), U"0".resize(11), B"0".resize(52)))
          }
          is(FpuOp.FPNAN) {
            resultReg := Fp64().assignFromBits(Cat(stack(0).isNaN, U"0".resize(11), B"0".resize(52)))
          }
          is(FpuOp.FPNOTFINITE) {
            resultReg := Fp64().assignFromBits(Cat(stack(0).isInf || stack(0).isNaN, U"0".resize(11), B"0".resize(52)))
          }
          is(FpuOp.FPCHKI32) {
            when(step === 0) {
              resultReg := stack(0)
            } else {
              val intVal = resultReg.mant(31 downto 0).asSInt
              flagsReg.OF := resultReg.exp > 31
              resultReg := Fp64().assignFromBits(Cat(intVal < 0, U"0".resize(11), intVal.abs.asBits.resize(52)))
            }
          }
          is(FpuOp.FPCHKI64) {
            when(step === 0) {
              resultReg := stack(0)
            } else {
              val intVal = resultReg.mant.asSInt
              flagsReg.OF := resultReg.exp > 63
              resultReg := Fp64().assignFromBits(Cat(intVal < 0, U"0".resize(11), intVal.abs.asBits.resize(52)))
            }
          }
          is(FpuOp.FPR321OR64) {
            when(step === 0) {
              resultReg := stack(0)
            } else {
              resultReg := resultReg.toFp32.toFp64
              flagsReg.NX := stack(0).mant(28 downto 0) =/= 0
            }
            status.fpaType := B"01"
          }
          is(FpuOp.FPR64TOR32) {
            when(step === 0) {
              resultReg := stack(0)
            } else {
              resultReg := resultReg.toFp32.toFp64
              flagsReg.NX := stack(0).mant(28 downto 0) =/= 0
            }
            status.fpaType := B"00"
          }
          is(FpuOp.FPRTOI32) {
            when(step === 0) {
              resultReg := stack(0)
            } else {
              val intVal = resultReg.mant(31 downto 0).asSInt
              flagsReg.OF := resultReg.exp > 31
              resultReg := Fp64().assignFromBits(Cat(intVal < 0, U"0".resize(11), intVal.abs.asBits.resize(52)))
            }
          }
          is(FpuOp.FPI321OR32) {
            when(step === 0) {
              resultReg := Fp64().assignFromBits(Cat(stack(0).sign, U(127 + 31), stack(0).mant))
            } else {
              resultReg := resultReg.toFp32.toFp64
              flagsReg.NX := stack(0).mant(28 downto 0) =/= 0
            }
            status.fpaType := B"00"
          }
          is(FpuOp.FPI321OR64) {
            resultReg := Fp64().assignFromBits(Cat(stack(0).sign, U(1023 + 31), stack(0).mant))
            status.fpaType := B"01"
          }
          is(FpuOp.FPB321OR64) {
            resultReg := Fp64().assignFromBits(stack(0).asBits)
            status.fpaType := B"01"
          }
          is(FpuOp.FPNOROUND) {
            when(step === 0) {
              resultReg := stack(0)
            } else {
              resultReg := Fp64().assignFromBits(Cat(stack(0).sign, stack(0).exp(7 downto 0).resize(11), stack(0).mant(51 downto 29) << 29))
            }
            status.fpaType := B"00"
          }
          is(FpuOp.FPINT) {
            when(step === 0) {
              resultReg := stack(0)
            } else {
              val shift = 52 - resultReg.exp.asSInt
              resultReg.mant := Mux(shift < 0, resultReg.mant << shift.abs, resultReg.mant >> shift)
              resultReg.exp := U(0, 11 bits)
            }
          }
          is(FpuOp.FPADD_S, FpuOp.FPADD_D) {
            resultReg := adder.io.result
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPSUB_S, FpuOp.FPSUB_D) {
            resultReg := adder.io.result
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPMUL_S, FpuOp.FPMUL_D) {
            resultReg := mul.io.result
            flagsReg := mul.io.flags
          }
          is(FpuOp.FPDIV_S, FpuOp.FPDIV_D) {
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPABS_S, FpuOp.FPABS_D) {
            resultReg := stack(0)
            resultReg.sign := False
          }
          is(FpuOp.FPEXPINC32) {
            resultReg := stack(0)
            resultReg.exp := stack(0).exp + 32
            flagsReg.OF := resultReg.exp > 254
          }
          is(FpuOp.FPEXPDEC32) {
            resultReg := stack(0)
            resultReg.exp := stack(0).exp - 32
            flagsReg.UF := resultReg.exp < 1
          }
          is(FpuOp.FPMULBY2_S, FpuOp.FPMULBY2_D) {
            resultReg := stack(0)
            resultReg.exp := stack(0).exp + 1
            flagsReg.OF := resultReg.exp > (if (isSingle) 254 else 2046)
          }
          is(FpuOp.FPDIVBY2_S, FpuOp.FPDIVBY2_D) {
            resultReg := stack(0)
            resultReg.exp := stack(0).exp - 1
            flagsReg.UF := resultReg.exp < 1
          }
          is(FpuOp.FPUSQRTFIRST) {
            divRoot.io.isDiv := False
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPUSQRTSTEP) {
            divRoot.io.isDiv := False
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPUSQRTLAST_S, FpuOp.FPUSQRTLAST_D) {
            divRoot.io.isDiv := False
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPREMFIRST) {
            divRoot.io.isDiv := True
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPREMSTEP) {
            divRoot.io.isDiv := True
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPREM_S, FpuOp.FPREM_D) {
            divRoot.io.isDiv := True
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPSQRT_S, FpuOp.FPSQRT_D) {
            divRoot.io.isDiv := False
            resultReg := divRoot.io.result
            flagsReg := divRoot.io.flags
          }
          is(FpuOp.FPRANGE_S, FpuOp.FPRANGE_D) {
            when(step === 0) {
              resultReg := stack(0)
            } else when(step === 1) {
              resultReg := Mux(resultReg.exp > (if (isSingle) 254 else 2046),
                Fp64().assignFromBits(Cat(resultReg.sign, (if (isSingle) U(254, 11 bits) else U(2046)), B"0".resize(52))),
                resultReg)
              flagsReg.OF := resultReg.exp > (if (isSingle) 254 else 2046)
            } else when(step === 2) {
              resultReg := Mux(resultReg.exp < 1,
                Fp64().assignFromBits(Cat(resultReg.sign, U(1, 11 bits), B"0".resize(52))),
                resultReg)
              flagsReg.UF := resultReg.exp < 1
            }
          }
          is(FpuOp.FPGE_S, FpuOp.FPGE_D) {
            adder.io.isSub := True
            val diff = adder.io.result
            resultReg := Fp64().assignFromBits(Cat(~(diff.sign || diff.isZero), U"0".resize(11), B"0".resize(52)))
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPLG_S, FpuOp.FPLG_D) {
            adder.io.isSub := True
            val diff = adder.io.result
            resultReg := Fp64().assignFromBits(Cat(diff.sign && !diff.isZero, U"0".resize(11), B"0".resize(52)))
            flagsReg := adder.io.flags
          }
          is(FpuOp.FPSTALL) {
            io.memDataOut := stack(0).asBits
            io.memWrite := True
          }
          is(FpuOp.FPLDALL) {
            resultReg := Fp64().assignFromBits(io.memDataIn)
            status := FpStatus.assignFromBits(io.memDataIn(31 downto 0))
          }
        }
      }

      when(effectiveMicro.pushStack) {
        stack(2) := stack(1)
        stack(1) := stack(0)
        stack(0) := resultReg
        status.fpcType := status.fpbType
        status.fpbType := status.fpaType
        status.fpaType := Mux(isSingle, B"00", B"01")
      }
      when(effectiveMicro.popStack) {
        stack(0) := stack(1)
        stack(1) := stack(2)
        stack(2).assignFromBits(0)
        status.fpaType := status.fpbType
        status.fpbType := status.fpcType
        status.fpcType := B"00"
      }
      STEP := step + 1
    }

    io.result.valid := isValid && step === effectiveStepCount
    io.result.payload := resultReg.asBits
    io.flags := flagsReg
    status.roundingMode := B"01"
  }

  Builder(fetch, decode, execute, f2d, d2e)
}