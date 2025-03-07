package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber._

class FPUStackPlugin(override val config: FPUConfig, override val pipeline: Pipeline) extends FpuExecutionPlugin {
  override def build(): Unit = during setup {
    val io = new Bundle {
      val opcode = in(FpuOperation())
      val memAddress = in UInt(32 bits)
      val faIn = in(new FloatData(config))
      val fbIn = in(new FloatData(config))
      val fcIn = in(new FloatData(config))
      val statusIn = in(new FPUStatus())
      val tempA = out(new FloatData(config))
      val faOut = out(new FloatData(config))
      val fbOut = out(new FloatData(config))
      val statusOut = out(new FPUStatus())
      val active = out Bool()
      val trapInterface = out(TrapInterface())
    }
    this.io = io

    val fetchStage = pipeline.stages(0)
    io.faIn := fetchStage(FpuGlobal.FA)
    io.fbIn := fetchStage(FpuGlobal.FB)
    io.fcIn := fetchStage(FpuGlobal.FC)
    io.memAddress := fetchStage(FpuGlobal.MEM_ADDRESS)
    io.statusIn := fetchStage(FpuGlobal.STATUS)
    io.opcode := fetchStage(FpuGlobal.OPCODE)

    val stack = Vec(Reg(new FloatData(config)), 3)
    val types = Vec(Reg(UInt(2 bits)), 3)
    val mem = Mem(Bits(32 bits), 1024)

    val unalignError = io.memAddress(1 downto 0) =/= 0
    val accessViolation = False
    val memOutOfBounds = io.memAddress >= 1024 || (io.memAddress + Mux(io.statusIn.typeFPAreg === 1, U(2), U(1)) >= 1024)

    when(unalignError || accessViolation || memOutOfBounds) {
      io.trapInterface.trapEnable := True
      io.trapInterface.trapCause := Mux(unalignError, U(1), Mux(accessViolation, U(2), U(3)))
      fetchStage.throwIt()
    }

    io.active := False
    when(fetchStage.isValid && fetchStage(FpuGlobal.MICRO_PC) =/= 0 && io.microInst.op =/= MicrocodeOp.NONE) {
      io.active := True

      io.tempA := FloatData(config).fromBits(B(0, config.totalWidth bits))
      io.faOut := stack(0)
      io.fbOut := stack(1)
      io.statusOut := io.statusIn

      switch(io.microInst.op) {
        is(MicrocodeOp.LOAD) {
          when(io.opcode === FpuOperation.LDLNSN) {
            when(!unalignError && !accessViolation && !memOutOfBounds) {
              io.faOut := FloatData(config).fromBits(mem.readSync(io.memAddress).resize(32))
              stack(1) := stack(0); stack(2) := stack(1); stack(0) := io.faOut
              types(1) := types(0); types(2) := types(1); types(0) := 0
              fetchStage(FpuGlobal.TempA) := io.faOut
              io.statusOut.typeFPAreg := 0
            }
          }
          when(io.opcode === FpuOperation.LDNLSNI) {
            val offset = fetchStage(FpuGlobal.FB).asBits.asUInt
            when(!unalignError && !accessViolation && !memOutOfBounds) {
              io.faOut := FloatData(config).fromBits(mem.readSync(io.memAddress + offset).resize(32))
              stack(1) := stack(0); stack(2) := stack(1); stack(0) := io.faOut
              types(1) := types(0); types(2) := types(1); types(0) := 0
              fetchStage(FpuGlobal.TempA) := io.faOut
              io.statusOut.typeFPAreg := 0
            }
          }
          when(io.opcode === FpuOperation.LDNLDB) {
            when(!unalignError && !accessViolation && !memOutOfBounds) {
              io.faOut := FloatData(config).fromBits(Cat(mem.readSync(io.memAddress + 1), mem.readSync(io.memAddress)))
              stack(1) := stack(0); stack(2) := stack(1); stack(0) := io.faOut
              types(1) := types(0); types(2) := types(1); types(0) := 1
              fetchStage(FpuGlobal.TempA) := io.faOut
              io.statusOut.typeFPAreg := 1
            }
          }
          when(io.opcode === FpuOperation.LDNLDBI) {
            val offset = fetchStage(FpuGlobal.FB).asBits.asUInt * 2
            when(!unalignError && !accessViolation && !memOutOfBounds) {
              io.faOut := FloatData(config).fromBits(Cat(mem.readSync(io.memAddress + offset + 1), mem.readSync(io.memAddress + offset)))
              stack(1) := stack(0); stack(2) := stack(1); stack(0) := io.faOut
              types(1) := types(0); types(2) := types(1); types(0) := 1
              fetchStage(FpuGlobal.TempA) := io.faOut
              io.statusOut.typeFPAreg := 1
            }
          }
          when(io.opcode === FpuOperation.LDZEROSN) {
            io.faOut := FloatData(config).fromBits(B(0, 32 bits))
            stack(1) := stack(0); stack(2) := stack(1); stack(0) := io.faOut
            types(1) := types(0); types(2) := types(1); types(0) := 0
            fetchStage(FpuGlobal.TempA) := io.faOut
            io.statusOut.typeFPAreg := 0
          }
          when(io.opcode === FpuOperation.LDZERODB) {
            io.faOut := FloatData(config).fromBits(B(0, 64 bits))
            stack(1) := stack(0); stack(2) := stack(1); stack(0) := io.faOut
            types(1) := types(0); types(2) := types(1); types(0) := 1
            fetchStage(FpuGlobal.TempA) := io.faOut
            io.statusOut.typeFPAreg := 1
          }
          when(io.opcode === FpuOperation.LDALL) {
            when(!unalignError && !accessViolation && !memOutOfBounds) {
              val tempStatus = Reg(new FPUStatus())
              val tempFPA = Reg(new FloatData(config))
              val tempFPB = Reg(new FloatData(config))
              val tempFPC = Reg(new FloatData(config))
              val tempTypes = Reg(Vec(UInt(2 bits), 3))
              switch(io.microInst.nextPc) {
                is(1) { tempStatus := FPUStatus().fromBits(mem.readSync(io.memAddress)) }
                is(2) { tempFPA.assignFromBits(Cat(B(0, 32 bits), mem.readSync(io.memAddress + 1))) }
                is(3) { when(tempStatus.typeFPAreg === 1) { tempFPA.assignFromBits(Cat(mem.readSync(io.memAddress + 2), tempFPA.asBits(31 downto 0))) } }
                is(4) { tempFPB.assignFromBits(Cat(B(0, 32 bits), mem.readSync(io.memAddress + Mux(tempStatus.typeFPAreg === 1, U(3), U(2))))) }
                is(5) { when(tempStatus.typeFPBreg === 1) { tempFPB.assignFromBits(Cat(mem.readSync(io.memAddress + Mux(tempStatus.typeFPAreg === 1, U(4), U(3)) + 1), tempFPB.asBits(31 downto 0))) } }
                is(6) { tempFPC.assignFromBits(Cat(B(0, 32 bits), mem.readSync(io.memAddress + Mux(tempStatus.typeFPAreg === 1 && tempStatus.typeFPBreg === 1, U(5), Mux(tempStatus.typeFPAreg === 0 && tempStatus.typeFPBreg === 0, U(3), U(4)))))) }
                is(0) { when(tempStatus.typeFPCreg === 1) { tempFPC.assignFromBits(Cat(mem.readSync(io.memAddress + Mux(tempStatus.typeFPAreg === 1 && tempStatus.typeFPBreg === 1, U(6), Mux(tempStatus.typeFPAreg === 0 && tempStatus.typeFPBreg === 0, U(4), U(5))) + 1), tempFPC.asBits(31 downto 0))) }
                        io.statusOut := tempStatus
                        io.faOut := tempFPA
                        io.fbOut := tempFPB
                        stack(0) := tempFPA
                        stack(1) := tempFPB
                        stack(2) := tempFPC
                        tempTypes(0) := tempStatus.typeFPAreg
                        tempTypes(1) := tempStatus.typeFPBreg
                        tempTypes(2) := tempStatus.typeFPCreg
                        types := tempTypes
                        fetchStage(FpuGlobal.TempA) := tempFPA
                        fetchStage(FpuGlobal.TempB) := tempFPB }
              }
            }
          }
          when(io.opcode === FpuOperation.LDNLADDSN) {
            when(!unalignError && !accessViolation && !memOutOfBounds) {
              io.tempA := FloatData(config).fromBits(mem.readSync(io.memAddress).resize(32))
              fetchStage(FpuGlobal.TempA) := io.tempA
              io.statusOut.typeFPAreg := 0
            }
          }
          when(io.opcode === FpuOperation.LDNLADDDB) {
            when(!unalignError && !accessViolation && !memOutOfBounds) {
              io.tempA := FloatData(config).fromBits(Cat(mem.readSync(io.memAddress + 1), mem.readSync(io.memAddress)))
              fetchStage(FpuGlobal.TempA) := io.tempA
              io.statusOut.typeFPAreg := 1
            }
          }
        }
        is(MicrocodeOp.STORE) {
          when(io.opcode === FpuOperation.STALL) {
            switch(io.microInst.nextPc) {
              is(1) { mem.write(io.memAddress, Cat(io.statusIn.reserved, io.statusIn.typeFPCreg, io.statusIn.typeFPBreg, io.statusIn.typeFPAreg, io.statusIn.roundingMode)) }
              is(2) { mem.write(io.memAddress + 1, io.faIn.asBits(31 downto 0)) }
              is(3) { when(io.statusIn.typeFPAreg === 1) { mem.write(io.memAddress + 2, io.faIn.asBits(63 downto 32)) } }
              is(4) { mem.write(io.memAddress + Mux(io.statusIn.typeFPAreg === 1, U(3), U(2)), io.fbIn.asBits(31 downto 0)) }
              is(5) { when(io.statusIn.typeFPBreg === 1) { mem.write(io.memAddress + Mux(io.statusIn.typeFPAreg === 1, U(4), U(3)), io.fbIn.asBits(63 downto 32)) } }
              is(6) { mem.write(io.memAddress + Mux(io.statusIn.typeFPAreg === 1 && io.statusIn.typeFPBreg === 1, U(5), Mux(io.statusIn.typeFPAreg === 0 && io.statusIn.typeFPBreg === 0, U(3), U(4))), io.fcIn.asBits(31 downto 0)) }
              is(0) { when(io.statusIn.typeFPCreg === 1) { mem.write(io.memAddress + Mux(io.statusIn.typeFPAreg === 1 && io.statusIn.typeFPBreg === 1, U(6), Mux(io.statusIn.typeFPAreg === 0 && io.statusIn.typeFPBreg === 0, U(4), U(5))), io.fcIn.asBits(63 downto 32)) } }
            }
            fetchStage(FpuGlobal.TempB) := io.fcIn
          }
          when(io.opcode === FpuOperation.STNLSN) {
            when(!unalignError && !accessViolation && !memOutOfBounds) {
              mem.write(io.memAddress, io.faIn.asBits(31 downto 0))
              stack(0) := stack(1); stack(1) := stack(2)
              types(0) := types(1); types(1) := types(2)
              fetchStage(FpuGlobal.TempA) := stack(0)
            }
          }
          when(io.opcode === FpuOperation.STNLDB) {
            when(!unalignError && !accessViolation && !memOutOfBounds) {
              mem.write(io.memAddress, io.faIn.asBits(31 downto 0))
              mem.write(io.memAddress + 1, io.faIn.asBits(63 downto 32))
              stack(0) := stack(1); stack(1) := stack(2)
              types(0) := types(1); types(1) := types(2)
              fetchStage(FpuGlobal.TempA) := stack(0)
            }
          }
          when(io.opcode === FpuOperation.STNLI32) {
            when(!unalignError && !accessViolation && !memOutOfBounds) {
              mem.write(io.memAddress, io.faIn.asBits(31 downto 0))
              stack(0) := stack(1); stack(1) := stack(2)
              types(0) := types(1); types(1) := types(2)
              fetchStage(FpuGlobal.TempA) := stack(0)
            }
          }
        }
        is(MicrocodeOp.DUP) {
          stack(1) := stack(0); stack(2) := stack(1); stack(0) := io.faIn
          types(1) := types(0); types(2) := types(1); types(0) := io.statusIn.typeFPAreg
          io.faOut := io.faIn
          fetchStage(FpuGlobal.TempA) := io.faIn
        }
        is(MicrocodeOp.REV) {
          io.faOut := io.fbIn
          io.fbOut := io.faIn
          stack(0) := io.fbIn; stack(1) := io.faIn
          io.statusOut.typeFPAreg := io.statusIn.typeFPBreg
          io.statusOut.typeFPBreg := io.statusIn.typeFPAreg
          fetchStage(FpuGlobal.TempA) := io.faOut
          fetchStage(FpuGlobal.TempB) := io.fbOut
        }
      }

      io.statusOut.unalign := unalignError
      io.statusOut.accessViolation := accessViolation

      fetchStage(FpuGlobal.TempA) := io.tempA
      fetchStage(FpuGlobal.FA) := io.faOut
      fetchStage(FpuGlobal.FB) := io.fbOut
      fetchStage(FpuGlobal.STATUS) := io.statusOut
      when(io.microInst.nextPc =/= 0) { fetchStage.haltIt() }
    }

    def connectPayload(stage: Node): Unit = {
      stage(FpuGlobal.MICRO_PC) := io.microInst.nextPc
    }

    registerOperations(Map(
      "LDLNSN" -> FpuOperation.LDLNSN, "LDNLDB" -> FpuOperation.LDNLDB,
      "LDNLSNI" -> FpuOperation.LDNLSNI, "LDNLDBI" -> FpuOperation.LDNLDBI,
      "LDZEROSN" -> FpuOperation.LDZEROSN, "LDZERODB" -> FpuOperation.LDZERODB,
      "LDNLADDSN" -> FpuOperation.LDNLADDSN, "LDNLADDDB" -> FpuOperation.LDNLADDDB,
      "LDNLMULSN" -> FpuOperation.LDNLMULSN, "LDNLMULDB" -> FpuOperation.LDNLMULDB,
      "LDALL" -> FpuOperation.LDALL,
      "STNLSN" -> FpuOperation.STNLSN, "STNLDB" -> FpuOperation.STNLDB,
      "STNLI32" -> FpuOperation.STNLI32, "STALL" -> FpuOperation.STALL,
      "DUP" -> FpuOperation.DUP, "REV" -> FpuOperation.REV
    ))
    registerMicrocode(FpuOperation.LDLNSN, Seq(FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 1, 0)))
    registerMicrocode(FpuOperation.LDNLDB, Seq(FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 1, 0)))
    registerMicrocode(FpuOperation.LDNLSNI, Seq(FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 1, 0)))
    registerMicrocode(FpuOperation.LDNLDBI, Seq(FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 1, 0)))
    registerMicrocode(FpuOperation.LDZEROSN, Seq(FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 1, 0)))
    registerMicrocode(FpuOperation.LDZERODB, Seq(FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 1, 0)))
    registerMicrocode(FpuOperation.LDNLADDSN, Seq(FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 1, 0)))
    registerMicrocode(FpuOperation.LDNLADDDB, Seq(FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 1, 0)))
    registerMicrocode(FpuOperation.LDNLMULSN, Seq(
	  FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 1, 1),
	  FpuDatabase.instr(MicrocodeOp.MUL, 1, 0, 1, 0)
	))
    registerMicrocode(FpuOperation.LDNLMULDB, Seq(
	  FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 1, 1),
	  FpuDatabase.instr(MicrocodeOp.MUL, 1, 0, 1, 0)
	))
    registerMicrocode(FpuOperation.LDALL, Seq(
      FpuDatabase.instr(MicrocodeOp.LOAD, 0, 0, 0, 1),
      FpuDatabase.instr(MicrocodeOp.LOAD, 1, 0, 0, 2),
      FpuDatabase.instr(MicrocodeOp.LOAD, 1, 0, 0, 3),
      FpuDatabase.instr(MicrocodeOp.LOAD, 2, 0, 0, 4),
      FpuDatabase.instr(MicrocodeOp.LOAD, 2, 0, 0, 5),
      FpuDatabase.instr(MicrocodeOp.LOAD, 3, 0, 0, 6),
      FpuDatabase.instr(MicrocodeOp.LOAD, 3, 0, 0, 0)
    ))
    registerMicrocode(FpuOperation.STNLSN, Seq(FpuDatabase.instr(MicrocodeOp.STORE, 1, 0, 0, 0)))
    registerMicrocode(FpuOperation.STNLDB, Seq(FpuDatabase.instr(MicrocodeOp.STORE, 1, 0, 0, 0)))
    registerMicrocode(FpuOperation.STNLI32, Seq(FpuDatabase.instr(MicrocodeOp.STORE, 4, 0, 0, 0)))
    registerMicrocode(FpuOperation.STALL, Seq(
      FpuDatabase.instr(MicrocodeOp.STORE, 0, 0, 0, 1),
      FpuDatabase.instr(MicrocodeOp.STORE, 1, 0, 0, 2),
      FpuDatabase.instr(MicrocodeOp.STORE, 1, 0, 0, 3),
      FpuDatabase.instr(MicrocodeOp.STORE, 2, 0, 0, 4),
      FpuDatabase.instr(MicrocodeOp.STORE, 2, 0, 0, 5),
      FpuDatabase.instr(MicrocodeOp.STORE, 3, 0, 0, 6),
      FpuDatabase.instr(MicrocodeOp.STORE, 3, 0, 0, 0)
    ))
    registerMicrocode(FpuOperation.DUP, Seq(FpuDatabase.instr(MicrocodeOp.DUP, 1, 0, 0, 0)))
    registerMicrocode(FpuOperation.REV, Seq(FpuDatabase.instr(MicrocodeOp.REV, 1, 2, 0, 0)))
    awaitBuild()
  }
}