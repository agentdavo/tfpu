package fpu

import spinal.core._
import spinal.lib._

class VCU extends Component {
  val io = new Bundle {
    val a, b      = in(Fp64())
    val op        = in(FpuOp())
    val isSingle  = in Bool()
    val result    = out(Fp64())
    val flags     = out(FpuFlags())
    val bypass    = out Bool()
    val needsNorm = out Bool()
  }

  // Detect special values
  val aIsNaN = io.a.exp === U(2047) && io.a.mant =/= 0
  val bIsNaN = io.b.exp === U(2047) && io.b.mant =/= 0
  val aIsInf = io.a.exp === U(2047) && io.a.mant === 0
  val bIsInf = io.b.exp === U(2047) && io.b.mant === 0
  val aIsZero = io.a.exp === 0 && io.a.mant === 0
  val bIsZero = io.b.exp === 0 && io.b.mant === 0
  val aIsDenorm = io.a.exp === 0 && io.a.mant =/= 0
  val bIsDenorm = io.b.exp === 0 && io.b.mant =/= 0

  io.bypass := aIsNaN || bIsNaN || aIsInf || bIsInf || aIsZero || bIsZero
  io.needsNorm := aIsDenorm || bIsDenorm

  // NaN result generation
  val nanResult = Fp64()
  nanResult.sign := False
  nanResult.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
  nanResult.mant := io.op.mux(
    FpuOp.FPDIV_S  -> B"00010000000000000000000".resize(52),  // DivZeroByZeroNaN single
    FpuOp.FPDIV_D  -> B"0000000000100000000000000000000000000000000000000000".resize(52),  // DivZeroByZeroNaN double
    FpuOp.FPMUL_S  -> B"00011010000000000000000".resize(52),  // ZeroMulInfNaN single
    FpuOp.FPMUL_D  -> B"0000000000011010000000000000000000000000000000000000".resize(52),  // ZeroMulInfNaN double
    FpuOp.FPSUB_S  -> B"00011000000000000000000".resize(52),  // AddOpInfsNaN single
    FpuOp.FPSUB_D  -> B"0000000000011000000000000000000000000000000000000000".resize(52),  // AddOpInfsNaN double
    FpuOp.FPSQRT_S -> B"00010110000000000000000".resize(52),  // NegSqrtNaN single
    FpuOp.FPSQRT_D -> B"0000000000010110000000000000000000000000000000000000".resize(52),  // NegSqrtNaN double
    default -> Mux(io.isSingle, B"00010000000000000000000".resize(52), B"0000000000100000000000000000000000000000000000000000".resize(52))
  )

  // Handle special cases
  io.result.assignDontCare()
  io.flags.NV := False
  io.flags.NX := False
  io.flags.OF := False
  io.flags.UF := False
  io.flags.DZ := False

  when(aIsNaN || bIsNaN) {
    io.result := nanResult
    io.flags.NV := True
  } elsewhen(aIsInf && bIsInf) {
    when(io.op === FpuOp.FPSUB_S || io.op === FpuOp.FPSUB_D || io.op === FpuOp.FPADD_S || io.op === FpuOp.FPADD_D) {
      io.result := nanResult
      io.flags.NV := True
    } otherwise {
      io.result.sign := io.a.sign ^ io.b.sign
      io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
      io.result.mant := B(0, 52 bits)
    }
  } elsewhen(aIsInf) {
    io.result.sign := io.a.sign
    io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
    io.result.mant := B(0, 52 bits)
  } elsewhen(bIsInf) {
    io.result.sign := io.b.sign
    io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
    io.result.mant := B(0, 52 bits)
  } elsewhen(bIsZero && (io.op === FpuOp.FPDIV_S || io.op === FpuOp.FPDIV_D)) {
    io.result.sign := io.a.sign
    io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
    io.result.mant := B(0, 52 bits)
    io.flags.DZ := True
  } elsewhen(aIsZero && bIsZero && (io.op === FpuOp.FPDIV_S || io.op === FpuOp.FPDIV_D)) {
    io.result := nanResult
    io.flags.NV := True
  } elsewhen(aIsZero) {
    io.result.sign := io.a.sign
    io.result.exp := U(0, 11 bits)
    io.result.mant := B(0, 52 bits)
  }
}