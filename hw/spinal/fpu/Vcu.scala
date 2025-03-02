package fpu

import spinal.core._
import spinal.lib._

// Vacuum Cleaner Unit (VCU) for handling special values (NaN, Inf, Zero, Denormals)
// Implements specific NaN generation per ISM 11.14, Table 11.29
class VCU extends Component {
  // Input/Output interface
  val io = new Bundle {
    val a, b      = in(Fp64())       // Operands A and B (64-bit floating-point)
    val op        = in(FpuOp())      // Operation code from FPU
    val isSingle  = in Bool()        // Precision indicator: True for single, False for double
    val result    = out(Fp64())      // Result output (64-bit floating-point)
    val flags     = out(FpuFlags())  // Exception flags output
    val bypass    = out Bool()       // Bypass signal: True if VCU handles the operation
    val needsNorm = out Bool()       // Normalization signal: True if denormals need adjustment
  }

  // Detect special values for bypass: NaN, Infinity, Zero
  io.bypass := io.a.isNaN || io.b.isNaN || io.a.isInf || io.b.isInf || io.a.isZero || io.b.isZero
  // Detect denormals requiring normalization
  io.needsNorm := io.a.isDenorm || io.b.isDenorm
  // Default outputs: unassigned result and cleared flags
  io.result.assignDontCare()
  io.flags.NV := False // Invalid Operation flag
  io.flags.NX := False // Inexact flag
  io.flags.OF := False // Overflow flag
  io.flags.UF := False // Underflow flag
  io.flags.DZ := False // Divide by Zero flag

  // Define NaN result with specific mantissa values per ISM 11.14, Table 11.29
  val nanResult = Fp64()
  nanResult.sign := False // NaNs are positive by convention
  nanResult.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits)) // Single: 255, Double: 2047
  nanResult.mant := io.op.mux(
  
    // DivZeroByZeroNaN: #7FC00000 (single) / #7FF80000 (double)
    FpuOp.FPDIV_S -> B"00010000000000000000000".resize(52), // Single-precision mantissa (16 << 19)
    FpuOp.FPDIV_D -> B"0000000000100000000000000000000000000000000000000000".resize(52), // Double-precision mantissa (32 << 48)
	
    // ZeroMulInfNaN: #7FD00000 (single) / #7FFA0000 (double)
    FpuOp.FPMUL_S -> B"00011010000000000000000".resize(52), // Single-precision mantissa (13 << 19)
    FpuOp.FPMUL_D -> B"0000000000011010000000000000000000000000000000000000".resize(52), // Double-precision mantissa (26 << 48)
	
    // AddOpInfsNaN: #7FC80000 (single) / #7FF90000 (double)
    FpuOp.FPSUB_S -> B"00011000000000000000000".resize(52), // Single-precision mantissa (12 << 19)
    FpuOp.FPSUB_D -> B"0000000000011000000000000000000000000000000000000000".resize(52), // Double-precision mantissa (24 << 48),
	
    // NegSqrtNaN: #7FC40000 (single) / #7FF88000 (double)
    FpuOp.FPSQRT_S -> B"00010110000000000000000".resize(52), // Single-precision mantissa (11 << 19)
    FpuOp.FPSQRT_D -> B"0000000000010110000000000000000000000000000000000000".resize(52), // Double-precision mantissa (22 << 48)
	
    default -> Mux(io.isSingle,
      B"00010000000000000000000".resize(52),           // Default single NaN (16 << 19)
      B"0000000000100000000000000000000000000000000000000000".resize(52)) // Default double NaN (32 << 48)
  )

  // Handle special cases hierarchically per ISM 11.14
  when(io.a.isNaN || io.b.isNaN) {
    // Any NaN input produces a quiet NaN result with NV flag
    io.result := nanResult
    io.flags.NV := True
  } elsewhen(io.a.isInf && io.b.isInf) {
    // Both inputs infinite: check for invalid operations (Inf - Inf or Inf + Inf)
    when(io.op === FpuOp.FPSUB_S || io.op === FpuOp.FPSUB_D || 
         io.op === FpuOp.FPADD_S || io.op === FpuOp.FPADD_D) {
      io.result := nanResult // Produces AddOpInfsNaN for subtraction/addition
      io.flags.NV := True    // Invalid operation flag set
    } otherwise {
      // Otherwise, propagate infinity with sign determined by operation
      io.result.sign := io.a.sign ^ io.b.sign
      io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
      io.result.mant := B(0, 52 bits)
    }
  } elsewhen(io.a.isInf) {
    // A is infinite, B is finite: propagate A's infinity
    io.result.sign := io.a.sign
    io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
    io.result.mant := B(0, 52 bits)
  } elsewhen(io.b.isInf) {
    // B is infinite, A is finite: propagate B's infinity
    io.result.sign := io.b.sign
    io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
    io.result.mant := B(0, 52 bits)
  } elsewhen(io.b.isZero && (io.op === FpuOp.FPDIV_S || io.op === FpuOp.FPDIV_D)) {
    // Division by zero: produce infinity with A's sign, set DZ flag
    io.result.sign := io.a.sign
    io.result.exp := Mux(io.isSingle, U(255, 11 bits), U(2047, 11 bits))
    io.result.mant := B(0, 52 bits)
    io.flags.DZ := True
  } elsewhen(io.a.isZero && io.b.isZero && (io.op === FpuOp.FPDIV_S || io.op === FpuOp.FPDIV_D)) {
    // 0/0: produce DivZeroByZeroNaN, set NV flag
    io.result := nanResult
    io.flags.NV := True
  } elsewhen(io.a.isZero) {
    // A is zero, B is non-zero and finite: propagate zero
    io.result.sign := io.a.sign
    io.result.exp := U(0, 11 bits)
    io.result.mant := B(0, 52 bits)
  }
}