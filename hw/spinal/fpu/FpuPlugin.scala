package fpu

import spinal.core._
import spinal.core.fiber._
import spinal.lib.misc.plugin._

trait FpuPlugin[T <: FpuIntermediateState] extends FiberPlugin {
  val config: FPUConfig
  val io: Bundle = new Bundle {
    val input = in(new FpuPayload[T](config, intermediateFactory))
    val output = out(new FpuPayload[T](config, intermediateFactory))
    val active = in Bool() // Indicates plugin should process this cycle
  }

  def intermediateFactory: FPUConfig => T
  def process(): Unit // Plugin-specific logic
}