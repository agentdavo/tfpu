package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber._

trait FpuPlugin[T <: FpuIntermediateState] extends FiberPlugin {
  val config: FPUConfig

  override def build(): Unit = during setup {
    val io = new Bundle {
      val input = in(new FpuPayload[T](config, intermediateFactory))
      val output = out(new FpuPayload[T](config, intermediateFactory))
      val active = out Bool()
    }
    this.io = io
  }

  def intermediateFactory: FPUConfig => T
  def process(): Unit
}