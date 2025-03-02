package fpu

import spinal.core._
import spinal.lib._

class Fpu(config: FPUConfig = FPUConfig(64)) extends Component {
  val io = new Bundle {
    val inputA = in Bits(config.totalWidth bits)
    val inputB = in Bits(config.totalWidth bits)
    val inputC = in Bits(config.totalWidth bits)
    val operation = in(FpuOperation())
    val memAddress = in UInt(32 bits)
    val result = out Bits(config.totalWidth bits)
    val ready = out Bool()
    val specialResult = out Bits(config.totalWidth bits)
  }

  val database = new Database
  val host = database on new PluginHost

  def setupPlugins(): Unit = {
    val plugins = Seq[Hostable](
      new FpuConfigPlugin(config),
      new FPUStackPlugin(config),
      new VCUPlugin(config),
      new AdderSubtractorPlugin(config),
      new MultiplierPlugin(config, null),
      new DividerSqrtPlugin(config),
      new StatusPlugin(config),
      new NormalizerPlugin(config),
      new MicrocodeROMPlugin(config),
      new FpuCore(config)
    )
    host.asHostOf(plugins)
  }

  setupPlugins()
  val core = host.find[FpuCore](_.config == config)
  io <> core.io
}

object Fpu {
  def apply(config: FPUConfig = FPUConfig(64)): Fpu = new Fpu(config)
  def apply(config: FPUConfig, plugins: Seq[Hostable]): Fpu = {
    val fpu = new Fpu(config)
    fpu.host.asHostOf(plugins)
    fpu
  }
}