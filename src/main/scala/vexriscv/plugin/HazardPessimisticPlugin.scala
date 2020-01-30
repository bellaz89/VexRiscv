package vexriscv.plugin

import vexriscv._
import spinal.core._
import spinal.lib._


class HazardPessimisticPlugin() extends Plugin[VexRiscv] {
  import Riscv._

  override def setup(pipeline: VexRiscv): Unit = {
    import pipeline.config._
    val decoderService = pipeline.service(classOf[DecoderService])
    decoderService.addDefault(HAS_SIDE_EFFECT, False)
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._
    
    val writesIntPipeline = (intStagesFromExecute.map(s => s.arbitration.isValid && s.input(REGFILE_WRITE_VALID)) :+ 
                             RegNext(intStages.last.arbitration.isValid && intStages.last.input(REGFILE_WRITE_VALID)))

    val writesInPipeline = Bool
    
    if(withFloat) {
      val writesFloatPipeline = (floatStagesFromExecute.map(s => s.arbitration.isValid && (s.input(FLOAT_REGFILE_WRITE_VALID) | s.input(FLOAT_RAISE_EXCEPTION))) :+ 
                                RegNext(floatStages.last.arbitration.isValid && floatStages.last.input(FLOAT_REGFILE_WRITE_VALID) | floatStages.last.input(FLOAT_RAISE_EXCEPTION)))
      writesInPipeline := writesIntPipeline.orR | writesFloatPipeline.orR
    } else {
      writesInPipeline := writesIntPipeline.orR
    }

    decode.arbitration.haltByOther.setWhen(decode.arbitration.isValid && writesInPipeline)
  }
}
