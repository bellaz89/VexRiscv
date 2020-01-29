package vexriscv

import vexriscv.plugin._
import spinal.core._
import spinal.lib.float._

import scala.collection.mutable.ArrayBuffer

object RiscvRoundingMode extends SpinalEnum {
  val RNE = newElement() // Round to Nearest, ties to Even
  val RTZ = newElement() // Round towards Zero
  val RDN = newElement() // Round Down (towards -inf)
  val RUP = newElement() // Round Up   (towards +inf) 
  val RMM = newElement() // Round to Nearest, ties to Max Magnitude
  val DYN = newElement() // In instruction's rm field, selects dynamic
  val INV = newElement()

  def roundingFromBits(b : Bits) : RiscvRoundingMode.C = {
    val r = RiscvRoundingMode()

    switch(b){
      is(B"000") { r := RNE }
      is(B"001") { r := RTZ }
      is(B"010") { r := RDN }
      is(B"011") { r := RUP }
      is(B"100") { r := RMM }
      is(B"111") { r := DYN }
      default { r := INV }
    }
    r
  }

  def bitsFromRounding(r : RiscvRoundingMode.C) : Bits = {
    val b = Bits(3 bits)
    switch(r) {
      is(RNE) { b := B"000" }
      is(RTZ) { b := B"001" }
      is(RDN) { b := B"010" }
      is(RUP) { b := B"011" }
      is(RMM) { b := B"100" }
      is(DYN) { b := B"111" }
      default { b := B"101" }
    }
    b
  }
}

class RiscvFloatExceptions extends Bundle {

  val NV = Bool
  val DZ = Bool
  val OF = Bool
  val UF = Bool
  val NX = Bool

  def orExceptions(e : RiscvFloatExceptions) : RiscvFloatExceptions = {
    val or = new RiscvFloatExceptions
    or.NV := e.NV | NV
    or.DZ := e.DZ | DZ
    or.OF := e.OF | OF
    or.UF := e.UF | UF
    or.NX := e.NX | NX
    or
  }
}


object VexRiscvConfig{
  def apply(withMemoryStage : Boolean, 
            withWriteBackStage : Boolean, 
            plugins : Seq[Plugin[VexRiscv]]): VexRiscvConfig = {

      val config = VexRiscvConfig()
      config.plugins ++= plugins
      config.withMemoryStage = withMemoryStage
      config.withWriteBackStage = withWriteBackStage
      config
  }

  def apply(withMemoryStage : Boolean, 
            withWriteBackStage : Boolean, 
            plugins : Seq[Plugin[VexRiscv]],
            withFloat : Boolean,
            floatExecuteStages : Int): VexRiscvConfig = {

      val config = VexRiscvConfig()
      config.plugins ++= plugins
      config.withMemoryStage = withMemoryStage
      config.withWriteBackStage = withWriteBackStage
      config.withFloat = withFloat
      config.floatExecuteStages = floatExecuteStages
      config
  }
  
  def apply(plugins : Seq[Plugin[VexRiscv]] = ArrayBuffer()) : VexRiscvConfig = apply(true,true,plugins)
  def withFloat(plugins : Seq[Plugin[VexRiscv]] = ArrayBuffer(),
                floatExecuteStages : Int = 1) : VexRiscvConfig = apply(true,
                                                                       true,
                                                                       plugins,
                                                                       true,
                                                                       floatExecuteStages)
}

case class VexRiscvConfig(){
  var withMemoryStage = true
  var withWriteBackStage = true
  var withFloat = false
  var floatExecuteStages =  1
  val plugins = ArrayBuffer[Plugin[VexRiscv]]()

  def add(that : Plugin[VexRiscv]) : this.type = {plugins += that;this}

  //Default Stageables
  object IS_RVC extends Stageable(Bool)
  object BYPASSABLE_EXECUTE_STAGE   extends Stageable(Bool)
  object BYPASSABLE_MEMORY_STAGE   extends Stageable(Bool)
  object RS1   extends Stageable(Bits(32 bits))
  object RS2   extends Stageable(Bits(32 bits))
  object RS1_USE extends Stageable(Bool)
  object RS2_USE extends Stageable(Bool)
  object RESULT extends Stageable(UInt(32 bits))
  object PC extends Stageable(UInt(32 bits))
  object PC_CALC_WITHOUT_JUMP extends Stageable(UInt(32 bits))
  object INSTRUCTION extends Stageable(Bits(32 bits))
  object INSTRUCTION_READY extends Stageable(Bool)
  object INSTRUCTION_ANTICIPATED extends Stageable(Bits(32 bits))
  object LEGAL_INSTRUCTION extends Stageable(Bool)
  object REGFILE_WRITE_VALID extends Stageable(Bool)
  object REGFILE_WRITE_DATA extends Stageable(Bits(32 bits))

  object MPP extends PipelineThing[UInt]
  object DEBUG_BYPASS_CACHE extends PipelineThing[Bool]

  object SRC1   extends Stageable(Bits(32 bits))
  object SRC2   extends Stageable(Bits(32 bits))
  object SRC_ADD_SUB extends Stageable(Bits(32 bits))
  object SRC_ADD extends Stageable(Bits(32 bits))
  object SRC_SUB extends Stageable(Bits(32 bits))
  object SRC_LESS extends Stageable(Bool)
  object SRC_USE_SUB_LESS extends Stageable(Bool)
  object SRC_LESS_UNSIGNED extends Stageable(Bool)
  object SRC_ADD_ZERO extends Stageable(Bool)


  object HAS_SIDE_EFFECT extends Stageable(Bool)

  //Formal verification purposes
  object FORMAL_HALT       extends Stageable(Bool)
  object FORMAL_PC_NEXT    extends Stageable(UInt(32 bits))
  object FORMAL_MEM_ADDR   extends Stageable(UInt(32 bits))
  object FORMAL_MEM_RMASK  extends Stageable(Bits(4 bits))
  object FORMAL_MEM_WMASK  extends Stageable(Bits(4 bits))
  object FORMAL_MEM_RDATA  extends Stageable(Bits(32 bits))
  object FORMAL_MEM_WDATA  extends Stageable(Bits(32 bits))
  object FORMAL_INSTRUCTION extends Stageable(Bits(32 bits))


  object Src1CtrlEnum extends SpinalEnum(binarySequential){
    val RS, IMU, PC_INCREMENT, URS1 = newElement()   //IMU, IMZ IMJB
  }

  object Src2CtrlEnum extends SpinalEnum(binarySequential){
    val RS, IMI, IMS, PC = newElement() //TODO remplacing ZERO could avoid 32 muxes if SRC_ADD can be disabled
  }

  object SRC1_CTRL  extends Stageable(Src1CtrlEnum())
  object SRC2_CTRL  extends Stageable(Src2CtrlEnum())

  // F extension

  val FLOAT_RS1 = if(withFloat) new Stageable(new RecFloat33) else null
  val FLOAT_RS2 = if(withFloat) new Stageable(new RecFloat33) else null
  val FLOAT_RS3 = if(withFloat) new Stageable(new RecFloat33) else null
  val FLOAT_RS1_USE = if(withFloat) new Stageable(Bool) else null
  val FLOAT_RS2_USE = if(withFloat) new Stageable(Bool) else null
  val FLOAT_RS3_USE = if(withFloat) new Stageable(Bool) else null
  val FLOAT_RESULT = if(withFloat) new Stageable(new RecFloat33) else null
  val FLOAT_REGFILE_WRITE_VALID = if(withFloat) new Stageable(Bool) else null
  val FLOAT_REGFILE_WRITE_DATA = if(withFloat) new Stageable(new RecFloat33) else null
  val FLOAT_EXCEPTION = if(withFloat) new Stageable(new RiscvFloatExceptions) else null
  val FLOAT_ROUNDING = if(withFloat) new Stageable(RiscvRoundingMode()) else null
  val FLOAT_USE_ROUNDING = if(withFloat) new Stageable(Bool) else null

  object FloatSrc1CtrlEnum extends SpinalEnum(binarySequential){
    val RS = newElement()
  }

  object FloatSrc2CtrlEnum extends SpinalEnum(binarySequential){
    val RS, IMI, IMS = newElement()
  }

  object FloatSrc3CtrlEnum extends SpinalEnum(binarySequential){
    val RS  = newElement()
  }
  
  val FLOAT_SRC1_CTRL = if(withFloat) new Stageable(FloatSrc1CtrlEnum()) else null
  val FLOAT_SRC2_CTRL = if(withFloat) new Stageable(FloatSrc2CtrlEnum()) else null
  val FLOAT_SRC3_CTRL = if(withFloat) new Stageable(FloatSrc3CtrlEnum()) else null
}



object RVC_GEN extends PipelineThing[Boolean]
class VexRiscv(val config : VexRiscvConfig) extends Component with Pipeline{
  type  T = VexRiscv
  import config._

  //Define stages
  def newStage(): Stage = { val s = new Stage; stages += s; s }
  val decode    = newStage()
  val execute   = newStage()
  val memory    = ifGen(config.withMemoryStage)    (newStage())
  val writeBack = ifGen(config.withWriteBackStage) (newStage())
  val floatExecute = ArrayBuffer[Stage]

  val stagesFromExecuteOnlyInt = stages.dropWhile(_ != execute)

  if (withFloat && (stagesFromExecuteOnlyInt.length) < floatExecuteStages+1) {
      (0 to floatExecuteStages-stagesFromExecuteOnlyInt.length).foreach{ newStage() }
  }
  
  def stagesFromExecute = stages.dropWhile(_ != execute)
    
  if (withFloat) (0 to floatExecuteStages-1).foreach{ i => floatExecute += stagesFromExecute(i)}

  val floatWriteBack = if (withFloat) stagesFromExecute(floatExecuteStages) else null

  plugins ++= config.plugins

  //regression usage
  val lastStageInstruction = CombInit(stages.last.input(config.INSTRUCTION)) keep() addAttribute (Verilator.public)
  val lastStagePc = CombInit(stages.last.input(config.PC)) keep() addAttribute (Verilator.public)
  val lastStageIsValid = CombInit(stages.last.arbitration.isValid) keep() addAttribute (Verilator.public)
  val lastStageIsFiring = CombInit(stages.last.arbitration.isFiring) keep() addAttribute (Verilator.public)

  //Verilator perf
  decode.arbitration.removeIt.noBackendCombMerge
  if(withMemoryStage){
    memory.arbitration.removeIt.noBackendCombMerge
  }
  execute.arbitration.flushNext.noBackendCombMerge

  this(RVC_GEN) = false
}


