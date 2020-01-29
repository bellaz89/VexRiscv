package vexriscv.plugin
import vexriscv._
import vexriscv.VexRiscv
import spinal.core._
import spinal.lib.float._

class VexRiscvFloat(pipeline : VexRiscv, 
  floatExtension : FloatExtensionPlugin) {
    import pipeline._
    import floatExtension.stageables._
  }

  trait FloatPlugin extends Plugin[VexRiscv] {

    var floatExtension : FloatExtensionPlugin = null

    def setup(pipeline : VexRiscvFloat) : Unit = {}
    def build(pipeline : VexRiscvFloat) : Unit

    override def setup(pipeline : VexRiscv) : Unit = {
      setup(new VexRiscvFloat(pipeline, floatExtension))
    }
    override def build(pipeline : VexRiscv) : Unit = {
      setup(new VexRiscvFloat(pipeline, floatExtension))
    }
  }

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

  class FloatExtensionPlugin(executeStages : Int, 
    plugins : Seq[FloatPlugin]) extends Plugin[VexRiscv] {

      object stageables {
        object FLOAT_RS1 extends Stageable(new RecFloat33)
        object FLOAT_RS2 extends Stageable(new RecFloat33)
        object FLOAT_RS3 extends Stageable(new RecFloat33)
        object FLOAT_RS1_USE extends Stageable(Bool)
        object FLOAT_RS2_USE extends Stageable(Bool)
        object FLOAT_RS3_USE extends Stageable(Bool)
        object FLOAT_RESULT extends Stageable(new RecFloat33)
        object FLOAT_REGFILE_WRITE_VALID extends Stageable(Bool)
        object FLOAT_REGFILE_WRITE_DATA extends Stageable(new RecFloat33)
        object FLOAT_EXCEPTION extends Stageable(new RiscvFloatExceptions)
        object FLOAT_ROUNDING extends  Stageable(RiscvRoundingMode())
        object FLOAT_USE_ROUNDING extends Stageable(Bool)


        object FloatSrc1CtrlEnum extends SpinalEnum(binarySequential){
          val RS, RSR4 = newElement()
        }

        object FloatSrc2CtrlEnum extends SpinalEnum(binarySequential){
          val RS, IMI, IMS, RSR4 = newElement()
        }

        object FloatSrc3CtrlEnum extends SpinalEnum(binarySequential){
          val RSR4  = newElement()
        }
        object FloatSRC1_CTRL  extends Stageable(FloatSrc1CtrlEnum())
        object FloatSRC2_CTRL  extends Stageable(FloatSrc2CtrlEnum())
        object FloatSRC3_CTRL  extends Stageable(FloatSrc3CtrlEnum())

      }

      for (p <- plugins) {
        p.floatExtension = this
      }

      override def setup(pipeline: VexRiscv): Unit = {
        plugins.foreach(_.pipeline = this.pipeline)
        pipeline.plugins ++= plugins
        plugins.foreach(_.setup(pipeline))
      }

      override def build(pipeline: VexRiscv): Unit = {}
    }
