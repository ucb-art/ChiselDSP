package ChiselDSP
import Chisel._

/** Dbl LUT IO where
  * depth = depth of LUT
  * gen = Dbl values (DSPFixed or DSPDbl)
  */
class DblLUTIO [T <: DSPQnm[T]](depth: Int, gen : => T) extends IOBundle {
  val addr = DSPUInt(INPUT,(depth-1).max(0))
  val dout = gen.asOutput
}

/** LUT that calculates outputs from a list of Scala Complex's */
class DblLUT [T <: DSPQnm[T]](x: List[Double], gen : => T, inDelay: Int = 0) extends
GenDSPModule (gen, inputDelay = inDelay) {

  // TODO: Make 0 length lists have optional IO = None; allow real,imag widths to be different (based off of range)

  val depth = x.length

  override val io = new DblLUTIO(depth, gen)

  io.addr.doNothing()

  // TODO: Bring out pad lit function
  // TODO: Debug combined R,I Fixed
  // TODO: Don't separate DSPFixed and DSPDbl (get weird compilation error with DSPDbl -- use of undeclared identifier)
  val temp = {
    if (depth > 0) {
      val LUT = Vec(x.map(double2T(_)))
      LUT(io.addr.toUInt)
    }
    else {
      double2T(0.0)
    }
  }

  io.dout := temp
  io.dout.passDelay(io.addr,0)

  // Assign output range based off of min,max real,imag values in the list
  if (depth > 0) {
    gen match {
      case f: DSPFixed => {
        val range = (
          DSPFixed.toFixed(x.min, f.getFracWidth),
          DSPFixed.toFixed(x.max, f.getFracWidth)
        )
        io.dout.rangeOverride(range)
      }
      case _ =>
    }
  }
}