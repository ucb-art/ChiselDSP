package ChiselDSP
import Chisel._

/** Complex LUT IO where
  * depth = depth of LUT
  * gen = type of real, imag values (DSPFixed or DSPDbl)
  */
class ComplexLUTIO [T <: DSPQnm[T]](depth: Int, gen : => T) extends IOBundle {
  val addr = DSPUInt(INPUT,(depth-1).max(0))
  val dout = Complex(gen).asOutput
}

/** LUT that calculates outputs from a list of Scala Complex's */
class ComplexLUT [T <: DSPQnm[T]](cx: List[ScalaComplex], gen : => T, inDelay: Int = 0) extends
GenDSPModule (gen, inputDelay = inDelay) {

  // TODO: Make 0 length lists have optional IO = None; allow real,imag widths to be different (based off of range)

  val depth = cx.length

  Status("Complex LUT depth: " depth)

  override val io = new ComplexLUTIO(depth, gen)

  io.addr.doNothing()

  // TODO: Bring out pad lit function
  // TODO: Debug combined R,I Fixed
  // TODO: Don't separate DSPFixed and DSPDbl (get weird compilation error with DSPDbl -- use of undeclared identifier)
  val temp = {
    if (depth > 0) {
      gen match {
        case gen: DSPFixed => {
          /*
          // Concatenate + store real,imag values in a LUT
          val LUT = Vec(cx.map(x => {
            val real = double2T(x.real, fixedParams)
            val imag = double2T(x.imag, fixedParams)
            val realPad = ZeroPadLit(real)
            val imagPad = ZeroPadLit(imag)
            val cat = "b".concat(realPad).concat(imagPad)
            val width = cat.length-1
            Lit(cat, width) {UInt(width = width)}
          }))

          // Convert back to Complex (separate concatenated bits). Real + imag components have the same width
          val out = LUT(io.addr.toUInt)
          val real = genCast(out(out.getWidth - 1, out.getWidth >> 1))
          val imag = genCast(out((out.getWidth >> 1) - 1, 0))
          Complex(real, imag)
          */
          val LUTr = Vec(cx.map(x => double2T(x.real)))
          val LUTi = Vec(cx.map(x => double2T(x.imag)))
          val outr = LUTr(io.addr.toUInt)
          val outi = LUTi(io.addr.toUInt)
          Complex(outr, outi)
        }
        case gen: DSPDbl => {
          val LUTr = Vec(cx.map(x => double2T(x.real)))
          val LUTi = Vec(cx.map(x => double2T(x.imag)))
          val outr = LUTr(io.addr.toUInt)
          val outi = LUTi(io.addr.toUInt)
          Complex(outr, outi)
        }
      }
    }
    else {
      Complex(double2T(0.0), double2T(0.0))
    }
  }

  io.dout := temp
  io.dout.passDelay(io.addr,0)

  // Assign output range based off of min,max real,imag values in the list
  val rangeSetup = cx.map(x => x.toList).transpose
  if (depth > 0) {
    gen match {
      case f: DSPFixed => {
        val realRange = (
          DSPFixed.toFixed(rangeSetup(0).min, f.getFracWidth),
          DSPFixed.toFixed(rangeSetup(0).max, f.getFracWidth)
          )
        io.dout.real.rangeOverride(realRange)
        val imagRange = (
          DSPFixed.toFixed(rangeSetup(1).min, f.getFracWidth),
          DSPFixed.toFixed(rangeSetup(1).max, f.getFracWidth)
          )
        io.dout.imag.rangeOverride(imagRange)
      }
      case _ =>
    }
  }
}