// TODO: Max, min of Vec, Vec of Complex

package ChiselDSP
import Chisel._

// TODO: Overflow handling
/** Trim fractional bits */
object Trim {
  def apply [T <: Data](x: T, n: Int, tType: TrimType = Complex.opts.trimType) : T = {
    val out = x match {
      case v: Vec[_] => Vec(v.map(apply(_,n,tType)))
      case t: DSPQnm[_] => {
        if (tType == Truncate) t $ n
        else if (tType == Round) t $$ n
        else t
      }
      case c: Complex[_] => c.trim(n,tType)
      case _ => Error("Incompatible element type for Trim.")
    }
    out.asInstanceOf[T]
  }
}

/** Shift all elements left (arithmetically) by amount n */
object SLA {
  def apply [T <: Data](x: T, n:Int) : T = {
    val out = x match {
      case v: Vec[_] => Vec(v.map(apply(_,n)))
      case t: DSPNum[_] => t << n
      case c: Complex[_] => c << n
      case _ => Error("Incompatible element type for SLA.")
    }
    out.asInstanceOf[T]
  }
  def apply [T <: Data](x: T, n:DSPUInt) : T = {
    val out = x match {
      case v: Vec[_] => Vec(v.map(apply(_,n)))
      case t: DSPNum[_] => t << n
      case c: Complex[_] => c << n
      case _ => Error("Incompatible element type for SLA.")
    }
    out.asInstanceOf[T]
  }
}

/** Shift all elements right (arithmetically) by amount n*/
object SRA {
  def apply [T <: Data](x: T, n:Int) : T = {
    val out = x match {
      case v: Vec[_] => Vec(v.map(apply(_,n)))
      case t: DSPNum[_] => t >> n
      case c: Complex[_] => c >> n
      case _ => Error("Incompatible element type for SRA.")
    }
    out.asInstanceOf[T]
  }
  def apply [T <: Data](x: T, n:DSPUInt) : T = {
    val out = x match {
      case v: Vec[_] => Vec(v.map(apply(_,n)))
      case t: DSPNum[_] => t >> n
      case c: Complex[_] => c >> n
      case _ => Error("Incompatible element type for SRA.")
    }
    out.asInstanceOf[T]
  }
}

/** Short [DSPUInt] Mod (x,n,[optional] dly)
  * Returns (x % n) for x <= 2n-1
  * & overflow flag set when (x >= n)
  * dly: output delay
  */
object Mod {
  def apply(x:DSPUInt, n:DSPUInt) : Tuple2[DSPUInt,DSPBool] = apply(x,n,0)
  def apply(x:DSPUInt, n:DSPUInt, dly:Int) : Tuple2[DSPUInt,DSPBool] = {
    val nmax = n.getRange.max
    val xmax = x.getRange.max
    val xValidMax = 2*nmax-1
    if (xmax > xValidMax)
      Error("Cannot use short mod x%n for given range of inputs. x <= 2n-1 required."
        + " (x,n) max --> (" + xmax + "," + nmax + ")")
    if (xmax < nmax) {
      n.doNothing()
      (x.pipe(dly),DSPBool(false))                                            // No mod needed when x < n
    }
    else {
      val newx = x.lengthen(xValidMax)
      val diff = (newx - n).pipe(dly)                                         // No FPGA retiming issue @ carry chain
      if (diff.getWidth != newx.getWidth)
        Error ("Sign bit location after subtraction is unexpected in Mod.")
      val nOF  = diff.extract(newx.getWidth-1)                                // x >= n -> mod = x-n; else mod = x
      val mod = Mux(nOF,x.pipe(dly),diff)
      (mod.shorten(nmax-1),!nOF)
    }
  }
}