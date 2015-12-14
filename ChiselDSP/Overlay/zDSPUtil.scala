/** Utility functions */

package ChiselDSP
import Chisel._

/** Register that keeps track of additional info */
object Reg {
  def apply [T <: Data](x: T) : T = {
    val out = x match {
      case v: Vec[_] => Vec(v.map(apply(_)))
      case t: DSPBits[_] => t.reg()
      case c: Complex[_] => c.reg()
      case _ => Error("Incompatible Vec element type for Reg.")
    }
    out.asInstanceOf[T]
  }
}

/** Pipe (x, n, [optional] en)
  * Delay all signals in x by n cycles (optional enable en)
  */
object Pipe {
  def apply[T <: Data](x: T, n: Int, en: DSPBool = DSPBool(true)): T = {
    val out = x match {
      case v: Vec[_] => Vec(v.map(apply(_, n, en)))
      case t: DSPBits[_] => t.pipe(n, en)
      case c: Complex[_] => c.pipe(n, en)
      case _ => Error("Incompatible Vec element type for Pipe.")
    }
    out.asInstanceOf[T]
  }
}

//----------------------------------------------------

/** Shift all elements left (arithmetically) by amount n*/
object SLA {
  def apply [T <: Data](x: T, n:Int) : T = {
    val out = x match {
      case v: Vec[_] => Vec(v.map(apply(_,n)))
      case t: DSPNum[_] => t << n
      case c: Complex[_] => c << n
      case _ => Error("Incompatible Vec element type for SLA.")
    }
    out.asInstanceOf[T]
  }
  def apply [T <: Data](x: T, n:DSPUInt) : T = {
    val out = x match {
      case v: Vec[_] => Vec(v.map(apply(_,n)))
      case t: DSPNum[_] => t << n
      case c: Complex[_] => c << n
      case _ => Error("Incompatible Vec element type for SLA.")
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
      case _ => Error("Incompatible Vec element type for SRA.")
    }
    out.asInstanceOf[T]
  }
  def apply [T <: Data](x: T, n:DSPUInt) : T = {
    val out = x match {
      case v: Vec[_] => Vec(v.map(apply(_,n)))
      case t: DSPNum[_] => t >> n
      case c: Complex[_] => c >> n
      case _ => Error("Incompatible Vec element type for SRA.")
    }
    out.asInstanceOf[T]
  }
}

//----------------------------------------------------

/** Mux (sel,tc,fc)
  * sel = (false,true) --> (fc,tc)
  */
object Mux {
  def apply [T <: DSPBits[T]] (sel: DSPBool, tc: T, fc: T) : T = (fc ? (!sel)) /| (tc ? sel)
  def apply [T <: DSPQnm[T]] (sel: DSPBool, tc: Complex[T], fc: Complex[T]) : Complex[T] = (fc ? (!sel)) /| (tc ? sel)
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
      //if (diff.getWidth != newx.getWidth)
      //  Error ("Sign bit location after subtraction is unexpected in Mod.")
      val nOF  = diff.extract(newx.getWidth-1)                                // x >= n -> mod = x-n; else mod = x
      val mod = Mux(nOF,x.pipe(dly),diff) 
      (mod.shorten(nmax-1),!nOF)
    } 
  }
}
