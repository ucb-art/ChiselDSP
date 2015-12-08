/** Utility functions */

package ChiselDSP
import Chisel._

/** Register that keeps track of additional info */
object Reg {
  def apply [T <: DSPBits[_], V <: Vec[_], C <: Complex[_]] (x: V) : V = {
    Vec(x.map(
      _ match {
        case t : T => t.reg() 
        case v : V => apply(v)
        case c : C => c.reg()
        case _ => {Error("Incompatible Vec element type for Reg."); _ }
      }
    )).asInstanceOf[V]
  }
}

/** Pipe (x, n, [optional] en)
  * Delay all signals in x by n cycles (optional enable en)
  */
object Pipe {
  def apply [T <: DSPBits[_], V <: Vec[_], C <: Complex[_]] (x: V, n:Int, en: DSPBool = DSPBool(true)) : V = {
    Vec(x.map(
      _ match {
        case t : T => t.pipe(n,en) 
        case v : V => apply(v,n,en)
        case c : C => c.pipe(n,en)
        case _ => {Error("Incompatible Vec element type for Pipe."); _ }
      }
    )).asInstanceOf[V]
  }
}

//----------------------------------------------------

/** Shift all elements left (arithmetically) by amount n*/
object SLA {
  def apply [T <: DSPBits[_], V <: Vec[_], C <: Complex[_]] (x: V, n:Int) : V = {
    Vec(x.map(
      _ match {
        case t : T => t << n
        case v : V => apply(v,n)
        case c : C => c << n
        case _ => {Error("Incompatible Vec element type for SLA."); _ }
      }
    )).asInstanceOf[V]
  }
  def apply [T <: DSPBits[_], V <: Vec[_], C <: Complex[_]] (x: V, n:DSPUInt) : V = {
    Vec(x.map(
      _ match {
        case t : T => t << n
        case v : V => apply(v,n)
        case c : C => c << n
        case _ => {Error("Incompatible Vec element type for SLA."); _ }
      }
    )).asInstanceOf[V]
  }
}

/** Shift all elements right (arithmetically) by amount n*/
object SRA {
  def apply [T <: DSPBits[_], V <: Vec[_], C <: Complex[_]] (x: V, n:Int) : V = {
    Vec(x.map(
      _ match {
        case t : T => t >> n
        case v : V => apply(v,n)
        case c : C => c >> n
        case _ => {Error("Incompatible Vec element type for SRA."); _ }
      }
    )).asInstanceOf[V]
  }
  def apply [T <: DSPBits[_], V <: Vec[_], C <: Complex[_]] (x: V, n:DSPUInt) : V = {
    Vec(x.map(
      _ match {
        case t : T => t >> n
        case v : V => apply(v,n)
        case c : C => c >> n
        case _ => {Error("Incompatible Vec element type for SRA."); _ }
      }
    )).asInstanceOf[V]
  }
}

//----------------------------------------------------

/** Mux (s,tc,fc)
  * s = (false,true) --> (fc,tc)
  */
object Mux {
  def apply [T <: DSPBits[_]] (s: DSPBool, tc: T, fc: T) : T = (fc ? !sel) /| (tc ? sel)
  def apply [T <: Complex[_]] (s: DSPBool, tc: T, fc: T) : T = (fc ? !sel) /| (tc ? sel)
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
      val newx = DSPUInt(x,(x.getRange.min, xValidMax))
      val diff = (newx - n).pipe(dly)                                         // No FPGA retiming issue @ carry chain
      if (diff.getWidth != newx.getWidth) 
        Error ("Sign bit location after subtraction is unexpected in Mod.")
      val nOF  = diff.extract(newx.getWidth-1)                                // x >= n -> mod = x-n; else mod = x
      val mod = Mux(nOF,x.pipe(dly),diff) 
      (DSPUInt(mod,nmax-1),!nOF)
    } 
  }
}
