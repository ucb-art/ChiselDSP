/** Utility functions */

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

/** Register a value; keeps track of additional info */
object RegNext {

  def apply [T <: Data](x: T, init: T = null.asInstanceOf[T], clock: Clock = null) : T = {
    (
      if (init != null){
        (x,init) match {
          case (x: Complex[_], init: Complex[_]) => {
            (x.real,x.imag,init.real,init.imag) match {
              case (xr: DSPFixed,xi: DSPFixed,initr: DSPFixed,initi: DSPFixed) => {
                Complex(xr.asInstanceOf[DSPFixed].reg(initr,clock),xi.asInstanceOf[DSPFixed].reg(initi,clock))
              }
              case (xr: DSPDbl,xi: DSPDbl,initr: DSPDbl,initi: DSPDbl) => {
                Complex(xr.asInstanceOf[DSPDbl].reg(initr,clock),xi.asInstanceOf[DSPDbl].reg(initi,clock))
              }
            }
          }
          case (x: DSPBool, init: DSPBool) => x.reg(init,clock)
          case (x: DSPUInt, init: DSPUInt) => x.reg(init,clock)
          case (x: DSPFixed, init: DSPFixed) => x.reg(init,clock)
          case (x: DSPDbl, init: DSPDbl) => x.reg(init,clock)
          case (x: Bits, init: Bits) => apply_dsp(x,init,clock)
          case (x: BaseN, init: BaseN) => {
            if (x.rad != init.rad) Error("BaseN RegNext initialization should have the same rad as next value")
            BaseN(x.zip(init).map(y => y._1.reg(init = y._2, clock = clock)), x.rad)
          }
          case (x: Vec[_], init: Vec[_]) => {
            Vec(x.zip(init).map(y => RegNext(y._1,y._2,clock)))
          }
          case (_, _) => {Error("Incompatible element type for RegNext"); x}
        }
      }
      else{
        x match {
          case x: Complex[_] => x.reg(clock = clock)
          case x: DSPBits[_] => x.reg(clock = clock)
          case x: Bits => apply_dsp(x,clock = clock)
          case x: BaseN => BaseN(x.map(_.reg(clock = clock)),x.rad)
          case x: Vec[_] => Vec(x.map( _ => { _ match {
            case y: Data => RegNext(y,clock = clock) 
          }}))
          case _ => {Error("Incompatible element type for RegNext"); x}
        }
      }
    ).asInstanceOf[T]

  }

  /** Pull in original Chisel RegNext functionality */
  private[ChiselDSP] def apply_dsp[T <: Bits](next: T, init: T = null.asInstanceOf[T], clock: Clock = null): T = {
    if (init != null) {
      (next,init) match {
        case (next: DSPBits[_], init: DSPBits[_]) => {
          if (init.getRange.min < next.getRange.min || init.getRange.max > next.getRange.max)
            Error("Range of RegNext initial value should be in the range of the 'next' value")
        }
        case (_,_) =>
      }
      if (!init.isLit) Error("RegNext Initial value should be a Lit")
      // TODO: Replace with padding
      if (init.getWidth != next.getWidth) Error("RegNext initial value should have the same width as the 'next' value")
    }
    val temp = {
      if (clock != null && init != null) Reg[T](Some(next), Some(next), Some(init), Some(clock))
      else if (clock == null && init == null) Reg[T](Some(next), Some(next), None, None)
      else if (clock == null) Reg[T](Some(next), Some(next), Some(init), None)
      else Reg[T](Some(next), Some(next), None, Some(clock))
    }.toBits
    chiselCast(temp){next.cloneType}
  }

}

/** Initialize register */
object RegInit {

  // TODO: Simplify RegNext (x.reg()) since it already passes all the info?
  /** Pull in Chisel RegInit functionality */
  private def apply_dsp [T<: Data](init: T, clock: Clock = null) : T = {
    if (clock == null) Reg[T](Some(init), None, Some(init), None)
    else Reg[T](Some(init), None, Some(init), Some(clock))
  }

  def apply [T <: Data](init: T, clock: Clock = null) : T = {
    init match {
      case a: Bits => {if (! init.isLit) Error ("RegInit Initial value should be a Lit")}
      case _ =>
    }
    (init match {
      case init: Complex[_] => Complex(RegInit(init.real,clock),RegInit(init.imag,clock))
      case init: DSPFixed => {
        val out = apply_dsp(init,clock)
        out.updateLimits(DSPFixed.toRange(out.getWidth))
        out
      }
      case init: DSPBits[_] => apply_dsp(init,clock)
      case init: Bits => apply_dsp(init,clock)
      case init: BaseN => BaseN(init.map(RegInit(_,clock)),init.rad)
      case init: Vec[_] => Vec(init.map(RegInit(_,clock)))
      case _ => Error("Incompatible element type for RegInit")
    }).asInstanceOf[T]
  }

}

/** Pipe (x, n, [optional] en)
  * Delay all signals in x by n cycles (optional enable en)
  */
object Pipe {
  def apply[T <: Data](x: T, n: Int): T = apply(x,n,DSPBool(true))
  def apply[T <: Data](x: T, n: Int, en: DSPBool): T = {
    val out = x match {
      case t: DSPBits[_] => t.pipe(n, en)
      case b: Bits => {
        en.doNothing()
        val temp = ShiftRegister(b,n,en.toBool).toBits
        chiselCast(temp){b.cloneType}
      }
      case b: BaseN => BaseN(b.map(_.pipe(n,en)),b.rad)
      case v: Vec[_] => Vec(v.map(apply(_, n, en)))
      case c: Complex[_] => c.pipe(n, en)
      case _ => Error("Incompatible element type for Pipe.")
    }
    out.asInstanceOf[T]
  }
}

//----------------------------------------------------

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

//----------------------------------------------------

// TODO: Mux on 2 Vecs (of the same length)
/** Mux (sel,tc,fc)
  * sel = (false,true) --> (fc,tc)
  */
object Mux {
  def apply [T <: Data](sel: DSPBool, tc: T, fc: T): T = {
    ((tc,fc) match {
      case (tc: Complex[_], fc: Complex[_]) => {
        (tc.real,tc.imag,fc.real,fc.imag) match {
          case (tr: DSPFixed,ti: DSPFixed,fr: DSPFixed,fi: DSPFixed) => Complex(Mux(sel,tr,fr),Mux(sel,ti,fi))
          case (tr: DSPDbl,ti: DSPDbl,fr: DSPDbl,fi: DSPDbl) => Complex(Mux(sel,tr,fr),Mux(sel,ti,fi))
        }
      }
      case (tc: DSPBool, fc: DSPBool) => (fc ? !sel) /| (tc ? sel)
      case (tc: DSPUInt, fc: DSPUInt) => (fc ? !sel) /| (tc ? sel)
      case (tc: DSPFixed, fc: DSPFixed) => (fc ? !sel) /| (tc ? sel)
      case (tc: DSPDbl, fc: DSPDbl) => (fc ? !sel) /| (tc ? sel)
      case (tc: BaseN, fc: BaseN) => {
        tc.sameType(fc)
        BaseN(tc.zip(fc).map{case (t,f) => Mux(sel,t,f)},tc.rad)
      }
      case (_,_) => {Error("Unsupported mux type"); tc}
    }).asInstanceOf[T]
  }
  def apply (sel: Bool, tc: UInt, fc: UInt) : UInt = Chisel.Mux(sel,tc,fc)
  def apply (sel: Bool, tc: SInt, fc: SInt) : SInt = Chisel.Mux(sel,tc,fc)
  def apply (sel: Bool, tc: Bool, fc: Bool) : Bool = Chisel.Mux(sel,tc,fc)
  def apply (sel: Bool, tc: Fixed, fc: Fixed) : Fixed = Chisel.Mux(sel,tc,fc)
  def apply (sel: Bool, tc: Flo, fc: Flo) : Flo = Chisel.Mux(sel,tc,fc)
  def apply (sel: Bool, tc: Dbl, fc: Dbl) : Dbl = Chisel.Mux(sel,tc,fc)
  def apply [T <: DSPQnm[T]] (sel: DSPBool, tc: Complex[T], fc: Complex[T]) : Complex[T] = (fc ? !sel) /| (tc ? sel)

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
