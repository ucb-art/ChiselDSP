package ChiselDSP
import Chisel._

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
          case x: Vec[_] => Vec(x.map(y => apply(y, init = null.asInstanceOf[y.type], clock = clock)))
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