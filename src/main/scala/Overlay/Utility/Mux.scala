package ChiselDSP
import Chisel._

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
      case (tc: BaseN, fc: BaseN) => (fc ? !sel) | (tc ? sel)
      case (tc: Vec[_], fc: Vec[_]) => {
        if (tc.length != fc.length) Error("Mux of 2 Vecs requires Vecs to be the same length!")
        Vec(tc.zip(fc).map{case (t,f) => Mux(sel,t,f)})
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