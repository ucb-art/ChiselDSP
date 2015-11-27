/** New Complex -- Note that operations are defined outside of the data type, since hardware operations 
  * are highly customized (SQNR, resource, timing, etc. constrained)
  */

package ChiselDSP
import Chisel._

object MyComplex {

  /** Create a new Complex number: real + imag*i
    * @tparam T the type to represent the complex number with, eg MyFixed, MyDbl
    */
  def apply[T<:MyBits with MyNum[T]](real: T, imag: T) = new Complex(real, imag)
}

/** Complex number representation */
class Complex[T<:MyBits with MyNum[T]](val real: T, val imag: T) extends Bundle 
