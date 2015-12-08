/** New Complex 
  * TODO: +,- (unary-),* Complex [ how many muls ], * Real, * Imag, Conj, abs 
  * (all variations, with delays, truncation/round, overflow)
  * Case class parameterization?
  */

package ChiselDSP
import Chisel._

object Complex {

  /** Create a new Complex number: real + imag*i
    * @tparam T the type to represent the complex number with, eg DSPFixed, DSPDbl
    */
  def apply[T <: DSPQnm[_]](real: T, imag: T) : Complex[T] = new Complex(real, imag)
  
  /** Creates non-Chisel complex class if real, imag inputs are type Scala Double */
  def apply(real:Double, imag:Double) : ScalaComplex = new ScalaComplex(real, imag)
  
}

/** Complex class for normal Scala */
class ScalaComplex (var real:Double, var imag:Double){
  override def toString() = {real + (if (imag < 0) " - " + (-imag) else " + " + imag) + " i"}
  def toList() = List(real,imag)
}

/** Complex number representation */
class Complex[T <: DSPQnm[_]](val real: T, val imag: T) extends Bundle {

  /** Pipe (n, [optional] en)
  * Delay complex by n cycles (optional enable en)
  */
  def pipe (n: Int, en: DSPBool = DSPBool(true)): Complex[T] = Complex(real.pipe(n,en),imag.pipe(n,en))

  /** Register that keeps track of additional info */
  def reg(): Complex[T] = Complex(real.reg(),imag.reg())
  
  /** Select function: s = true -> this; else 0 */
  def ? (s: DSPBool) : Complex[T] = Complex(real ? s, imag ? s)
  
  /** Custom bitwise or for muxing */
  def /| [U <: Complex[T]](b: U) : U = Complex(real /| b.real, imag /| b.imag)
  
  /** ARITHMETIC Right shift n */
  def >> (n: Int) : Complex[T] = Complex( real >> n, imag >> n)
  /** Variable ARITHMETIC Right shift n */
  def >> (n: DSPUInt) : Complex[T] = Complex( real >> n, imag >> n)
  
   /** ARITHMETIC Left shift n */
  def << (n: Int) : Complex[T] = Complex( real << n, imag << n)
  /** Variable ARITHMETIC Left shift n */
  def << (n: DSPUInt) : Complex[T] = Complex( real << n, imag << n)
  
  /** Equality check */
  def === (b: Complex[T]): DSPBool = {
    val req = (real === b.real)
    val ieq = (imag === b.imag)
    req & ieq
  }
  
  /** Inequality check */
  def =/= (b: Complex[T]): DSPBool = {
    val rneq = (real =/= b.real)
    val ineq = (imag =/= b.imag)
    rneq | ineq
  }
  def != (b: Complex[T]): DSPBool = (this =/= b)
  
}
