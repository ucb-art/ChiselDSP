/** Base for new ChiselDSP types */
  
package ChiselDSP
import Chisel._
import scala.collection.mutable.Map

/** Additional operations for Fixed in Qn.m notation (and therefore MyDbl) */
abstract class Qnm[T <: Data] extends DSPNum[T] {
  // Truncate, round, overflow handling
}

/** Allow numeric operators */
abstract class DSPNum[T <: Data] extends DSPBits[T] {

  /** Don't allow divides (not synthesizable on FPGA -- designer should think carefully!) */
  def /  (b: T): T = error ("/ not allowed.").asInstanceOf[T]
  def %  (b: T): T = error ("% not allowed.").asInstanceOf[T]
  
  /** Add + subtract and different flavors (to be overridden) */
  def +  (b: T): T                // determines optimal # bits
  def +& (b: T): T = this + b     // (should) grow bit
  def +% (b: T): T = this + b     // (should) wrap
  def -  (b: T): T
  def -& (b: T): T = this - b
  def -% (b: T): T = this - b
  def unary_-(): T 
  
  def *  (b: T): T
  
  /** Bit shifts as alternatives to explicit multiply,divide by 2^n */
  def << (n: Int): T
  def >> (n: Int): T
  def << (n: DSPUInt): T
  def >> (n: DSPUInt): T
  
  /** Numeric comparison */
  def <  (b: T): DSPBool
  def <= (b: T): DSPBool
  def >  (b: T): DSPBool
  def >= (b: T): DSPBool
  
}

/** ChiselDSP type info (associated with each signal) 
  * rangeBits = max range allowed by signal width
  */
case class Info (
  var range:Map[String,BigInt]  = Map("min" -> BigInt(0), "max" -> BigInt(0)),
  var rangeBits:Map[String,BigInt]  = Map("min" -> BigInt(0), "max" -> BigInt(0)),
  var isUsed: Boolean = false,
  var isAssigned: Boolean = false,
  var dly:Int = 0
)

/** Bits */
abstract class DSPBits [T <: Data] extends Bits {

  /** Print width + other useful debug info */
  def infoString(): String = (this.getWidth + " bits")
  
  final protected def error(msg: String) : this.type = {throwException(msg); this}

  /** Info associated with signal */
  protected var info = Info()
  
  /** Marks the signal as being used to prevent invalid future updates */
  final def used() {info.isUsed = true}  
  /** Marks that node has been assigned */
  final protected def assigned() {info.isAssigned = true}
  /** Returns the signal. Marks it as used */
  final def doNothing() : this.type = {this.used; this}
 
  /** Change INPUT to OUTPUT and OUTPUT to INPUT. NODIR stays the same. */
  final override def flip: this.type = {
    dir match {
      case INPUT => dir = OUTPUT
      case OUTPUT => dir = INPUT
      case NODIR => dir = NODIR
    }
    this
  }
  
  /** Pass (range, etc.) info of input to output */
  private def passThrough[T <: DSPBits[_]](in: T) {
    in.used
    info.range = in.info.range
    info.rangeBits = in.info.rangeBits
    this.assigned
  }
  
  /** Delay n clock cycles */
  final def pipe (n: Int, en: DSPBool = DSPBool(true)): T = {
    en.used
    val res = {
      if (this.isLit) this
      else {
        val out = ShiftRegister(this,n,en.asInstanceOf[Bool])
        out.passThrough(this)
        out.info.dly = info.dly + n
        out
      }
    }
    res.asInstanceOf[T]
  }
  
  /** Register that keeps track of additional info */
  final def reg(): T = {
    val res = {
      if (this.isLit) this
      else {
        val out = Reg(next = this)
        out.passThrough(this)
        out.info.dly = info.dly + 1
        out
      }
    }
    res.asInstanceOf[T]
  }
  
  /** Pass info of input to output on comparison */
  protected def passComparison[T <: DSPBits[_]](in1: T, in2: T) {
    in1.used; in2.used
    this.assigned
    info.dly = in1.info.dly.max(in2.info.dly)
  }
   
  /** Equality check */
  final def === (b: T): DSPBool = {
    val out = DSPBool(this.asInstanceOf[Bits] === b.asInstanceOf[Bits])
    out.passComparison(this,b)
    out
  }
  
  /** Inequality check */
  final def =/= (b: T): DSPBool = {
    val out = DSPBool(this.asInstanceOf[Bits] != b.asInstanceOf[Bits])
    out.passComparison(this,b)
    out
  }
  final def != (b: T): DSPBool = {this =/= b}
  
  /** Enforce that bitwise operations return bits to explicitly destroy info */
  
  private val warn = " Additional info like ranges, delays, etc. will be lost."
  
  /** Bitwise negate */
  final override def unary_~(): this.type = error("Please cast to Bits (x.toBits) before performing bitwise negate." + warn)
  /** Bitwise and */
  final override def & (b: Bits): this.type = error("Please cast to Bits (x.toBits) before performing bitwise and. " + warn)
  /** Bitwise or */
  final override def | (b: Bits): this.type = error("Please cast to Bits (x.toBits) before performing bitwise or. " + warn)                                 
  /** Bitwise xor */
  final override def ^ (b: Bits): this.type = error("Please cast to Bits (x.toBits) before performing bitwise xor. " + warn) 
  /** Shift left (unsigned) */
  final override def <<(b: UInt): this.type = error("Please use DSPUInt or Int for left shift amount." + warn)
  /** Cat bits */
  final override def ##[T <: Data](right: T): this.type = error("Please cast to Bits (x.toBits) before performing bit concatenation. " + warn)
  /** Set bit 'off' */
  final override def bitSet(off: UInt, dat: UInt): this.type = error("Please cast to Bits (x.toBits) before manually setting bits. " + warn)
  
  /** Select function: s = true -> this; else 0 */
  def ? (s: DSPBool) : T
  /** Custom bitwise or for muxing on type T */
  def /| (b: T) : T

}
