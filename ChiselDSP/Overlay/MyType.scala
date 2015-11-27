/** Base for new types */
  
package ChiselDSP
import Chisel._

abstract trait MyNum[T <: Data] {

  def unary_-(): T
  def +  (b: T): T
  def *  (b: T): T
  def /  (b: T): T
  def %  (b: T): T
  def -  (b: T): T
  def <  (b: T): MyBool
  def <= (b: T): MyBool
  def >  (b: T): MyBool
  def >= (b: T): MyBool
  
  def << (n: Int): T
  def >> (n: Int): T
  def << (n: MyUInt): T
  def >> (n: MyUInt): T
  def ? (s: MyBool) : T
  def /| (b: T) : T
  
}

abstract class MyBits extends Bits {

  /** Delay n clock cycles */
  def pipe (n: Int): this.type = {
    this.used()
    if (this.isLit) this
    else ShiftRegister(this,n)
  }
  
  /** Register */
  def reg(): this.type = {
    this.used()
    if (this.isLit) this
    else Reg(next = this).asInstanceOf[this.type]
  }

  def printWidth(): String
  
  protected var noUpdate = false  
  
  /** Marks the signal as being used to prevent invalid future updates */
  def used(): Unit = (noUpdate = true)   
  /** Returns the signal. Marks it as used */
  def doNothing() : this.type = {this.used(); this}

  /** Change INPUT to OUTPUT and OUTPUT to INPUT. NODIR stays the same. */
  override def flip: this.type = {
    dir match {
      case INPUT => dir = OUTPUT
      case OUTPUT => dir = INPUT
      case NODIR => dir = NODIR
    }
    this
  }
  
}
