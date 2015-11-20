/** MyBool class customizations
  * TO DO: Support better pipeline parameterization.
  */
package ChiselDSP
import Chisel._

object MyBool {

  /** Creates a MyBool object from a constant true/false */
  def apply(x: Boolean): MyBool = {
    val res = Lit(if(x) 1 else 0, 1){MyBool()}
    res.asDirectionless
    res
  }
  
  /** Convert 1-bit Bits to a MyBool */
  def apply(x: Bits): MyBool = {
    if (x.getWidth != 1) throwException("Node to be converted to MyBool must have width = 1")
    chiselCast(x){MyBool(x.dir)}
  }

  /** MyBool construction */
  def apply(dir: IODirection = NODIR): MyBool = {
    val res = new MyBool()
    res.dir = dir
    res.init("", 1)
    res
  }

}

class MyBool extends Bits{

  /** Print MyBool width */
  def printWidth() : String = ("1-bit bool")

  type T = MyBool
  
  /** Lit val is true */
  def isTrue: Boolean = litValue() == 1
  
  private var noUpdate = false  
  
  /** Marks the signal as being used to prevent future updates */
  def used(): Unit = (noUpdate = true)   
  
  /** Returns the signal. Marks it as used */
  def doNothing() : MyBool = {this.used(); this}
  
  /** Create a Bool from an Int */
  override def fromInt(x: Int): this.type = MyBool(x > 0).asInstanceOf[this.type]
  
  /** Factory method to create and assign a *Bool* type to a Node *n*. */
  override def fromNode(n: Node): this.type = MyBool(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
 
  /** Implementation of := operator, assigns value to this MyBool */
  override protected def colonEquals(that: Bits): Unit = that match {
    case b: MyBool => super.colonEquals(b)
    case _ => illegalAssignment(that)
  }
 
  /** Delay n clock cycles */
  def pipe (n: Int): MyBool = {
    this.used()
    if (this.isLit) this
    else ShiftRegister(this,n)
  }
  
  /** Register */
  def reg(): MyBool = {
    this.used()
    if (this.isLit) this
    else Reg(next = this)
  }
  
  /** Change INPUT to OUTPUT and OUTPUT to INPUT. NODIR stays the same. */
  override def flip: this.type = {
    dir match {
      case INPUT => dir = OUTPUT
      case OUTPUT => dir = INPUT
      case NODIR => dir = NODIR
    }
    this
  }
  
  /** Bitwise and */
  def & (b: MyBool): MyBool = {
    this.used(); b.used();
    if (this.isLit) {if (this.isTrue) b else MyBool(false)}
    else if (b.isLit) {if (b.isTrue) this else MyBool(false)}
    else MyBool(this.toBool & b.toBool)
  }
  def ? (b: MyBool): MyBool = this & b
  
  /** Bitwise or */
  def | (b: MyBool): MyBool = {
    this.used(); b.used()
    if (this.isLit) {if (this.isTrue) MyBool(true) else b}
    else if (b.isLit) {if(b.isTrue) MyBool(true) else this}
    else MyBool(this.toBool | b.toBool)  
  }
  def /| (b: MyBool): MyBool = this | b
  
  /** Invert */
  def unary_!(): MyBool = {
    this.used()
    if (this.isLit) {MyBool(!this.isTrue)}
    else MyBool(~(this.toBool))
  }
  
  /** Bitwise xor */
  def ^ (b: MyBool): MyBool = {
    this.used(); b.used()
    if (this.isLit && b.isLit) {MyBool((this.litValue() ^ b.litValue()) > 0)}
    else if (this.isLit) {if (this.isTrue) ~b else b}
    else if (b.isLit) {if (b.isTrue) ~this else this}
    else MyBool(this.toBool ^ b.toBool)
  }
  
}
