/** DSPBool class customizations
  * TO DO: Support better pipeline parameterization.
  */
package ChiselDSP
import Chisel._

object DSPBool {

  /** Creates a DSPBool object from a constant true/false */
  def apply(x: Boolean): DSPBool = {
    val res = Lit(if(x) 1 else 0, 1){DSPBool()}
    res.asDirectionless
    res
  }
  
  /** Convert 1-bit Bits to a DSPBool */
  def apply(x: Bits): DSPBool = {
    if (x.getWidth != 1) throwException("Node to be converted to DSPBool must have width = 1")
    chiselCast(x){DSPBool(x.dir)}
  }

  /** DSPBool construction */
  def apply(dir: IODirection = NODIR): DSPBool = {
    val res = new DSPBool()
    res.dir = dir
    res.init("", 1)
    res
  }

}

class DSPBool extends MyBits{

  /** Print DSPBool width */
  def printWidth() : String = ("1-bit bool")

  type T = DSPBool
  
  /** Lit val is true */
  def isTrue: Boolean = litValue() == 1
  
  /** Create a Bool from an Int */
  override def fromInt(x: Int): this.type = DSPBool(x > 0).asInstanceOf[this.type]
  
  /** Factory method to create and assign a *Bool* type to a Node *n*. */
  override def fromNode(n: Node): this.type = DSPBool(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
 
  /** Implementation of := operator, assigns value to this DSPBool */
  override protected def colonEquals(that: Bits): Unit = that match {
    case b: DSPBool => {that.used; super.colonEquals(b)}
    case _ => illegalAssignment(that)
  }
  
  /** Bitwise and */
  def & (b: DSPBool): DSPBool = {
    this.used(); b.used();
    if (this.isLit) {if (this.isTrue) b else DSPBool(false)}
    else if (b.isLit) {if (b.isTrue) this else DSPBool(false)}
    else DSPBool(this.toBool & b.toBool)
  }
  def ? (b: DSPBool): DSPBool = this & b
  
  /** Bitwise or */
  def | (b: DSPBool): DSPBool = {
    this.used(); b.used()
    if (this.isLit) {if (this.isTrue) DSPBool(true) else b}
    else if (b.isLit) {if(b.isTrue) DSPBool(true) else this}
    else DSPBool(this.toBool | b.toBool)  
  }
  def /| (b: DSPBool): DSPBool = this | b
  
  /** Invert */
  def unary_!(): DSPBool = {
    this.used()
    if (this.isLit) {DSPBool(!this.isTrue)}
    else DSPBool(~(this.toBool))
  }
  
  /** Bitwise xor */
  def ^ (b: DSPBool): DSPBool = {
    this.used(); b.used()
    if (this.isLit && b.isLit) {DSPBool((this.litValue() ^ b.litValue()) > 0)}
    else if (this.isLit) {if (this.isTrue) ~b else b}
    else if (b.isLit) {if (b.isTrue) ~this else this}
    else DSPBool(this.toBool ^ b.toBool)
  }
  
}
