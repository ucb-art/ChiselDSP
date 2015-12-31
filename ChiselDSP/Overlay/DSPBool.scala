/** DSPBool class customizations */

package ChiselDSP
import Chisel._

object DSPBool {

  /** Creates a DSPBool object from a constant true/false */
  def apply(x: Boolean): DSPBool = {
    val res = Lit(if(x) 1 else 0, 1){apply()}
    res.asDirectionless
    res.assign()
  }
  
  /** Convert 1-bit Bits to a DSPBool */
  def apply(x: Bits): DSPBool = {
    if (x.getWidth != 1) Error("Node to be converted to DSPBool must have width = 1")
    val res = chiselCast(x){apply(x.dir)}
    res.assign()
  }

  /** DSPBool construction */
  def apply(dir: IODirection = NODIR): DSPBool = {
    val res = new DSPBool()
    res.dir = dir
    res.init("", 1)
    res
  }

}

class DSPBool extends DSPBits[DSPBool]{

  /** Clone this instantiation */
  override def cloneType: this.type = {
    val out = DSPBool(dir)
    out.copyInfo(this).asInstanceOf[this.type]
  }

  /** Lit val is true */
  def isTrue(): Boolean = (litValue() == 1)

  /** DSPBool info */
  override def infoString() : String = ("bool")

  type T = DSPBool
  
  /** Create a Bool from an Int */
  override def fromInt(x: Int): this.type = DSPBool(x > 0).asInstanceOf[this.type]
  
  /** Factory method to create and assign a bool type to a node n. */
  override def fromNode(n: Node): this.type = DSPBool(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
 
  /** Implementation of := operator, assigns value to this DSPBool */
  override protected def colonEquals(that: Bits): Unit = that match {
    case b: DSPBool => {
      reassign(b)
      super.colonEquals(b)
    }
    case _ => illegalAssignment(that)
  }
  
  /** Bitwise and */
  def & (b: DSPBool): DSPBool = {
    val out = {
      if (isLit) {if (isTrue) b else DSPBool(false)}
      else if (b.isLit) {if (b.isTrue) this else DSPBool(false)}
      else DSPBool(this.toBits & b.toBits)
    }
    out.pass2to1(this,b)
  }
  def ? (b: DSPBool): DSPBool = this & b
  
  /** Bitwise or */
  def | (b: DSPBool): DSPBool = {
    val out = {
      if (isLit) {if (isTrue) DSPBool(true) else b}
      else if (b.isLit) {if(b.isTrue) DSPBool(true) else this}
      else DSPBool(this.toBits | b.toBits)  
    }
    out.pass2to1(this,b)
  }
  def /| (b: DSPBool): DSPBool = this | b
  
  /** Invert */
  def unary_!(): DSPBool = {
    val out = {
      if (isLit) DSPBool(!isTrue)
      else DSPBool(~(this.toBits))
    }
    out.passThrough(this)
  }
  
  /** Bitwise xor */
  def ^ (b: DSPBool): DSPBool = {
    val out = {
      if (isLit && b.isLit) {DSPBool((litValue() ^ b.litValue()) > 0)}
      else if (isLit) {if (isTrue) !b else b}
      else if (b.isLit) {if (b.isTrue) !this else this}
      else DSPBool(this.toBits ^ b.toBits)
    }
    out.pass2to1(this,b)
  }
  
  /** Bool range always [0,1] */
  protected def updateLimits(range: (BigInt,BigInt)): Unit = {
    setRangeBits(range)
    setRange(range)
  }
  updateLimits((BigInt(0),BigInt(1)))

}
