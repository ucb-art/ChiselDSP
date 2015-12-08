/** Double class customizations */

package ChiselDSP
import Chisel._
import Node.fixWidth

object DSPDbl {

  /** Creates a DSPDbl object from a constant Double */
  def apply(x: Double): DSPDbl = {
    val res = Lit(java.lang.Double.doubleToLongBits(x), 64){ DSPDbl() }
    res.asDirectionless
    res.assign()
  }

  /** Creates a DSPDbl object with specified IO direction */
  def apply(dir: IODirection = NODIR): DSPDbl = {
    val res = new DSPDbl();
    res.dir = dir;
    res.init("", fixWidth(64))
    res
  }
  
}

class DSPDbl extends DSPQnm[DSPDbl] {

  /** Convert Bits to a DSPDbl by reinterpreting the Bits */
  private def toT(x: Bits) : DSPDbl = {
    val res = chiselCast(x){DSPDbl(x.dir)}
    res.assign()
  }

  /** Print DSPDbl info */
  override def infoString() : String = "double"

  type T = DSPDbl

  /** Convert an Integer to DSPDbl representation */
  override def fromInt(x: Int): this.type = DSPDbl(x.toDouble).asInstanceOf[this.type]
  
  override def fromNode(n: Node) = DSPDbl(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
  
  /** Reassign with ":=". Certain conditions must be enforced so delay is consistent */
  override protected def colonEquals(that : Bits): Unit = that match {
    case d: DSPDbl => {
      reassign(d)
      super.colonEquals(d)
    }
    case _ => illegalAssignment(that)
  }
  
  /** Ops using Dbl backend --------------------- */
  
  def + (b: DSPDbl): DSPDbl = {
    val out = newBinaryOp(b, "d+")
    out.pass2to1(this,b)
  }
  def - (b: DSPDbl): DSPDbl = {
    val out = newBinaryOp(b, "d-")
    out.pass2to1(this,b)
  }
  override def unary_-(): DSPDbl = {DSPDbl(0.0)-this}
  def * (b: DSPDbl): DSPDbl = {
    val out = newBinaryOp(b, "d*")
    out.pass2to1(this,b)
  }
  
  private[ChiselDSP] override def / (b: DSPDbl): DSPDbl = {newBinaryOp(b, "d/")}
  
  /** Comparison operators --------------------- */
  
  override def === (b: DSPDbl): DSPBool = {
    val out = DSPBool(newLogicalOp(b, "d=="))
    out.pass2to1(this,b)
  }
  override def =/= (b: DSPDbl): DSPBool = { 
    val out = DSPBool(newLogicalOp(b, "d!="))
    out.pass2to1(this,b)
  }
  def > (b: DSPDbl): DSPBool = {
    val out = DSPBool(newLogicalOp(b, "d>"))
    out.pass2to1(this,b)
  }
  def < (b: DSPDbl): DSPBool = {
    val out = DSPBool(newLogicalOp(b, "d<"))
    out.pass2to1(this,b)
  }
  def <= (b: DSPDbl): DSPBool = {
    val out = DSPBool(newLogicalOp(b, "d<="))
    out.pass2to1(this,b)
  }
  def >= (b: DSPDbl): DSPBool = {
    val out = DSPBool(newLogicalOp(b, "d>="))
    out.pass2to1(this,b)
  }

  /** ARITHMETIC Right shift n --> this/2^n */
  override def >> (n: Int) : DSPDbl = {
    val out = this/DSPDbl(math.pow(2,n))
    out.updateGeneric(this)
  }
    
  /** ARITHMETIC Left shift n --> this*2^n */
  def << (n: Int) : DSPDbl = {
    val out = this*DSPDbl(math.pow(2,n))
    out.updateGeneric(this)
  }
  
  /** ARITHMETIC Right shift variable n --> this/2^n */
  def >> (n: DSPUInt) : DSPDbl = {
    if (n.getRange.max > 64) error("Can't divide by more than 2^64")
    val shiftLUT = Vec((0 until 65).map( x => DSPDbl(math.pow(2,x))))
    val out = this/shiftLUT(n.toUInt)
    out.pass2to1(this,n)
  }
  
  /** ARITHMETIC Left shift variable n --> this*2^n */
  def << (n: DSPUInt) : DSPDbl = {
    if (n.getRange.max > 64) error("Can't multiply by more than 2^64")
    val shiftLUT = Vec((0 until 65).map( x => DSPDbl(math.pow(2,x))))
    val out = this*shiftLUT(n.toUInt)
    out.pass2to1(this,n)
  }

  /** select ? this (true) : 0 (false) -- used for Mux */
  def ? (select: DSPBool) : DSPDbl = {
    val out = toT(this & Fill(64,select.toBool))
    out.pass2to1(this,select)
  }
  
  /** Bitwise-OR. Use case: Mux, etc. with the expectation that one input is 0 */ 
  def /| (b: DSPDbl) : DSPDbl = {
    val out = toT(this.toBits | b.toBits)
    out.pass2to1(this,b)
  }
  
  /** Dbl doesn't care about limits */
  protected def updateLimits(range: (BigInt,BigInt)): Unit = {
    setRangeBits(range)
    setRange(range)
  }
  updateLimits((BigInt(0),BigInt(0))) 
  
}
