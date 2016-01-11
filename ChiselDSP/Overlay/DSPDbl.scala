/** Double class customizations */

// TODO: For Fixed, Dbl, if both inputs to +,-,*, etc. are lits, solve as lits so you don't have delay mismatch errors

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

  /** Clone this instantiation */
  override def cloneType: this.type = {
    val out = DSPDbl(dir)
    out.copyInfo(this).asInstanceOf[this.type]
  }

  /** Convert Bits to a DSPDbl by reinterpreting the Bits */
  private def toT(x: Bits) : DSPDbl = {
    val res = chiselCast(x){DSPDbl(x.dir)}
    res.assign()
  }

  /** For converting inputs to Dbl */
  protected def dbl() : Dbl = chiselCast(this){Dbl(this.dir)}

  /** Print DSPDbl info */
  override def infoString() : String = "double"
  def Q(): String = infoString

  type T = DSPDbl

  /** Convert an Integer to DSPDbl representation */
  override def fromInt(x: Int): this.type = DSPDbl(x.toDouble).asInstanceOf[this.type]
  
  override def fromNode(n: Node) = DSPDbl(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
  
  /** Reassign with ":=". Certain conditions must be enforced so delay is consistent */
  override protected def colonEquals(that : Bits): Unit = that match {
    case d: DSPDbl => super.colonEquals(assign(d))
    case _ => illegalAssignment(that)
  }

  /** Used for bulk assigning + := */
  private[ChiselDSP] def assign(d: DSPDbl): DSPDbl = {reassign(d);d}
  
  /** Ops using Dbl backend --------------------- (requires conversion to Chisel.Dbl first) */
  
  def + (b: DSPDbl): DSPDbl = {
    if (isLit && litValue() == 0) b
    else if (b.isLit && b.litValue() == 0) this
    else {
      val out = toT(dbl() + b.dbl())
      out.pass2to1(this,b)
    }
  }
  def - (b: DSPDbl): DSPDbl = {
    if (isLit && b.isLit) {
      val aLit = java.lang.Double.longBitsToDouble(litValue().longValue)
      val bLit = java.lang.Double.longBitsToDouble(b.litValue().longValue)
      DSPDbl(aLit-bLit)
    }
    else if (b.isLit && b.litValue() == 0) this
    else {
      val out = toT(dbl() - b.dbl())
      out.pass2to1(this, b)
    }
  }
  override def unary_-(): DSPDbl = {DSPDbl(0.0)-this}

  // TODO: multiply by literal -1
  def * (b: DSPDbl): DSPDbl = {
    val bits1 = BigInt(java.lang.Double.doubleToLongBits(1.0))
    if (isLit && litValue() == 0) DSPDbl(0.0)
    else if (isLit && litValue() == bits1) b
    else if (b.isLit && b.litValue() == 0) DSPDbl(0.0)
    else if (b.isLit && b.litValue() == bits1) this
    else {
      val out = toT(dbl() * b.dbl())
      out.pass2to1(this,b)
    }
  }
  private[ChiselDSP] override def / (b: DSPDbl): DSPDbl = toT(dbl() / b.dbl())

  /** Comparison operators --------------------- */
  
  override def === (b: DSPDbl): DSPBool = {
    val out = DSPBool(dbl() === b.dbl())
    out.pass2to1(this,b)
  }
  override def =/= (b: DSPDbl): DSPBool = { 
    val out = DSPBool(dbl() =/= b.dbl())
    out.pass2to1(this,b)
  }
  def > (b: DSPDbl): DSPBool = {
    val out = DSPBool(dbl() > b.dbl())
    out.pass2to1(this,b)
  }
  def < (b: DSPDbl): DSPBool = {
    val out = DSPBool(dbl() < b.dbl())
    out.pass2to1(this,b)
  }
  def <= (b: DSPDbl): DSPBool = {
    val out = DSPBool(dbl() <= b.dbl())
    out.pass2to1(this,b)
  }
  def >= (b: DSPDbl): DSPBool = {
    val out = DSPBool(dbl() >= b.dbl())
    out.pass2to1(this,b)
  }

  /** ARITHMETIC Right shift n --> this/2^n */
  override def >> (n: Int) : DSPDbl = {
    if (n < 0) error("Shift amount must be non-negative")
    if (n == 0) this
    else if (isLit) {
      val x = java.lang.Double.longBitsToDouble(litValue().longValue)/math.pow(2,n)
      DSPDbl(x)
    }
    else{
      val out = this/DSPDbl(math.pow(2,n))
      out.updateGeneric(this)
    }
  }
    
  /** ARITHMETIC Left shift n --> this*2^n */
  def << (n: Int) : DSPDbl = {
    if (n < 0) error("Shift amount must be non-negative")
    if (n == 0) this
    else if (isLit) {
      val x = java.lang.Double.longBitsToDouble(litValue().longValue)*math.pow(2,n)
      DSPDbl(x)
    }
    else {
      val out = this * DSPDbl(math.pow(2, n))
      out.updateGeneric(this)
    }
  }
  
  /** ARITHMETIC Right shift variable n --> this/2^n */
  def >> (n: DSPUInt) : DSPDbl = {
    if (n.getRange.max > 64) error("Can't divide by more than 2^64")
    if (n.isLit) this >> n.litValue().intValue
    else {
      val shiftLUT = Vec((0 until 65).map( x => DSPDbl(math.pow(2,x))))
      val out = this/shiftLUT(n.toUInt)
      out.pass2to1(this,n)
    }
  }
  
  /** ARITHMETIC Left shift variable n --> this*2^n */
  def << (n: DSPUInt) : DSPDbl = {
    if (n.getRange.max > 64) error("Can't multiply by more than 2^64")
    if (n.isLit) this << n.litValue().intValue
    else {
      val shiftLUT = Vec((0 until 65).map(x => DSPDbl(math.pow(2, x))))
      val out = this * shiftLUT(n.toUInt)
      out.pass2to1(this, n)
    }
  }

  /** select ? this (true) : 0 (false) -- used for Mux */
  def ? (select: DSPBool) : DSPDbl = {
    if (select.isLit) {if (select.isTrue) this else DSPDbl(0.0)}
    else {
      val out = toT(this.toBits & Fill(64, select.toBool))
      out.pass2to1(this, select)
    }
  }
  
  /** Bitwise-OR. Use case: Mux, etc. with the expectation that one input is 0 */ 
  def /| (b: DSPDbl) : DSPDbl = {
    if (isLit && litValue() == 0) b
    else if (b.isLit && b.litValue() == 0) this
    else {
      val out = toT(this.toBits | b.toBits)
      out.pass2to1(this, b)
    }
  }

  // TODO: Add lit support below
  /** Trim functions */
  private def floor: DSPDbl = toT(dbl().floor)
  private def ceil: DSPDbl = toT(dbl().ceil)
  private def round: DSPDbl = toT(dbl().round)

  /** Gets integer portion as DSPFixed. Optional rounding mode. */
  def toInt(r: TrimType): DSPFixed = {
    val res = {
      if (r == Truncate) floor
      else if (r == Round) round
      else error("Invalid trim type for toInt")
    }
    val sintVal = res.dbl().toSInt()
    val out = DSPSInt(sintVal,DSPFixed.toRange(64))
    out.updateGeneric(this)
  }
  
  /** Dbl doesn't care about limits */
  protected def updateLimits(range: (BigInt,BigInt)): Unit = {
    setRangeBits(range)
    setRange(range)
  }
  updateLimits((BigInt(0),BigInt(0))) 
  
}
