/** DSPUInt class customizations --
  * Uses range instead of bitwidth (bitwidth inferred via max).
  * Keeps track of operation ranges to achieve best bitwidth inference.
  * Expands # of useful operators.
  */
  
package ChiselDSP
import Chisel._

object DSPUInt {

  /** Convert Bits to a DSPUInt (upperbound specified w/ max) by reinterpreting the Bits */
  def apply(x: Bits, max: BigInt): DSPUInt = apply(x,(BigInt(0),max))
  def apply(x:Bits, range:(BigInt,BigInt)): DSPUInt = {
    val res = chiselCast(x){apply(x.dir,range)}
    res.assign()
  }

  /** Determine bitwidth needed for value x */
  def toBitWidth(x: BigInt): Int = {
    if (x == 0) 1
    else x.bitLength
  }
	
  /** Calculate signal max from bitwidth */
  def toMax(w: Int): BigInt = (BigInt(2) << w) - 1 
    
  /** Creates a DSPUInt object from a constant BigInt (or Int casted to BigInt) */
  def apply(x: BigInt): DSPUInt = {
    val range = (x,x)
    val res = Lit(x, toBitWidth(x)){apply(NODIR, range)}
    res.asDirectionless
    res.updateLimits(range)
    res.assign()
  }
  
  /** Create a DSPUInt obect with a specified IODirection and range */
  def apply(dir: IODirection, max: BigInt): DSPUInt = apply(dir,(BigInt(0),max))
  def apply(dir: IODirection, range: (BigInt,BigInt)): DSPUInt = {
    if (range == null) Error("Range cannot be null.")
    if (range._1 < 0 || range._2 < 0) Error("DSPUInt must be non-negative!")
    apply_gen(dir,range)
  }
  
  /** Internally handles some edge cases i.e. when Chisel relies on node conversion,
    * width/range is not defined up front 
    */
  private def apply_gen(dir: IODirection, range: (BigInt,BigInt)): DSPUInt = {
    val res = new DSPUInt()
    val updateBits = (range != null)
    res.create(dir, if (updateBits) toBitWidth(range._2) else -1)
    if (updateBits) res.updateLimits(range)
    res
  }

}

class DSPUInt extends DSPNum[DSPUInt] {

  /** DSPUInt info */
  override def infoString() : String = (getWidth + "-bit uint, range = " + rangeString())

  type T = DSPUInt
  
  /** Sign of UInt always 0 */
  override def sign(): DSPBool = DSPBool(false)

  /** Set the value of this DSPUInt */
  override def fromInt(x: Int): this.type = DSPUInt(x).asInstanceOf[this.type]

  override def fromNode(n: Node): this.type = {
    val res = DSPUInt.apply_gen(OUTPUT,null).asTypeFor(n).asInstanceOf[this.type]
    n match {
      case l: Literal =>
        if (l.isZ && Driver.minimumCompatibility > "2") {
          // Chisel3 compatibility - generic don't care UInts/Bits are deprecated.
          ChiselError.warning("General don't care DSPUInts are deprecated. Please use BitPat().")
        }
      case _ =>
    }
    res
  }
  
  /** Convert Bits to a DSPUInt by reinterpreting the Bits. 
    * opRange specifies the range of the output as determined by the operation.
    * rangeOverride allows the designer to use a smaller range (shorten bits appropriately).
    */
  private def toT(x: Bits, opRange: (BigInt,BigInt), rangeOverride: (BigInt,BigInt) = null) : DSPUInt = {
    if (rangeOverride == null) DSPUInt(x,opRange)
    else {
      val min = rangeOverride._1.max(opRange._1)
      val max = rangeOverride._2.min(opRange._2) 
      DSPUInt(x,(min,max))
    }
  }
  	
  /** Update the range of this DSPUInt instance. Range can be expanded until it exceeds
    * what is allowable by the DSPUInt bitwidth, which is fixed @ creation.
    */
  protected def updateLimits(range: (BigInt,BigInt)): Unit = {
    setRangeBits((BigInt(0),DSPUInt.toMax(getWidth)))
    setRange(range)
  }
  
  /** Reassign with ":=". Certain conditions must be enforced so delay + range are consistent */
  override protected def colonEquals(that : Bits): Unit = {
    that match {
      case u: DSPUInt => {
        reassign(u)
        val (x,y) = matchWidth(u)
        super.colonEquals(toT(y,List2Tuple(u.getRange))) 
      }
      case _ => illegalAssignment(that)
    }
  }
  
  /** Match operator widths */
  private def matchWidth(y: DSPUInt) : Tuple2[Bits,Bits] = {
    val diff = y.getWidth - getWidth
    if (diff > 0) (Cat(Fill(diff,sign.toBits),this), y.toBits) 
    else if (diff < 0) (this.toBits, Cat(Fill(-diff,y.sign.toBits),y)) 
    else (this.toBits, y.toBits)
  }
  
  /** select ? this (true) : 0 (false) -- used for Mux */
  def ? (select: DSPBool): DSPUInt = {
    val out = {
      if (select.isLit) {if (select.isTrue) this else DSPUInt(0)}
      else{ 
        val res = this & Fill(getWidth,select.toBits)
        toT(res,List2Tuple(getRange))
      }
    }
    out.pass2to1(this,select)
  }
    
  /** Right shift n --> this/2^n */
  def >> (n: Int) : DSPUInt = {
    if (n < 0) error("Shift amount must be non-negative")
    val out = {
      if (isLit) DSPUInt(litValue() >> n)
      else if (n >= getWidth) DSPUInt(0)
      else if (n == 0) this
      else {
        val res = this(getWidth-1,n)
        val newRange = getRange map { _ >> n }
        toT(res,List2Tuple(newRange))
      }
    }
    out.updateGeneric(this)
  }
  
  /** Left shift n --> this*2^n */
  def << (n: Int) : DSPUInt = {
    if (n < 0) error("Shift amount must be non-negative")
    val out = {
      if (isLit) DSPUInt(litValue() << n)
      else if (n == 0) this
      else {
        val res = Cat(this,Fill(n,Bits(0,1)))
        val newRange = getRange map { _ << n }
        toT(res,List2Tuple(newRange))
      }
    }
    out.updateGeneric(this)
  }
  
  /** Variable right shift this/2^n */
  def >> (n: DSPUInt) : DSPUInt = {
    val out = {
      if (n.isLit) this >> n.litValue().intValue
      else {
        val res = this.toUInt >> n.toUInt
        val newRange = (getRange, n.getRange.reverse).zipped.map( _ >> _.intValue )
        toT(res,List2Tuple(newRange))
      }
    }
    out.pass2to1(this,n)
  }
  
  /** Variable left shift this*2^n */
  def << (n: DSPUInt) : DSPUInt = {
    val out = {
      if (n.isLit) this << n.litValue().intValue
      else{
        val res = this.toUInt << n.toUInt
        val newRange = (getRange, n.getRange).zipped.map( _ << _.intValue )
        toT(res,List2Tuple(newRange))
      }
    }
    out.pass2to1(this,n)
  }
  
  /** Greater than */
  def > (b : DSPUInt) : DSPBool = {
    val (x,y) = matchWidth(b)
    val out = DSPBool(x.toUInt > y.toUInt)
    out.pass2to1(this,b)
  }

  /** Less than */
  def < (b : DSPUInt) : DSPBool = {
    val (x,y) = matchWidth(b)
    val out = DSPBool(x.toUInt < y.toUInt)
    out.pass2to1(this,b)
  }

  /** Greater than or equal to */
  def >= (b : DSPUInt) : DSPBool = {
    val (x,y) = matchWidth(b)
    val out = DSPBool(x.toUInt >= y.toUInt)
    out.pass2to1(this,b)
  }

  /** Less than or equal to */
  def <= (b : DSPUInt) : DSPBool = {
    val (x,y) = matchWidth(b)
    val out = DSPBool(x.toUInt <= y.toUInt)
    out.pass2to1(this,b)
  }
  
  /** Bitwise-OR with custom range inference */
  def /| (b: DSPUInt) : DSPUInt = {
    val out = {
      if (isLit && litValue() == 0) b
      else if (b.isLit && b.litValue() == 0) this
      else{ 
        val (x,y) = matchWidth(b)
        val opMax =  getRange.max.max(b.getRange.max)   
        val opMin =  getRange.min.min(b.getRange.min)
        toT(x | y,(opMin,opMax))
      }
    }
    out.pass2to1(this,b)
  }

  /** Add overflow -> wrap to max(this.width,b.width) # of bits */
  override def +% (b: DSPUInt) : DSPUInt = {
    val out = {
      if (isLit && litValue() == 0) b
      else if (b.isLit && b.litValue() == 0) this
      else{
        val (x,y) = matchWidth(b)        
        val newRange = (getRange, b.getRange).zipped.map( _ + _ )
        val inMaxBits = DSPUInt.toMax(x.getWidth)
        val opMax = inMaxBits.min(newRange.max)
        val opMin = if (opMax != newRange.max) BigInt(0) else newRange.min
        toT(x+y,(opMin,opMax))
      }
    }
    out.pass2to1(this,b)
  }
  
  /** Explicit add with bit growth 
    * Range [0,max] where max is determined by sum bitwidth 
    * => max(this.width,b.width) + 1
    */
  override def +& (b: DSPUInt) : DSPUInt = {
    val (x,y) = matchWidth(b)
    val sum = Cat(sign.toBits,x)+Cat(sign.toBits,y)
    val out = toT(sum,(BigInt(0),DSPUInt.toMax(x.getWidth+1)))
    out.pass2to1(this,b)
  }
  
  /** Add that determines optimal sum range, bitwidth */
  def + (b: DSPUInt) : DSPUInt = {
    val out = {
      if (isLit && litValue() == 0) b
      else if (b.isLit && b.litValue() == 0) this
      else{
        val newRange = (getRange, b.getRange).zipped.map( _ + _ )
        toT((this +& b),List2Tuple(newRange))
      }
    }
    out.pass2to1(this,b)
  }
  
  /** Subtract and wrap on negative result */
  def - (b: DSPUInt) : DSPUInt = {
    val out = {
      if (b.isLit && b.litValue() == 0) this
      else{
        val (x,y) = matchWidth(b)
        val diff = x.toUInt-y.toUInt
        val newRange = (getRange, b.getRange.reverse).zipped.map( _ + _ )
        if (newRange.min < 0) Warn("Possible DSPUInt subtraction negative result will wrap.")
        val opMin = newRange.min.max(BigInt(0))
        val opMax = if (opMin != newRange.min) DSPUInt.toMax(x.getWidth) else newRange.max
        toT(diff,(opMin,opMax))
      }
    }
    out.pass2to1(this,b)
  }
  
  /** Multiply while determining optimal product bitwidth + range */
  def * (b: DSPUInt) : DSPUInt = {
    val out = {
      if (isLit && litValue() == 0) DSPUInt(0)
      else if (isLit && litValue() == 1) b
      else if (b.isLit && b.litValue() == 0) DSPUInt(0)
      else if (b.isLit && b.litValue() == 1) this
      else {
        val res = this.toBits * b.toBits
        val newRange = (getRange, b.getRange).zipped.map( _ * _ )
        toT(res,List2Tuple(newRange))
      }
    }
    out.pass2to1(this,b)
  }
  
  /** Bit extraction @ index 'bit' */
  def extract(bit:Int): DSPBool = {
    val out = DSPBool(Extract(this,bit){Bool()})
    out.updateGeneric(this)

  }
  def extract(bit:DSPUInt): DSPBool = {
    val out = {
      if (bit.isLit) extract(bit.litValue().intValue) 
      else DSPBool(Extract(this,bit.toUInt){Bool()})
    }
    out.pass2to1(this,bit)
  }
  
  /** Extract range of bits, inclusive from hi to lo. Optional range override (won't report invalid).*/
  def extract(hi:Int, lo:Int): DSPUInt = extract(hi,lo,rangeOverride = null)
  def extract(hi:Int, lo:Int, maxOverride: BigInt):DSPUInt = extract(hi,lo,(BigInt(0),maxOverride))
  def extract(hi:Int, lo:Int, rangeOverride:(BigInt,BigInt)): DSPUInt = {
    val res = Extract(this,hi,lo){UInt()}
    val bitMax = DSPUInt.toMax(res.getWidth)
    val out = toT(res,(BigInt(0),bitMax),rangeOverride)
    out.updateGeneric(this)
  }
  def extract(hi:DSPUInt, lo:DSPUInt): DSPUInt = extract(hi,lo, rangeOverride = null)
  def extract(hi:DSPUInt, lo:DSPUInt, maxOverride:BigInt): DSPUInt = extract(hi,lo,(BigInt(0),maxOverride))
  def extract(hi:DSPUInt, lo:DSPUInt, rangeOverride:(BigInt,BigInt)): DSPUInt = { 
    val out = {
      if (hi.isLit && lo.isLit) extract(hi.litValue().intValue,lo.litValue().intValue,rangeOverride)
      else {
        val res = Extract(this,hi.toUInt,lo.toUInt){UInt()}
        val bitMax = DSPUInt.toMax(res.getWidth)
        toT(res,(BigInt(0),bitMax),rangeOverride)
      }
    }
    out.pass3to1(this,hi,lo)
  }
  
  /** Shorten # of bits -- get rid of MSBs by forcing the generator to interpret a new (smaller, positive) max
    * CAREFUL: could eliminate useful bits!!! 
    */
  def shorten(newMax: BigInt): DSPUInt = {
    val newMin = getRange.min.min(newMax)
    val out = toT(this,List2Tuple(getRange),(newMin,newMax)) 
    out.updateGeneric(this) 
  }
  
}
