/** Fixed class customizations */

package ChiselDSP
import Chisel._

/** Different overflow handling methods */
abstract class OverflowType
case object Saturate extends OverflowType
case object Wrap extends OverflowType
case object Grow extends OverflowType

/** Different trim methods */
abstract class TrimType
case object Truncate extends TrimType
case object Round extends TrimType
case object NoTrim extends TrimType

object DSPFixed {

  /** Convert SInt to a DSPFixed by reinterpreting the Bits */
  def apply(s: SInt, fracWidth:Int, range: (Double,Double)): DSPFixed = {
    val res = chiselCast(s) {DSPFixed(s.dir, fracWidth, range)}
    res.assign()
  }
  private [ChiselDSP] def apply(s: SInt, fracWidth:Int, range: => (BigInt, BigInt)): DSPFixed = {
    val res = chiselCast(s) {DSPFixed(s.dir, fracWidth, range)}
    res.assign()
  }
  private [ChiselDSP] def apply(s: SInt, fixedParams: (Int, Int), range: => (BigInt, BigInt)): DSPFixed = {
    val res = chiselCast(s) {apply_gen(s.dir,fixedParams,range,true)}
    res.assign()
  }

  def apply(s: SInt, fixedParams: (Int, Int)): DSPFixed = {
    apply(s,fixedParams,toRange(paramsToWidth(fixedParams)))
  }

  /** Creates a DSPFixed object from a constant Double. 
    * fixedParams = (intWidth,fracWidth), or alternatively, fracWidth, must be specified!
    * Qn.m --> n = intWidth, m = fracWidth, width = n + m + 1 (sign).
    * intWidth is automatically determined for a given Lit + fracWidth if intWidth isn't specified.
    */
  def apply(x: Double): DSPFixed = apply(x,Complex.getFrac)
  def apply(x: Double, fixedParams: (Int, Int)): DSPFixed = apply(toFixed(x, fixedParams._2), fixedParams)
  def apply(x: Double, fracWidth: Int): DSPFixed = apply(toFixed(x, fracWidth),fracWidth)

  /** Creates DSPFixed Literal after Double has been converted into a BigInt */
  private[ChiselDSP] def apply (x: BigInt, fracWidth:Int): DSPFixed = {
    val litVal = apply(NODIR, fracWidth, (x,x))
    createLit(litVal,x,fracWidth)
  }
  private[ChiselDSP] def apply(x: BigInt, fixedParams: (Int, Int)): DSPFixed = {
    val supportedRange = toRange(paramsToWidth(fixedParams))
    if ( x < supportedRange._1 || x > supportedRange._2)
      Error("Constant can't be represented with integer width specified")
    val litVal = apply(NODIR, fixedParams)
    createLit(litVal,x,fixedParams._2)
  }

  private def createLit(litVal: DSPFixed, x: BigInt, fracWidth: Int): DSPFixed = {
    val res = Lit(x, litVal.getWidth) {litVal}
    res.fractionalWidth = fracWidth
    res.asDirectionless
    res.updateLimits((x,x))
    res.assign()
  }

  /** Creates a DSPFixed object with a specified IODirection, (intWidth,fracWidth) */
  def apply(dir: IODirection): DSPFixed = apply(dir,Complex.getFixedParams)
  def apply(): DSPFixed = apply(Complex.getFixedParams)
  def apply(fixedParams: (Int,Int)): DSPFixed = apply(NODIR,fixedParams)
  def apply(dir: IODirection, fixedParams: (Int, Int)): DSPFixed = {
    apply(dir, fixedParams, toRange(paramsToWidth(fixedParams)))
  }
  /** Creates a DSPFixed object with a specified IODirection, (intWidth,fracWidth), 
    * and [optional] Double (min,max) range.
    */
  def apply(range : => (Double,Double)): DSPFixed = apply(Complex.getFixedParams,range)
  def apply(dir: IODirection, range : => (Double,Double)): DSPFixed = apply(dir,Complex.getFixedParams,range)
  def apply(fixedParams:(Int,Int), range: (Double,Double)) : DSPFixed = apply(NODIR,fixedParams,range)
  def apply(dir: IODirection, fixedParams:(Int,Int), range: (Double,Double)) : DSPFixed = {
    val min = toFixed(range._1, fixedParams._2)
    val max = toFixed(range._2, fixedParams._2)
    apply(dir,fixedParams,(min,max))
  }
  def apply(fracWidth:Int, range: (Double,Double)) : DSPFixed = apply(NODIR,fracWidth,range)
  def apply(dir: IODirection, fracWidth:Int, range: (Double,Double)) : DSPFixed = {
    val min = toFixed(range._1, fracWidth)
    val max = toFixed(range._2, fracWidth)
    apply(dir,fracWidth,(min,max))
  }

  private[ChiselDSP] def apply(dir: IODirection, fracWidth: Int, range: => (BigInt,BigInt)): DSPFixed = {
    val intWidth = rangeToWidth(range)-1-fracWidth
    apply(dir, (intWidth.max(0) , fracWidth), range)
  }
  /** Intermediate apply function to check if range is null */
  private def apply(dir: IODirection, fixedParams:(Int,Int), range: => (BigInt,BigInt)): DSPFixed = {
    if (range == null) Error("Range cannot be null.")
    apply_gen(dir,fixedParams,range)
  }
  
  /** Internally handles some edge cases i.e. when Chisel relies on node conversion,
    * width/range is not defined up front. Width determined by worst case of
    * fixedParams and range.
    */
  private def apply_gen(dir : IODirection, fixedParams: (Int, Int), range: (BigInt,BigInt),
                        fixedWidth: Boolean = false) : DSPFixed = {
    val updateBits = (range != null)
    val width = paramsToWidth(fixedParams)
    val rangeWidth = if (updateBits) rangeToWidth(range).max(fixedParams._2 + 1) else width
    if (updateBits){ 
      if (fixedParams._1 < 0) Error("Fixed integer width must be non-negative.")
      if (fixedParams._2 < 0) Error("Fixed fractional width must be non-negative.")
      if (rangeWidth > width) Error("Rounded + normalized to LSB range [" + range._1 + "," + range._2 +
        "] of Fixed value greater than range allowed by fixed parameters ["
        + fixedParams._1 + "," + fixedParams._2 + "]")
    }
    val res = new DSPFixed(fixedParams._2)
    res.create(dir, if (updateBits) (if (fixedWidth) width else rangeWidth) else -1)
    if (updateBits) res.updateLimits(range)
    res
  }
 
  // --------- Helper functions
  
  /** Convert BigInt to Double, knowing fractional width */
  def toDouble(x: BigInt, fracWidth: Int) : Double = x.doubleValue/math.pow(2,fracWidth)
  /** Convert Double to BigInt, knowing fractional width */
  def toFixed(x: Double, fracWidth: Int) : BigInt = BigInt(math.round(x*math.pow(2,fracWidth)))

  /** From fixed width, determine (min, max) as BigInts.
    * Renormalized so LSB --> |1| regardless of fractional width.
    * In general, Qm.n has range [-(2^m), 2^m - 2^(-n)] and resolution 2^(-n)
    */
  def toRange(w: Int) : Tuple2[BigInt,BigInt] = {
    // Don't include sign bit in range calculations.
    val dataBits = w-1
    val absRange = (BigInt(1) << dataBits)
    val max = absRange -1
    val min = -1 * absRange
    (min,max)
  }
  
  /** Returns width from fixedParams (accounting for sign bit) */
  def paramsToWidth(fixedParams:(Int,Int)): Int = fixedParams._1 + fixedParams._2 + 1

  /** Returns width from range */
  def rangeToWidth(range:(BigInt,BigInt)): Int = range._1.bitLength.max(range._2.bitLength) + 1

}

class DSPFixed (private var fractionalWidth:Int = 0)  extends DSPQnm[DSPFixed] {

  /** Clone this instantiation */
  override def cloneType: this.type = {
    val out = DSPFixed(dir,(getIntWidth,getFracWidth))
    // TODO: Maybe should just copy delay? (see below cloneType)
    out.copyInfo(this).asInstanceOf[this.type]
  }
  override def cloneType(fixedParams:(Int,Int)): this.type = {
    val out = DSPFixed(dir,fixedParams)
    out.passDelay(this).asInstanceOf[this.type]
  }

  /** Convert SInt to a DSPFixed by reinterpreting the Bits */
  private def fromSInt(s: SInt, fracWidth:Int, opRange: (BigInt, BigInt)): DSPFixed = DSPFixed(s,fracWidth,opRange)
  private def fromSInt(s: SInt, fixedParams:(Int,Int), opRange: (BigInt, BigInt)): DSPFixed = {
    DSPFixed(s,fixedParams,opRange)
  }

  /** Print DSPFixed info */
  override def infoString() : String = Q + ", range = " + rangeString(getFracWidth)
  def Q(): String = "+/-Q"+getIntWidth+"."+getFracWidth

  /** Get widths */
  override def getIntWidth(): Int = getWidth-getFracWidth-1
  override def getFracWidth(): Int = fractionalWidth

  type T = DSPFixed

  /** Create a DSPFixed representation from an Int */
  override def fromInt(x : Int) : this.type = DSPFixed(x.toDouble, (getIntWidth,getFracWidth)).asInstanceOf[this.type]

  override def fromNode(n : Node): this.type = {
    // fixedParams mean nothing
    DSPFixed.apply_gen(OUTPUT,(n.getWidth,0),null).asTypeFor(n).asInstanceOf[this.type]
  }

  /** Update the range of this DSPFixed instance. Range can be expanded until it exceeds
    * what is allowable by the DSPFixed bitwidth, which is fixed @ creation.
    */
  private[ChiselDSP] def updateLimits(range: (BigInt,BigInt)): Unit = {
    setRangeBits(DSPFixed.toRange(getWidth))
    setRange(range)
  }

  /** Adding (+n) /subtracting (-n) fractional bits --> adjust range */
  private def adaptFracRange(n: Int): List[BigInt] = {
    if (n >= 0) getRange map { _ << n }
    else getRange map { _ >> n}                               // Signed
  }

  /** Match fractional widths of fixed point values and return (potentially) right-padded SInts
    * (SInt a, SInt b, a New Range, b new Range, fracWidth)
    * */
  private def matchFracWidth (b: DSPFixed) : Tuple5[SInt,SInt,List[BigInt],List[BigInt],Int] = {
    val fracWidth = getFracWidth.max(b.getFracWidth)
    val fracDiff = getFracWidth - b.getFracWidth
    val zro = Bits(0,1)
    if (fracDiff > 0)
      (this.toSInt,Cat(b,Fill(fracDiff,zro)).toSInt,getRange,b.adaptFracRange(fracDiff),fracWidth)
    else if (fracDiff < 0)
      (Cat(this,Fill(-fracDiff,zro)).toSInt,b.toSInt,adaptFracRange(-fracDiff),b.getRange,fracWidth)
    else (this.toSInt,b.toSInt,getRange,b.getRange,fracWidth)
  }

  /** Sign extend this/b width if shorter than b/this width: Outputs width matched operands, updated ranges,
    * and fracWidth used.
    */
  private def matchWidth(that: DSPFixed) : Tuple5[SInt,SInt,List[BigInt],List[BigInt],Tuple2[Int,Int]] = {
    val (x,y,xrange,yrange,fracWidth) = matchFracWidth(that)
    val widthDiff = y.getWidth - x.getWidth
    val (a,b) = {
      if (widthDiff > 0) (Cat(Fill(widthDiff,sign.toBits),x).toSInt, y)
      else if (widthDiff < 0) (x, Cat(Fill(-widthDiff,that.sign.toBits),y).toSInt)
      else (x, y)
    }
    val intWidth = a.getWidth - 1 - fracWidth
    (a,b,xrange,yrange,(intWidth,fracWidth))
  }

  /** Check fractional width alignment */
  private def checkAlign(f: DSPFixed): Unit = {
    if (getFracWidth != f.getFracWidth) Error("Fractional widths should match. L = " + getFracWidth +
                                              ", R = " + f.getFracWidth)
  }

  /** Reassign with ":=". Certain conditions must be enforced so delay + range are consistent.
    * Note that fracWidths of L,R in L := R should match.
    */
  override protected def colonEquals(that : Bits): Unit = {
    that match {
      case f: DSPFixed => super.colonEquals(assign(f))
      case _ => illegalAssignment(that)
    }
  }

  /** Used for bulk assigning + := */
  private[ChiselDSP] def assign(f: DSPFixed): DSPFixed = {
    checkAlign(f)
    reassign(f)
    if (getWidth == f.getWidth) f
    else {
      val (x, y, xrange, yrange, fixedParams) = matchWidth(f)
      fromSInt(y, fixedParams, List2Tuple(yrange))
    }
    // TODO: Check that assigning between modules when widths don't match doesn't cause problems (UInt too)
  }

  /** Shorten # of integer bits -- get rid of MSBs by forcing the generator to use a smaller width
    * CAREFUL: could eliminate useful bits!!!
    */
  override def shortenTo(intWidth: Int = Complex.getInt): DSPFixed = {
    val newWidth = DSPFixed.paramsToWidth((intWidth,getFracWidth))
    if (newWidth > getWidth) Error("New width must be <= old Fixed width.")
    val out = fromSInt(this.toSInt,getFracWidth,DSPFixed.toRange(newWidth))
    out.updateGeneric(this)
  }

  // Fixed operations

  /** Equality check */
  override def === (b: T): DSPBool = {
    val (x,y,n1,n2,n3) = matchWidth(b)
    val out = DSPBool(x === y)
    out.pass2to1(this,b)
  }

  /** Inequality check */
  override def =/= (b: T): DSPBool = {
    val (x,y,n1,n2,n3) = matchWidth(b)
    val out = DSPBool(x =/= y)
    out.pass2to1(this,b)
  }

  /** Greater than */
  def > (b : DSPFixed) : DSPBool = {
    val (x,y,n1,n2,n3) = matchWidth(b)
    val out = DSPBool(x.toSInt > y.toSInt)
    out.pass2to1(this,b)
  }

  /** Less than */
  def < (b : DSPFixed) : DSPBool = {
    val (x,y,n1,n2,n3) = matchWidth(b)
    val out = DSPBool(x.toSInt < y.toSInt)
    out.pass2to1(this,b)
  }

  /** Greater than or equal to */
  def >= (b : DSPFixed) : DSPBool = {
    val (x,y,n1,n2,n3) = matchWidth(b)
    val out = DSPBool(x.toSInt >= y.toSInt)
    out.pass2to1(this,b)
  }

  /** Less than or equal to */
  def <= (b : DSPFixed) : DSPBool = {
    val (x,y,n1,n2,n3) = matchWidth(b)
    val out = DSPBool(x.toSInt <= y.toSInt)
    out.pass2to1(this,b)
  }

  /** Right shift n --> this/2^n */
  def >> (n: Int) : DSPFixed = {
    if (n < 0) error("Shift amount must be non-negative")
    val out = {
      if (isLit) DSPFixed(signed_fix() >> n,getFracWidth)
      else if (n >= getWidth) DSPFixed(0,getFracWidth)
      else if (n == 0) this
      else {
        val res = this(getWidth-1,n)
        val newRange = getRange map { _ >> n }
        fromSInt(res.toSInt,getFracWidth,List2Tuple(newRange))
      }
    }
    out.updateGeneric(this)
  }

  /** Left shift n --> this*2^n */
  def << (n: Int) : DSPFixed = {
    if (n < 0) error("Shift amount must be non-negative")
    val out = {
      if (isLit) DSPFixed(signed_fix() << n,getFracWidth)
      else if (n == 0) this
      else {
        val res = Cat(this,Fill(n,Bits(0,1)))
        val newRange = getRange map { _ << n }
        fromSInt(res.toSInt,getFracWidth,List2Tuple(newRange))
      }
    }
    out.updateGeneric(this)
  }

  /** Variable right shift this/2^n */
  def >> (n: DSPUInt) : DSPFixed = {
    val out = {
      if (n.isLit) this >> n.litValue().intValue
      else {
        val res = this.toSInt >> n.toUInt
        val newRange = (getRange, n.getRange.reverse).zipped.map( _ >> _.intValue )
        fromSInt(res,getFracWidth,List2Tuple(newRange))
      }
    }
    out.pass2to1(this,n)
  }

  /** Variable left shift this*2^n */
  def << (n: DSPUInt) : DSPFixed = {
    val out = {
      if (n.isLit) this << n.litValue().intValue
      else{
        val res = this.toSInt << n.toUInt
        val newRange = (getRange, n.getRange).zipped.map( _ << _.intValue )
        fromSInt(res,getFracWidth,List2Tuple(newRange))
      }
    }
    out.pass2to1(this,n)
  }

  /** select ? this (true) : 0 (false) -- used for Mux */
  def ? (select: DSPBool): DSPFixed = {
    val out = {
      if (select.isLit) {if (select.isTrue) this else DSPFixed(0,getFracWidth)}
      else{
        val res = this.toBits & Fill(getWidth,select.toBits)
        fromSInt(res.toSInt,getFracWidth,List2Tuple(getRange))
      }
    }
    out.pass2to1(this,select)
  }

  /** Bitwise-OR with custom range inference */
  def /| (b: DSPFixed) : DSPFixed = {
    val out = {
      if (isLit && signed_fix() == 0) b
      else if (b.isLit && b.signed_fix() == 0) this
      else{
        val (x,y, xrange, yrange,fixedParams) = matchWidth(b)
        val opMax =  xrange.max.max(yrange.max)
        val opMin =  xrange.min.min(yrange.min)
        fromSInt(x | y,fixedParams._2,(opMin,opMax))
      }
    }
    out.pass2to1(this,b)
  }

  /** Truncate to n fractional bits.
    * For positive # abs. value decreases; for negative # abs. value increases
    */
  override def $ (n: Int) : DSPFixed = {
    if (n < 0) Error("Can't truncate to negative fractional bits.")
    val truncateAmount = getFracWidth-n
    if (truncateAmount <= 0) this
    else{
      val temp = this >> truncateAmount
      val out = fromSInt(temp.toSInt,n,List2Tuple(temp.getRange))
      out.updateGeneric(this)
    }
  }

  // TODO: Overflow handling
  /** Round (half up) to n fractional bits
    * Reduces bias as compared to truncation (bias still present at half-way pt)
    * Generally rounds by magnitude except at half pt, which rounds to more positive value
    */
  override def $$ (n: Int, of: OverflowType) : DSPFixed = {
    val truncateAmount = getFracWidth-n
    if (truncateAmount <= 0) this
    else {
      val truncated = this $ n
      val roundingBit = this(truncateAmount-1)
      // Possible overflow
      val resShort =  Cat(Bits(0,1),truncated) + roundingBit
      val newRange = (truncated.getRange.min-1,truncated.getRange.max+1)
      val out = fromSInt(resShort.toSInt,n,newRange)
      out.updateGeneric(this)
    }
  }

  // TODO: When rounding up from 3.8 to 4, it overflows
  /** Gets integer portion of DSPFixed. Optional rounding mode. */
  def toInt(r: TrimType): DSPFixed = {
    if (r == Truncate) this $ 0
    else if (r == Round) this $$ (0, Grow)
    else error("Invalid trim type")
  }

  /** Add overflow -> wrap to max(this.width,b.width) # of bits */
  override def +% (b: DSPFixed) : DSPFixed = {
    val out = {
      if (isLit && signed_fix() == 0) b
      else if (b.isLit && b.signed_fix() == 0) this
      else{
        val (x,y,xrange,yrange,fixedParams) = matchWidth(b)
        val newRange = (xrange, yrange).zipped.map( _ + _ )
        val bitRange = DSPFixed.toRange(x.getWidth)
        val opMax = bitRange._2.min(newRange.max)
        val opMin = bitRange._1.max(newRange.min)
        fromSInt(x+y,fixedParams,(opMin,opMax))
      }
    }
    out.pass2to1(this,b)
  }

  /** Explicit add with bit growth
    * Range determined by sum bitwidth
    */
  override def +& (b: DSPFixed) : DSPFixed = {
    val (x,y,xrange,yrange,(intW,fracW)) = matchWidth(b)
    val sum = Cat(sign.toBits,x)+Cat(b.sign.toBits,y)
    val newFixedParams = (intW+1,fracW)
    val out = fromSInt(sum.toSInt,newFixedParams,DSPFixed.toRange(DSPFixed.paramsToWidth(newFixedParams)))
    out.pass2to1(this,b)
  }

  /** Add that determines optimal sum range, bitwidth */
  def + (b: DSPFixed) : DSPFixed = {
    val out = {
      if (isLit && signed_fix() == 0) b
      else if (b.isLit && b.signed_fix() == 0) this
      else{
        val (x,y,xrange,yrange,(intW,fracW)) = matchWidth(b)
        val newRange = (xrange, yrange).zipped.map( _ + _ )
        fromSInt((this +& b).toSInt, fracW, List2Tuple(newRange))
      }
    }
    out.pass2to1(this,b)
  }

  // TODO: Separately handle cases when both operator inputs are Lits for ALL operations (+,-,*)
  // --> reason behind delay mismatch errors

  /** Make sure Lit fractional widths are also matched */
  def matchLitFracWidth(b: DSPFixed) : Tuple3[BigInt,BigInt,Int] = {
    val fracWidth = getFracWidth.max(b.getFracWidth)
    val fracDiff = getFracWidth - b.getFracWidth
    val x = signed_fix()
    val y = b.signed_fix()
    val out = {
      if (fracDiff > 0) (x, y << fracDiff)
      else if (fracDiff < 0) (x << math.abs(fracDiff), y)
      else (x,y)
    }
    (out._1,out._2,fracWidth)
  }

  /** Fix sign of Lit */
  def signed_fix() : BigInt = {
    val rv = litValue()
    val w = rv.bitLength.max(getWidth)
    if(rv >= (BigInt(1) << w - 1)) (rv - (BigInt(1) << w)) else rv
  }

  /** Subtract and wrap */
  override def -% (b: DSPFixed) : DSPFixed = {
    val out = {
      if (b.isLit && b.signed_fix() == 0) this
      else if (isLit && b.isLit) {
        val (aT,bT,fracW) = matchLitFracWidth(b)
        DSPFixed(aT-bT,fracW)
      }
      else{
        val (x,y,xrange,yrange,fixedParams) = matchWidth(b)
        val newRange = (xrange, yrange.reverse).zipped.map( _ - _ )
        val bitRange = DSPFixed.toRange(x.getWidth)
        val opMax = bitRange._2.min(newRange.max)
        val opMin = bitRange._1.max(newRange.min)
        fromSInt(x-y,fixedParams,(opMin,opMax))
      }
    }
    out.pass2to1(this,b)
  }

  /** Explicit sub with bit growth
    * Range determined by difference bitwidth
    */
  override def -& (b: DSPFixed) : DSPFixed = {
    val (x,y,xrange,yrange,(intW,fracW)) = matchWidth(b)
    val diff = Cat(sign.toBits,x).toSInt-Cat(b.sign.toBits,y).toSInt
    val newFixedParams = (intW+1,fracW)
    val out = fromSInt(diff.toSInt,newFixedParams,DSPFixed.toRange(DSPFixed.paramsToWidth(newFixedParams)))
    out.pass2to1(this,b)
  }

  /** Sub that determines optimal diff range, bitwidth */
  def - (b: DSPFixed) : DSPFixed = {
    val out = {
      if (b.isLit && b.signed_fix() == 0) this
      else if (isLit && b.isLit) {
        val (aT,bT,fracW) = matchLitFracWidth(b)
        DSPFixed(aT-bT,fracW)
      }
      else{
        val (x,y,xrange,yrange,(intW,fracW)) = matchWidth(b)
        val newRange = (xrange, yrange.reverse).zipped.map( _ - _ )
        fromSInt((this -& b).toSInt, fracW, List2Tuple(newRange))
      }
    }
    out.pass2to1(this,b)
  }

  /** 0 - this (mathematically correct; bitwidth sized accordingly) */
  override def unary_-():DSPFixed = DSPFixed(BigInt(0),(getIntWidth,getFracWidth)) - this

  // TODO: Overflow handling for *, also check correctness of n * -1 conversion into 0-n (correct sub from -, -%, -&)
  // see https://sestevenson.wordpress.com/2009/08/10/overflow-handling-in-fixed-point-computations/
  // for -1 multiplication overflow problem
  /** Multiply while determining optimal product bitwidth + range */
  def * (b: DSPFixed) : DSPFixed = {
    val out = {
      if (isLit && signed_fix() == 0) DSPFixed(0,(getIntWidth,getFracWidth))
      else if (isLit && signed_fix() == DSPFixed.toFixed(1,getFracWidth)) b
      else if (isLit && signed_fix() == DSPFixed.toFixed(-1,getFracWidth)) -b
      else if (b.isLit && b.signed_fix() == 0) DSPFixed(0,(b.getIntWidth,b.getFracWidth))
      else if (b.isLit && b.signed_fix() == DSPFixed.toFixed(1,b.getFracWidth)) this
      else if (b.isLit && b.signed_fix() == DSPFixed.toFixed(-1,b.getFracWidth)) -this
      else {
        val (x,y,xrange,yrange,(intW,fracW)) = matchWidth(b)
        val res = x * y
        val rangeOutcomes = for { xr <- xrange; yr <- yrange } yield xr * yr
        fromSInt(res, fracW*2, (rangeOutcomes.min,rangeOutcomes.max))
      }
    }
    out.pass2to1(this,b)
  }

  /** Restrict range of input */
  def clamp(max:Double): DSPFixed = clamp(DSPFixed(max,getFracWidth))
  def clamp (range: => (Double,Double)): DSPFixed = {
    clamp((DSPFixed(range._1,getFracWidth),DSPFixed(range._2,getFracWidth)))
  }

  // TODO: Redundant List2Tuple
  /** Shorten range for DSPSInt */
  override def shorten(newRange: List[BigInt]): DSPFixed = {
    if (newRange.length != 2) Error("Range list can only have 2 values")
    shorten(List2Tuple(newRange))
  }

  // TODO: Combine better with shortenTo and min,max update (like in DSPUInt)
  def shorten(newRange: (BigInt,BigInt)): DSPFixed = {
    // TODO: Enforce that Tuple, List[BigInt] only come from getRange, etc. (so user doesn't type unusuable range 
    // i.e. LSB is normalized based on fractional width for DSPFixed)
    // if (getFracWidth != 0) Error("Can only use shorten on DSPSInt (fractional width = 0), not DSPFixed")
    val newWidth = DSPFixed.rangeToWidth(newRange)
    if (newWidth > getWidth) Error("New width must be <= old DSPSInt width.")
    val min = newRange._1.max(getRange.min)
    val max = newRange._2.min(getRange.max)
    val out = fromSInt(this.toSInt,getFracWidth,(min,max))
    out.updateGeneric(this)
  }

  // TODO: Shorten for Fixed, in general (not just DSPSInt)

}

