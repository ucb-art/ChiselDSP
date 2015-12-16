/** Fixed class customizations 
  * TODO: Actually implement operators... :(
  */

package ChiselDSP
import Chisel._

object DSPFixed {

  /** Creates a DSPFixed object from a constant Double. 
    * fixedParams = (intWidth,fracWidth), or alternatively, fracWidth, must be specified!
    * Qn.m --> n = intWidth, m = fracWidth, width = n + m + 1 (sign).
    * Note that for Lits, the intWidth serves as a guide for maximum # of
    * integer bits needed. If the Lit can be represented in fewer bits,
    * the smaller # is used.
    */
  def apply(x: Double, fixedParams: (Int, Int)): DSPFixed = apply(toFixed(x, fixedParams._2), fixedParams)
  def apply(x: Double, fracWidth: Int): DSPFixed = {
    val fixed = toFixed(x, fracWidth)
    val intWidth = fixed.bitLength-fracWidth
    apply(fixed,(intWidth,fracWidth))
  }
  /** Creates DSPFixed Literal after Double has been converted into a BigInt */
  private def apply(x: BigInt, fixedParams: (Int, Int)): DSPFixed = {
    val range = (x, x)
    val litVal = apply(NODIR, fixedParams, range)
    val res = Lit(x, litVal.getWidth) {litVal}
    res.fractionalWidth = fixedParams._2
    res.asDirectionless
    res.updateLimits(range)
    res.assign()
  }

  /** Creates a DSPFixed object with a specified IODirection, (intWidth,fracWidth) */
  def apply(fixedParams: (Int,Int)): DSPFixed = apply(NODIR,fixedParams)
  def apply(dir: IODirection, fixedParams: (Int, Int)): DSPFixed = {
    apply(dir, fixedParams, toRange(paramsToWidth(fixedParams)))
  }
  /** Creates a DSPFixed object with a specified IODirection, (intWidth,fracWidth), 
    * and [optional] Double (min,max) range.
    */
  def apply(fixedParams:(Int,Int), range: (Double,Double)) : DSPFixed = apply(NODIR,fixedParams,range)
  def apply(dir: IODirection, fixedParams:(Int,Int), range: (Double,Double)) : DSPFixed = {
    val min = toFixed(range._1, fixedParams._2)
    val max = toFixed(range._2, fixedParams._2)
    apply(dir,fixedParams,(min,max))
  }
  def apply(fracWidth:Int, range: (Double,Double)) : DSPFixed = apply(NODIR,fracWidth,range)
  def apply(dir: IODirection = NODIR, fracWidth:Int, range: (Double,Double)) : DSPFixed = {
    val min = toFixed(range._1, fracWidth)
    val max = toFixed(range._2, fracWidth)
    apply(dir,fracWidth,(min,max))
  }

  private def apply(dir: IODirection, fracWidth: Int, range: (BigInt,BigInt)): DSPFixed = {
    val intWidth = rangeToWidth(range)-1-fracWidth
    apply(dir, (intWidth, fracWidth), range)
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
  private def apply_gen(dir : IODirection, fixedParams: (Int, Int), range: (BigInt,BigInt)) : DSPFixed = {
    val updateBits = (range != null)
    val width = paramsToWidth(fixedParams)
    val rangeWidth = if (updateBits) rangeToWidth(range) else width
    if (updateBits){ 
      if (fixedParams._1 < 0) Error("Fixed integer width must be non-negative.")
      if (fixedParams._2 < 0) Error("Fixed fractional width must be non-negative.")
      if (rangeWidth > width) Error("Rounded + normalized to LSB range [" + range._1 + "," + range._2 +
        "] of Fixed value greater than range allowed by fixed parameters ["
        + fixedParams._1 + "," + fixedParams._2 + "]")
    }
    val res = new DSPFixed(fixedParams._2)
    res.create(dir, if (updateBits) rangeWidth else -1)
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
    out.copyInfo(this).asInstanceOf[this.type]
  }

  /** Convert SInt to a DSPFixed by reinterpreting the Bits */
  private def fromSInt(s: SInt, fracWidth:Int, opRange: (BigInt, BigInt)): DSPFixed = {
    val res = chiselCast(s){DSPFixed(s.dir,fracWidth,opRange)}
    res.assign()
  }

  /** Print DSPFixed info */
  override def infoString() : String = "+/-Q"+getIntWidth+"."+getFracWidth+", range = " + rangeString(getFracWidth)

  /** Get widths */
  def getIntWidth(): Int = getWidth-getFracWidth-1
  def getFracWidth(): Int = fractionalWidth

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
  protected def updateLimits(range: (BigInt,BigInt)): Unit = {
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
  private def matchWidth(that: DSPFixed) : Tuple5[SInt,SInt,List[BigInt],List[BigInt],Int] = {
    val (x,y,xrange,yrange,fracWidth) = matchFracWidth(that)
    val widthDiff = y.getWidth - x.getWidth
    val (a,b) = {
      if (widthDiff > 0) (Cat(Fill(widthDiff,sign.toBits),x).toSInt, y)
      else if (widthDiff < 0) (x, Cat(Fill(-widthDiff,that.sign.toBits),y).toSInt)
      else (x, y)
    }
    (a,b,xrange,yrange,fracWidth)
  }

  /** Check fractional width alignment */
  private def checkAlign(f: DSPFixed): Unit = {
    if (getFracWidth != f.getFracWidth) Error("Fractional widths should match.")
  }

  /** Reassign with ":=". Certain conditions must be enforced so delay + range are consistent.
    * Note that fracWidths of L,R in L := R should match.
    */
  override protected def colonEquals(that : Bits): Unit = {
    that match {
      case f: DSPFixed => {
        checkAlign(f)
        reassign(f)
        val (x,y,xrange,yrange,fracWidth) = matchWidth(f)
        super.colonEquals(fromSInt(y,fracWidth,List2Tuple(yrange)))
      }
      case _ => illegalAssignment(that)
    }
  }

  /** Shorten # of integer bits -- get rid of MSBs by forcing the generator to use a smaller width
    * CAREFUL: could eliminate useful bits!!!
    */
  override def shortenTo(intWidth: Int): DSPFixed = {
    val newWidth = DSPFixed.paramsToWidth((intWidth,getFracWidth))
    if (newWidth > getWidth) Error("New width must be <= old Fixed width.")
    val out = fromSInt(this.toSInt,getFracWidth,DSPFixed.toRange(newWidth))
    out.updateGeneric(this)
  }

  /** TODO: Implement */
  def >> (n:Int) : DSPFixed = this
  def >> (b:DSPUInt):DSPFixed = this
  def <<(n:Int):DSPFixed = this
  def <<(b:DSPUInt):DSPFixed = this
  def > (b:DSPFixed):DSPBool = DSPBool(true)
  def >= (b:DSPFixed):DSPBool =DSPBool(true)
  def < (b:DSPFixed):DSPBool= DSPBool(true)
  def <= (b:DSPFixed):DSPBool = DSPBool(true)
  def + (b:DSPFixed):DSPFixed = this
  override def +& (b:DSPFixed):DSPFixed = this
  override def +% (b:DSPFixed):DSPFixed = this
  def - (b:DSPFixed):DSPFixed = this
  override def -& (b:DSPFixed):DSPFixed = this
  override def -% (b:DSPFixed):DSPFixed = this
  override def unary_-():DSPFixed = this
  def * (b:DSPFixed):DSPFixed = this
  def ? (n:DSPBool) : DSPFixed = this
  def /| (b: DSPFixed) : DSPFixed = this

}