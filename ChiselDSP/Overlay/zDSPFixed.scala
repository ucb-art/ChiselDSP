/** Fixed class customizations 
  * TODO: Actually implement operators... :(
  */

package ChiselDSP
import Chisel._

object DSPFixed {

  /** Creates a DSPFixed object from a constant Double. 
    * fixedParams = (intWidth,fracWidth) must be specified!
    * Qn.m --> n = intWidth, m = fracWidth, width = n + m + 1 (sign)
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
    res.asDirectionless
    res.updateLimits(range)
    res.assign()
  }

  /** Creates a DSPFixed object with a specified IODirection, (intWidth,fracWidth) */
  def apply(dir: IODirection, fixedParams: (Int, Int)): DSPFixed = {
    apply(dir, fixedParams, toRange(paramsToWidth(fixedParams)))
  }
  /** Creates a DSPFixed object with a specified IODirection, (intWidth,fracWidth), 
    * and [optional] Double (min,max) range.
    */
  def apply(dir: IODirection, fixedParams:(Int,Int), range: (Double,Double)) : DSPFixed = {
    val min = toFixed(range._1, fixedParams._2)
    val max = toFixed(range._2, fixedParams._2)
    apply(dir,fixedParams,(min,max))
  }
  def apply(dir: IODirection, fracWidth:Int, range: (Double,Double)) : DSPFixed = {
    val min = toFixed(range._1, fracWidth)
    val max = toFixed(range._2, fracWidth)
    apply(dir,fracWidth,(min,max))
  }

  private def apply(dir: IODirection, fracWidth: Int, range: (BigInt,BigInt)): DSPFixed = {
    val intWidth = rangeToWidth(range)-1-fracWidth
    apply(dir, (intWidth, fracWidth), range)
  }
  /** Intermediate apply function to check if range is null */
  private def apply(dir: IODirection, fixedParams:(Int,Int), range: (BigInt,BigInt)): DSPFixed = {
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
      if (rangeWidth > width) Error("Rounded range of Fixed value greater than range allowed by width.")
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
    val absRange = (BigInt(2) << dataBits)
    val max = absRange -1
    val min = -1 * absRange
    (min,max)
  }
  
  /** Returns width from fixedParams (accounting for sign bit) */
  def paramsToWidth(fixedParams:(Int,Int)): Int = fixedParams._1 + fixedParams._2 + 1

  /** Returns width from range */
  def rangeToWidth(range:(BigInt,BigInt)): Int = range._1.bitLength.max(range._2.bitLength) + 1

}

class DSPFixed (private val fractionalWidth:Int = 0)  extends DSPQnm[DSPFixed] {

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
  def getFractionalWidth(): Int = fractionalWidth

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
    setRangeBits(toRange(getWidth))
    setRange(range)
  }











  /*
  colonequals -- does match width change meaning? -- need to shift
  matchwidth

   */

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

  /** Shorten # of bits -- get rid of MSBs by forcing the generator to interpret a new (smaller, positive) max
    * CAREFUL: could eliminate useful bits!!!
    */
  def shorten(newMax: BigInt): DSPUInt = {
    val newMin = getRange.min.min(newMax)
    val out = toT(this,List2Tuple(getRange),(newMin,newMax))
    out.updateGeneric(this)
  }

  /** Match operator widths */
  private def matchWidth(y: DSPUInt) : Tuple2[Bits,Bits] = {
    val diff = y.getWidth - getWidth
    if (diff > 0) (Cat(Fill(diff,sign.toBits),this), y.toBits)
    else if (diff < 0) (this.toBits, Cat(Fill(-diff,y.sign.toBits),y))
    else (this.toBits, y.toBits)
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





