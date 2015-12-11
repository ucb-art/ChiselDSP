/** Fixed class customizations 
  * TODO: Actually implement operators... :(
  */

package ChiselDSP
import Chisel._

object DSPFixed{

  /** Creates a DSPFixed object from a constant Double. 
    * fixedParams = (intWidth,fracWidth) must be specified!
    * Qn.m --> n = intWidth, m = fracWidth, width = n + m + 1 (sign)
    */
  def apply(x: Double, fixedParams: (Int,Int)) : DSPFixed = apply(toFixed(x, fixedParams._2), fixedParams)
 
 /** Creates DSPFixed Literal after Double has been converted into a BigInt */
  private def apply(x : BigInt, fixedParams: (Int, Int)) : DSPFixed =  { 
    val width = toWidth(fixedParams)
    val range = (x,x)
    val res = Lit(x, width){apply(NODIR,fixedParams,range)}
    res.asDirectionless
    res.updateLimits(range)
    res.assign()
  }
  
  /** Creates a DSPFixed object with a specified IODirection, (intWidth,fracWidth) */
  def apply(dir: IODirection, fixedParams:(Int,Int)) : DSPFixed = apply(dir,fixedParams,toRange(toWidth(fixedParams)))
  /** Creates a DSPFixed object with a specified IODirection, (intWidth,fracWidth), 
    * and [optional] Double (min,max) range.
    */
  def apply(dir: IODirection, fixedParams:(Int,Int), range: (Double,Double)) : DSPFixed = {
    val min = toFixed(range._1, fixedParams._2)
    val max = toFixed(range._2, fixedParams._2)
    apply(dir,fixedParams,(min,max))
  }
  
  /** Intermediate apply function to check if range is null */
  private def apply(dir: IODirection, fixedParams:(Int,Int), range: (BigInt,BigInt)): DSPFixed = {
    if (range == null) Error("Range cannot be null.")
    apply_gen(dir,fixedParams,range)
  }
  
  /** Internally handles some edge cases i.e. when Chisel relies on node conversion,
    * width/range is not defined up front
    */
  private def apply_gen(dir : IODirection, fixedParams: (Int, Int), range: (BigInt,BigInt)) : DSPFixed = {
    val updateBits = (range != null)
    if (updateBits){ 
      if (fixedParams._1 < 0) Error("Fixed integer width must be non-negative.")
      if (fixedParams._2 < 0) Error("Fixed fractional width must be non-negative.")
      
      
      
      
    }
    val res = new DSPFixed(fixedParams._2)
    res.create(dir, if (updateBits) toWidth(fixedParams) else -1)
    if (updateBits) res.updateLimits(range)
    res
  }
 
 
 
 
 
   w/ width range = toRange(w) if ((x < range._1) || (x > range._2)) throwException("Literal not in Fixed range")

 
 
 
 
 
 
 
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
  def toWidth(fixedParams:(Int,Int)): Int = fixedParams._1 + fixedParams._2 + 1
  
  
  
 
 
 
 
 
 
 
 
 
 
 

  
  
  
  
  
 
 
 
 
 
 
 
    
 

 

  
  
  

  
  
  

  
  
  
  
  
















}


// getfractionalwidth



class DSPFixed (private var fractionalWidth:Int = 0)  extends MyBits with MyNum[MyFixed] {
  override def >> (n:Int) : MyFixed = {this}
  override def ? (n:MyBool) : MyFixed = {this}
  def fromInt(x: Int):this.type = this
  def /| (b: MyFixed) : MyFixed = this
  def >> (b:MyUInt):MyFixed = this
  def <<(b:MyUInt):MyFixed = this
  def <<(n:Int):MyFixed = this
  override def pipe(n:Int):this.type = this
  def / (b:MyFixed):MyFixed = this
  def > (b:MyFixed):MyBool = MyBool(true)
  def >= (b:MyFixed):MyBool =MyBool(true)
  def < (b:MyFixed):MyBool= MyBool(true)
  def <= (b:MyFixed):MyBool = MyBool(true)
  def - (b:MyFixed):MyFixed = this
  def % (b:MyFixed):MyFixed = this
  def + (b:MyFixed):MyFixed = this
  def * (b:MyFixed):MyFixed = this
  def unary_-():MyFixed = this
  def getFractionalWidth() : Int = 10
  def printWidth() : String = ("+/-Q"+this.getIntWidth+"."+this.getFracWidth)
  

  
  private def fromSInt(s: SInt, fracWidth:Int) : MyFixed = {
    
    val intWidth = s.getWidth - 1 -fracWidth
    chiselCast(s){MyFixed.apply(s.dir,(intWidth,fracWidth))}
  }
  
  override def fromNode(n: Node): this.type = {
    val res = MyFixed.apply(OUTPUT,-1).asTypeFor(n).asInstanceOf[this.type]
    n match {
      case l: Literal =>
        if (l.isZ && Driver.minimumCompatibility > "2") {
          // Chisel3 compatibility - generic don't care UInts/Bits are deprecated.
          ChiselError.warning("General don't care MyUInts are deprecated. Please use BitPat().")
        }
      case _ =>
    }
    res.fractionalWidth = this.fractionalWidth
    res
  }
  
    def getIntWidth() : Int = this.getWidth-this.fractionalWidth-1
  
  def getFracWidth() : Int = this.fractionalWidth
  
  override protected def colonEquals(that : Bits): Unit = {
    that match {
      case f: MyFixed => {
        
        
        super.colonEquals(fromSInt(f.toSInt,f.getFracWidth))

      }
      case _ => illegalAssignment(that)
    }
  }
  
  
}





