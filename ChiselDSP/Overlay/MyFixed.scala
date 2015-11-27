/* Fixed class customizations */

package ChiselDSP
import Chisel._



class MyFixed (private var fractionalWidth:Int = 0)  extends MyBits with MyNum[MyFixed] {
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


object MyFixed{


def toFixed(x: Double, fracWidth: Int) : BigInt = BigInt(math.round(x*math.pow(2,fracWidth)))


def toDouble(x: BigInt, fracWidth: Int) : Double = x.doubleValue/math.pow(2,fracWidth)

def apply(x: Double, fixedParams: (Int,Int)) : MyFixed = {
 
 val width = fixedParams._1 + fixedParams._2 + 1
 
 //println(fixedParams._1)
 //println(fixedParams._2)
 
val t = toFixed(x, fixedParams._2)
val res = Lit(t, width){apply(NODIR,fixedParams)}
    
    res
  }

def apply(dir: IODirection, fixedParams: (Int,Int)) : MyFixed = {

  val intWidth = fixedParams._1 
    val fracWidth = fixedParams._2 
    val width = intWidth + fracWidth + 1
   
    
    
  val res = new MyFixed(fracWidth)
  res.create(dir, width)
  res

}


def apply(dir: IODirection, width :Int) : MyFixed = {

  val res = new MyFixed()
  res.create(dir, width)
  res

}



}


/*

object MyFixed{

  def toDouble(x: BigInt, fracWidth: Int) : Double = x.doubleValue/math.pow(2,fracWidth)
 
  def toFixed(x: Double, fracWidth: Int) : BigInt = BigInt(math.round(x*math.pow(2,fracWidth)))

  def apply(x : Double, fixedParams: (Int, Int)) : MyFixed = apply(toFixed(x, fixedParams._2), fixedParams)
  
  def apply(x : BigInt, fixedParams: (Int, Int)) : MyFixed =  { 
    val res = Lit(x, fixedParams._1){apply_gen(NODIR,fixedParams,null)}
    val range = toRange(fixedParams._1)
    res.initLimits()
    if ((x < range._1) || (x > range._2)) throwException("Literal not in Fixed range")
    res.updateLimits((x,x)) 
    // Don't update constants
    res.used()
    res
  }
  
  def toRange(w: Int) : Tuple2[BigInt,BigInt] = {
    // Don't include sign bit
    val dataBits = w-1
    val max = (BigInt(2) << dataBits) -1
    val min = -1 * (BigInt(2) << dataBits)
    (min,max)
  }
  
  def apply(dir: IODirection, fixedParams: (Int,Int)) : MyFixed = apply_gen(dir,fixedParams,null)
  
  private def apply_gen(dir : IODirection, fixedParams: (Int, Int), limits: (BigInt,BigInt)) : MyFixed = {
    val updateBits = (fixedParams != null)
    val width = if (updateBits) fixedParams._1 else -1
    val fracWidth = if (updateBits) fixedParams._2 else -1 
    if (updateBits){ 
      if (fixedParams._1 < 1) throwException("Fixed width must be > 0")
      if (fixedParams.2 < 0) throwException("Fractional width must be non-negative")
      if ((width-fracWidth) < 1) throwException("Signed fixed width must be > fractional width")
    }
    val res = new MyFixed(fracWidth)
    res.create(dir, width)
    if (updateBits) res.initLimits()
    if (dir == INPUT) res.updateLimits(toRange(width))
    else if (limits != null) res.updateLimits(limits)
    res
  }
  
}

class MyFixed (private var fractionalWidth:Int = 0) extends MyBits with MyNum[MyFixed] {

  type T = MyFixed
  
  def getIntWidth() : Int = this.width-this.fractionalWidth-1
  
  def getFracWidth() : Int = this.fractionalWidth
  
  // Convert a Node to a MyFixed data type with the same fractional width as this instantiation 
  override def fromNode(n : Node): this.type = MyFixed.apply_gen(OUTPUT,null,null).asTypeFor(n).asInstanceOf[this.type]

  // Create a MyFixed representation from an Int 
  override def fromInt(x : Int) : this.type = MyFixed(x, (this.width, this.fractionalWidth)).asInstanceOf[this.type]

  // Convert an SInt to a MyFixed by reinterpreting the Bits - note that width in fixedParams should be worst case
  private def fromSInt(s: SInt, fracWidth:Int, opLimits: (BigInt, BigInt)) : MyFixed = {
    val dataBits = opLimits._1.bigLength.max(opLimits._2.bitLength)                       // Not including sign bit
    val fixedWidth = s.getWidth.min(dataBits+1)
    chiselCast(s){MyFixed.appy_gen(s.dir,(fixedWidth,fracWidth),opLimits)}
  }
   
  // Max + min allowed by bitwidths
  private var maxBits = BigInt(0)
  private var minBits = BigInt(0)
  // Range of possible values
  private var max = BigInt(0)
  private var min = BigInt(0)
  
  private def initLimits() : Unit = {
    val range = MyFixed.toRange(fixedParams._1)
    (minBits,maxBits) = range
    // Start from opposite of worst case in given direction
    (max,min) = range
  }

  def printWidth() : String = ("+/-Q"+this.getIntWidth+"."+this.fractionalWidth)
  
  def getLimits() : Tuple2[BigInt, BigInt] = (min,max)
  
  def getParams() : Tuple2[BigInt, BigInt] = (this.width,this.fractionalWidth)

  private def updateLimits(limits: (BigInt,BigInt)): Unit = {
    val (a,b) = limits
    max = b.max(max)
    min = a.min(min)
    if (max > maxBits) {
      Warn("Warning, possible MyFixed overflow: max > max by bits. Signals down the chain may be wrong.")
      max = maxBits
    }
    if (max < minBits) {
      Warn("Warning, possible MyFixed overflow: max < min by bits. Signals down the chain may be wrong.")
      max = minBits
    }
    if (min < minBits) {
      Warn("Warning, possible MyFixed overflow: min < min by bits Signals down the chain may be wrong.")
      min = minBits
    }
    if (min > maxBits) {
      Warn("Warning, possible MyFixed overflow: min > max by bits. Signals down the chain may be wrong.")
      min = maxBits
    }
  }
  
  // Make sure operands have same fractional width: Outputs frac width matched operands + frac width used
  private matchFracWidth (a: MyFixed, b: MyFixed) : Tuple3[SInt,SInt,Int] = {
    val fracWidth = a.getFracWidth.max(b.getFracWidth)
    val diff = a.getFracWidth - b.getFracWidth
    val zro = Bits(0,1)
    if (diff > 0) (a.toSInt,Cat(b,Fill(diff,zro)).toSInt,fracWidth)
    else if (diff < 0) (Cat(a,Fill(-diff,zro)).toSInt,b.toSInt,fracWidth)
    else (a.toSInt,b.toSInt,fracWidth)
  }
  
  // Sign extend a/b width if shorter than b/a width: Outputs width matched operands + frac width used  
  private def matchWidth(a: MyFixed, b: MyFixed) : Tuple2[SInt,SInt,Int] = {
    val (x,y,fracWidth) = matchFracWidth(a,b)
    val diff = y.getWidth - x.getWidth
    val sx = x(x.getWidth-1)
    val sy = y(y.getWidth-1)
    if (diff > 0) (Cat(Fill(diff,sx),x).toSInt, y.toSInt,fracWidth) 
    else if (diff < 0) (x.toSInt, Cat(Fill(-diff,sy),y).toSInt,fracWidth) 
    else (x.toSInt, y.toSInt,fracWidth)
  }
  
  private var noUpdate = false
  
  private def used() : Unit = (noUpdate = true)
  
  def doNothing() : MyFixed = {this.used(); this}
  
  // Ensure two Fixed point data types have the same fractional width, Error if not 
  private def checkAligned(b : Fixed) : Unit = {
    if (this.fractionalWidth != b.getFracWidth) throwException(" Fractional bits do not match ")
  }  

  override protected def colonEquals(that : Bits): Unit = {
    if (this.noUpdate) throwException("Previous lines of code have used signal. To ensure range consistency, signal cannot be updated. Move := earlier in code!")
    that match {
      case f: MyFixed => {
        checkAligned(f)
        val (x,y,_) = matchWidth(this,f)
        super.colonEquals(fromSInt(y,f.getFracWidth,f.getLimits))
        this.updateLimits(f.getLimits)
      }
      case _ => illegalAssignment(that)
    }
  }

  // Select ? this : 0
  def ? (select: Bool) : MyFixed = {
    this.used()
    val res = (this & Fill(this.width,select)).toSInt
    fromSInt(res,this.fractionalWidth,this.getLimits)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  // Arithmetic (signed) shift right by n --> this/2^n
  // 
  override def >> (n : Int) : MyFixed = {
    this.used()
    if (n < 0) throwException("Shift amount must be non-negative")
    val s = this(this.width-1)
    val res = {
      if (n > (this.width-1)) Fill(this.width,s)
      else if (n == 0) this.toSInt
      else Cat(Fill(n,s),this(this.width-1,n)).toSInt
    }
    fromSInt(res,this.fractionalWidth,newLimits)
    
    
    
    
    
    
    
    underflow????  
        out.updateLimits(this.upperBound/math.pow(2,b),this.lowerBound/math.pow(2,b))
        out
      }
    }
  }



*/


// ad multiply w/ 1, 0 - optimize?

  /*
  // try + , +&; see where they go/come from
   
  }*/
  
  
  
  // saturate function w/ OF flag,   // mux,pipe, >><< uint
  
  
  
/*

WARNING MAY BREAK

  
    
  // Order Operators
  def > (b : Fixed) : Bool = {
    checkAligned(b)
    val (x,y) = matchWidth(this,b)
    x > y
  }

  def < (b : Fixed) : Bool = {
    checkAligned(b)
    val (x,y) = matchWidth(this,b)
    x < y
  }

  def >= (b : Fixed) : Bool = {
    checkAligned(b)
    val (x,y) = matchWidth(this,b)
    x >= y
  }

  def <= (b : Fixed) : Bool = {
    checkAligned(b)
    val (x,y) = matchWidth(this,b)
    x <= y
  }

  def === (b : Fixed) : Bool = {
    checkAligned(b)
    val (x,y) = matchWidth(this,b)
    x === y
  }
    
    
    
  // Arithmetic shift left (signed multiply by 2^b) CAN OVERFLOW - always compare first!
  override def <<< (b : Int) : Fixed = {
    if (b < 0) ChiselError.error("Shift amount should be >= 0")
    if (b == 0) this else {
      val out = fromSInt(Cat(this.toUInt,Fill(b,Bits(0,width=1))).toSInt)
      out.updateLimits(this.upperBound*math.pow(2,b),this.lowerBound*math.pow(2,b))
      out
    }
  }    
    
  // Check worst case sum (max, min) and detect overflow  
  def checkSumOverflow(a: Fixed, b: Fixed) : Tuple3[Boolean,Double,Double] = {
    val maxSum = a.upperBound + b.upperBound
    val minSum = a.lowerBound + b.lowerBound
    val maxInBits = if (a.maxBits > b.maxBits) a.maxBits else b.maxBits
    val minInBits = if (a.minBits < b.minBits) a.minBits else b.minBits
    var OF = true
    if (maxSum <= maxInBits && minSum >= minInBits) OF = false 
    (OF,maxSum,minSum)
  }

  // "Wrapping" add : Q(n_max).m i.e. no overflow handling
  def + (b : Fixed) : Fixed = {
    checkAligned(b)
    val (overflow,maxSum,minSum) = checkSumOverflow(this,b)
    val (x,y) = matchWidth(this,b)
    val sum = fromSInt(x+y, x.getWidth(), this.getFractionalWidth())
    sum.updateLimits(maxSum,minSum)
    sum
  }
  
  //+% wrap and store bounds while + no store bounds

  // Add (max_width+1) operator where applicable i.e. integer width increases if overflow possible
  def +& (b : Fixed) : Fixed = {
    val (overflow,maxSum,minSum) = checkSumOverflow(this,b)
    //if (!overflow) this + b
    //else{
      checkAligned(b)
      val (x,y) = matchWidth(this,b) 
      // Sign extend
      val j = Cat(x(x.getWidth()-1),x.toBits).toSInt
      val k = Cat(y(y.getWidth()-1),y.toBits).toSInt
      val sum = fromSInt(j+k,x.getWidth()+1,this.getFractionalWidth())
      sum.updateLimits(maxSum,minSum)
      sum
    //}   
  }

  // Add saturate (if overflow) operator where applicable
  def +@ (b : Fixed) : Fixed = {
    val (overflow,maxSum,minSum) = checkSumOverflow(this,b)
    //if (!overflow) this + b
    //else{
      checkAligned(b)
      val (x,y) = matchWidth(this,b) 
      // width = max(wx,wy)
      val sum = x + y
      // Sign bits of x,y,z where x+y=z
      val xs = this(this.getWidth()-1)
      val ys = b(b.getWidth()-1)
      val zs = sum(sum.getWidth()-1)
      // Add two positive numbers, get negative # (overflow) -> assign most positive #
      val posOFs = !xs & !ys & zs
      val posOF = Fill(sum.getWidth(),posOFs)
      // Add two negative numbers, get positive # (overflow) -> assign most negative #
      val negOFs = xs & ys & !zs
      val negOF = Fill(sum.getWidth(),negOFs)
      val noOFs = !negOFs & !posOFs
      val noOF = Fill(sum.getWidth(),noOFs)
      val max = Cat(Bits(0,width=1),Fill(sum.getWidth()-1,Bits(1,width=1)))
      val min = Cat(Bits(1,width=1),Fill(sum.getWidth()-1,Bits(0,width=1)))
      val result = ((posOF & max) | (negOF & min) | (noOF & sum)).toSInt
      val res = fromSInt(result,x.getWidth(),this.getFractionalWidth())  
      res.updateLimits() 
      res
      // Limits set to max/min by bitwidth
    //}
  }

  // Determine if difference will result in overflow; return worst case max/min of difference
  def checkDiffOverflow(a: Fixed, b: Fixed) : Tuple3[Boolean,Double,Double] = {
    val maxDiff = a.upperBound - b.lowerBound
    val minDiff = a.lowerBound - b.upperBound
    val maxInBits = if (a.maxBits > b.maxBits) a.maxBits else b.maxBits
    val minInBits = if (a.minBits < b.minBits) a.minBits else b.minBits
    var overflow = true
    if (maxDiff <= maxInBits && minDiff >= minInBits) overflow = false 
    (overflow,maxDiff,minDiff)
  }

  // "Wrapping" sub : Q(n_max).m doesn't handle overflow
  def - (b : Fixed) : Fixed = {
    checkAligned(b)
    val (overflow,maxDiff,minDiff) = checkDiffOverflow(this,b)
    val (x,y) = matchWidth(this,b)
    val diff = fromSInt(x-y, x.getWidth(), this.getFractionalWidth())
    diff.updateLimits(maxDiff,minDiff)
    diff
  }

  // Sub (max_width+1) operator where applicable i.e. integer width increases if overflow
  def -& (b : Fixed) : Fixed = {
    val (overflow,maxDiff,minDiff) = checkDiffOverflow(this,b)
    //if (!overflow) this - b
    //else{
      checkAligned(b)
      val (x,y) = matchWidth(this,b) 
      // Sign extend
      val j = Cat(x(x.getWidth()-1),x.toBits).toSInt
      val k = Cat(y(y.getWidth()-1),y.toBits).toSInt
      val diff = fromSInt(j-k,x.getWidth()+1,this.getFractionalWidth())
      diff.updateLimits(maxDiff,minDiff)
      diff
    //}   
  }

  // Sub saturate (if overflow) operator where applicable
  def -@ (b : Fixed) : Fixed = {
    val (overflow,maxDiff,minDiff) = checkDiffOverflow(this,b)
    //if (!overflow) this - b
    //else{
      checkAligned(b)
      val (x,y) = matchWidth(this,b) 
      // width max(wx,wy)
      val diff = x - y
      // Sign bits of x,y,z where x+y=z
      val xs = this(this.getWidth()-1)
      val ys = b(b.getWidth()-1)
      val zs = diff(diff.getWidth()-1)
      // pos - neg = neg (overflow) -> assign most positive #
      val posOFs = !xs & ys & zs
      val posOF = Fill(diff.getWidth(),posOFs)
      // neg - pos = pos (overflow) -> assign most negative #
      val negOFs = xs & !ys & !zs
      val negOF = Fill(diff.getWidth(),negOFs)
      val noOFs = !negOFs & !posOFs
      val noOF = Fill(diff.getWidth(),noOFs)
      val max = Cat(Bits(0,width=1),Fill(diff.getWidth()-1,Bits(1,width=1)))
      val min = Cat(Bits(1,width=1),Fill(diff.getWidth()-1,Bits(0,width=1)))
      val result = ((posOF & max) | (negOF & min) | (noOF & diff)).toSInt
      val res = fromSInt(result,x.getWidth(),this.getFractionalWidth())  
      res.updateLimits()
      res
      // Limits set to max/min bitwidth 
    //}
  }

  // Truncate to n fractional bits
  // For positive # abs. value decreases; for negative # abs. value increases
  override def $ (n: Int) : Fixed = {
    if (n < 0) ChiselError.error("Can't truncate to negative fractional bits.")
    val truncateAmount = this.getFractionalWidth()-n
    if (truncateAmount < 0) {
      ChiselError.error("Can't truncate (expand) to " + n + " fractional bits.")
      this
    }
    else if (truncateAmount == 0) this
    else{
      val out = fromSInt(this(this.getWidth()-1,truncateAmount).toSInt,this.getWidth()-truncateAmount,n)
      // For fewer fractional bits, the upper bound is possibly reduced i.e. 2^n - 2^(-m)
      out.updateLimits(this.upperBound,this.lowerBound,true)
      out
    }
  }
  
  // OR of two fixed point values (for mux construction)
  def ||| (b: Fixed) : Fixed = {
    checkAligned(b)
    if(this.getWidth() != b.getWidth()) ChiselError.error(this.getWidth() + " Width does not match " + b.getWidth())
    val out = fromSInt(this.toSInt | b.toSInt)
    val maxBound = if (this.upperBound > b.upperBound) this.upperBound else b.upperBound
    val minBound = if (this.lowerBound < b.lowerBound) this.lowerBound else b.lowerBound
    out.updateLimits(maxBound,minBound)
    out
  }
  

  
  // Multiply without implementing any saturation/overflow logic
  def * (b : Fixed) : Fixed = {
    this * (b,false)
  }
  
  // Multiply increasing the Bit Width as needed (except when inputs are Q0.x notation - then -1 * -1 overflow handled separately --> saturated if desired)
  def * (b : Fixed, handleOF: Boolean = false) : Fixed = {
    checkAligned(b)
    // Find maximum + and minimum - products
    val maxProd1 = this.upperBound * b.upperBound
    val maxProd2 = this.lowerBound * b.lowerBound
    val maxProd = if (maxProd1 > maxProd2) maxProd1 else maxProd2
    val minProd1 = this.upperBound * b.lowerBound
    val minProd2 = this.lowerBound * b.upperBound
    val minProd = if (minProd1 < minProd2) minProd1 else minProd2
    val absMaxProd = math.abs(maxProd)
    val absMinProd = math.abs(minProd)
    val absMax = if (absMaxProd > absMinProd) absMaxProd else absMinProd
    val intBitsSmall = math.log(absMax)/math.log(2)
    val intBitsUp = math.ceil(intBitsSmall)
    // Calculate integer and fractional widths of product as needed
    var prodIntWidth = if (intBitsSmall < 0) 0 else if (intBitsSmall < intBitsUp) intBitsUp.toInt else intBitsSmall.toInt + 1
    // Worst case Qn1.m1 * Qn2.m2 = Q(n1+n2).(m1+m2), but in this case, fractional widths are the same
    val prodFracWidth = this.getFractionalWidth()*2
    checkBounds(maxProd, prodFracWidth + prodIntWidth + 1, prodFracWidth)
    checkBounds(minProd, prodFracWidth + prodIntWidth + 1, prodFracWidth)
    // Saturate for -1 * -1 (since 1 can't be represented) when inputs are Q0.x
    //if (saturateFrac && prodIntWidth == 1 && this.getIntWidth() == 0 && b.getIntWidth() == 0){
    if (this.getIntWidth() == 0 && b.getIntWidth() == 0 && handleOF){
      val negOne = Cat(Bits(1,width=1),Fill(this.getWidth()-1,Bits(0,width=1))).toSInt
      val aEqNegOne = (this.toSInt === negOne)
      val bEqNegOne = (b.toSInt === negOne)
      val overflows = aEqNegOne & bEqNegOne
      val overflow = Fill(prodFracWidth+1,overflows).toSInt
      val maxFrac = Cat(Bits(0,width=1),Fill(prodFracWidth,Bits(1,width=1))).toSInt
      // Check for overflow
      val prod = (overflow & maxFrac) | (~overflow & (this.toSInt * b.toSInt))
      val out = fromSInt(prod, prodFracWidth + 1, prodFracWidth)
      out.updateLimits(maxProd,minProd)
      out
    }
    // -1 * -1 will give wrong value
    else if (this.getIntWidth() == 0 && b.getIntWidth() == 0 && !handleOF){
      val out = fromSInt(this.toSInt * b.toSInt, prodFracWidth + 1, prodFracWidth)
      out.updateLimits(maxProd,minProd)
      out
    }
    // Not Q0.x notation
    else{ 
      if (handleOF) throw new Exception ("Overflow correction for anything other than Q0.x currently not supported")
      val intBW = if (this.getIntWidth() > b.getIntWidth()) this.getIntWidth() else b.getIntWidth()
      // Integer bitwidth doesn't change
      val out = fromSInt(this.toSInt * b.toSInt, prodFracWidth + intBW + 1, prodFracWidth)
      //val out = fromSInt(this.toSInt *b.toSInt, prodFracWidth + prodIntWidth + 1, prodFracWidth)
      out.updateLimits(maxProd,minProd)
      out
    }
  }
  
    // Round (half up) to n fractional bits - compensates for chance of overflow i.e. at positive max value
  // Reduces bias as compared to truncation (bias still present at half-way pt)
  // Generally rounds by magnitude except at half pt: + rounds up; neg rounds to smaller magnitude
  override def $$ (n: Int) : Fixed = {
    if (n < 0) ChiselError.error("Can't truncate to negative fractional bits.")
    val truncateAmount = this.getFractionalWidth()-n
    if (truncateAmount < 0) {
      ChiselError.error("Can't truncate (expand) to " + n + " fractional bits.")
      this
    }
    else if (truncateAmount == 0) this
    else {
      val roundingBit = this(truncateAmount-1)
      val truncated = this $ n
      val resShort =  truncated.toUInt + roundingBit.toUInt
      val newWidth = truncated.getWidth()
      if (newWidth == 1) ChiselError.error("New width must be >1 for signed value.")
      // For fewer fractional bits, the upper bound is possibly reduced i.e. 2^n - 2^(-m) -> possible overflow with new fractional bit width
      //if ( this.upperBound >= (math.pow(2,this.getIntWidth())-math.pow(2,-n))){
        val newMax = Cat(Bits(0,width=1),Fill(newWidth-1,Bits(1,width=1)))
        val OFs = !truncated(truncated.getWidth()-1) & resShort(resShort.getWidth()-1)
        val OF = Fill(newWidth,OFs)
        // Overflow handling (only occurs when dealing w/ most positive truncated value i.e. switch sign bit from 0 to 1)
        val out = fromSInt(((OF & newMax) | (~OF & resShort)).toSInt,newWidth,n)
        out.updateLimits(truncated.upperBound + math.pow(2,-n) ,truncated.lowerBound)
        out
      //}
      
      //else{
      //  val out = fromSInt(resShort.toSInt,newWidth,n)
        // Rounding can add approx. 1 new LSB for positive values, lower bound stays the same
      //  out.updateLimits(this.upperBound + math.pow(2,-n) ,this.lowerBound)
      //  out
      //}
      
    }
  }
  
  


SATURATE FOR ADD
SATURATE FOR MULTIPLY: multiply: no grow (cut)
multiply grow
multiply saturate (if Q0.n notation) - separate into 2 functions
note for multiply fractional always grows (need separate trim)
  
  match width, but not match frac width: should be wrong if frac widths don't match???
}
*/





















 
  



