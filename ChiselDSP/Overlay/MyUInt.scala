/** MyUInt class customizations --
  * Uses max (range) instead of bitwidth.
  * Keeps track of operation ranges to achieve best bitwidth inference.
  * Expands # of useful operators.
  * All operators allow range overrides. 
  * TO DO: Some degree of auto-pipelining / keeping track of pipelining
  */
package ChiselDSP
import Chisel._

object MyUInt {

  /** Convert Bits to a MyUInt (upperbound specified w/ max) by reinterpreting the Bits */
  def apply(x:Bits, max:BigInt): MyUInt = chiselCast(x){MyUInt(x.dir,max)}

  /** Determine bitwidth needed for literal */
  def toBitWidth(x: BigInt): Int = {
    if (x == 0) 1
    else x.bitLength
  }
	
  /** Calculate signal max from bitwidth */
  def toMax(w: Int): BigInt = (BigInt(2) << w) - 1 
    
  /** Creates a MyUInt object from a constant BigInt (or Int casted to BigInt) */
  def apply(x: BigInt): MyUInt = {
    val res = Lit(x, toBitWidth(x)){MyUInt(NODIR, x)}
    res.updateLimits(x)
    res.asDirectionless
    res
  }
  
  /** Create a MyUInt obect with a specified IODirection and maximum allowable value */
  def apply(dir: IODirection, max: BigInt): MyUInt = {
    if (max < 0) throwException ("MyUInt must be non-negative!")
    apply_gen(dir,max)
  }
  
  /** Internally handles some edge cases i.e. when Chisel relies on node conversion,
    * width/range is not defined up front 
    */
  private def apply_gen(dir: IODirection, max: BigInt): MyUInt = {
    val res = new MyUInt()
    val updateBits = (max >= 0)
    res.create(dir, if (updateBits) toBitWidth(max) else max.toInt)
    if (updateBits) res.updateLimits(max)
    res
  }

}

class MyUInt extends Bits with MyNum[MyUInt] {

  /** Print MyUInt width */
  def printWidth() : String = (this.getWidth + "-bit uint, max = " + this.getMax)

  type T = MyUInt

  /** Set the value of this MyUInt */
  override def fromInt(x: Int): this.type = MyUInt(x).asInstanceOf[this.type]

  override def fromNode(n: Node): this.type = {
    val res = MyUInt.apply_gen(OUTPUT,-1).asTypeFor(n).asInstanceOf[this.type]
    n match {
      case l: Literal =>
        /* 
        if (l.isZ && Driver.minimumCompatibility > "2") {
          // Chisel3 compatibility - generic don't care UInts/Bits are deprecated.
          ChiselError.warning("General don't care MyUInts are deprecated. Please use BitPat().")
        }
        */
      case _ =>
    }
    res
  }
  
  /** Convert Bits to a MyUInt by reinterpreting the Bits. 
    * opMax specifies the range of the output as determined by the operation.
    * maxOverride allows the designer to use a smaller range (shorten bits appropriately).
    */
  private def toMyUInt(x: Bits, opMax: BigInt, maxOverride: BigInt) : MyUInt = {
    //if (maxOverride > opMax || maxOverride < 0) throwException("New MyUInt max must be smaller than current max and non-negative.")
    val max = if (maxOverride >= 0 && maxOverride < opMax) maxOverride else opMax
    MyUInt(x,max)
  }
  	
  private var max = BigInt(0)
  private var maxBits = BigInt(0)
  
  /** Accessor to gets range of MyUInt */ 
  def getMax(): BigInt = max
  
  /** Update the range of this MyUInt instance. Range can be increased until it exceeds
    * what is allowable by the MyUInt bitwidth, which is fixed @ creation.
    */
  private def updateLimits(x: BigInt): Unit = {
    maxBits = MyUInt.toMax(this.getWidth) 
    max = x.max(max)
    if (max > maxBits) {Warn("Warning, possible MyUInt overflow. Signals down the chain may be wrong."); maxBits}  
  }
  
  private var noUpdate = false  
  
  /** Marks the signal as being used to prevent future updates to its range.*/
  def used(): Unit = (noUpdate = true)   
  
  /** Returns the signal. Marks it as used */
  def doNothing() : MyUInt = {this.used(); this}
  
  /** Only allow a MyUInt to be updated if it hasn't been used or if the signal being assigned ot it
    * has a smaller range
    */
  override protected def colonEquals(that : Bits): Unit = {
    that match {
      case u: MyUInt => {
        if (this.noUpdate && (u.getMax > this.getMax)) 
          throwException("Previous lines of code have used signal. To ensure range consistency,"
            + "signal cannot be updated with signal of larger range. Move := earlier in code!")
        val (x,y) = matchWidth(this,u)
        super.colonEquals(toMyUInt(y,u.getMax,-1))
        this.updateLimits(u.getMax)
      }
      case _ => illegalAssignment(that)
    }
  }
  
  /** select ? this : 0 */
  def ? (select: MyBool): MyUInt = {
    this.used(); select.used()
    if (select.isLit) {
      if (select.isTrue) this
      else MyUInt(0)
    }
    else{
      val res = this & Fill(this.getWidth,select.toBool)
      toMyUInt(res,this.max,-1)
    }
  }
  
  /** Right shift n --> this/2^n */
  override def >> (n: Int) : MyUInt = {
    this.used()
    if (n < 0) throwException ("Shift amount must be non-negative")
    if (this.isLit) MyUInt(this.litValue() >> n)
    else {
      val res = {
        if (n >= this.getWidth) UInt(0,1) 
        else this(this.getWidth-1,n)
      }
      val opMax = this.max >> n
      toMyUInt(res,opMax,-1)
    }
  }
  
  /** Left shift n --> this*2^n */
  def << (n: Int) : MyUInt = {
    this.used()
    if (n < 0) throwException ("Shift amount must be non-negative")
    if (this.isLit) MyUInt(this.litValue() << n)
    else {
      val res = {
        if (n == 0) this
        else Cat(this,Fill(n,Bits(0,1)))
      }
      val opMax = this.max << n
      toMyUInt(res,opMax,-1)
    }
  }
  
  /** Variable right shift */
  def >> (n: MyUInt) : MyUInt = {
    this.used(); n.used()
    if (n.isLit) this >> n.litValue().toInt
    else {
      val res = this.toBits >> n.toBits
      toMyUInt(res,this.max,-1)
    }
  }
  
  /** Variable left shift */
  def << (n: MyUInt) : MyUInt = {
    this.used(); n.used()
    if (n.isLit) this << n.litValue().toInt
    else{
      val res = this.toBits << n.toBits
      val opMax = this.max << n.getMax.intValue 
      toMyUInt(res,opMax,-1)
    }
  }
  
  /** Delay n clock cycles */
  def pipe (n: Int) : MyUInt = {
    this.used()
    if(this.isLit) this 
    else toMyUInt(ShiftRegister(this,n),this.max,-1)
  }
  
  /** Register - for things like counters - no max override */
  def reg(): MyUInt = {
    this.used()
    if (this.isLit) this else toMyUInt(Reg(next = this),this.max,-1)
  }
  
  /** Match operator widths */
  private def matchWidth(x: MyUInt, y: MyUInt) : Tuple2[Bits,Bits] = {
    val diff = y.getWidth - x.getWidth
    val zro = Bits(0,1)
    if (diff > 0) (Cat(Fill(diff,zro),x), y) 
    else if (diff < 0) (x, Cat(Fill(-diff,zro),y)) 
    else (x, y)
  }  
  
  /** Bitwise-OR where output max assumes one input is 0 [i.e. for mux] */
  def /| (b: MyUInt) : MyUInt = {
    this.used(); b.used()
    if (this.isLit && this.litValue() == 0) b
    else if (b.isLit && b.litValue() == 0) this
    else{ 
      val (x,y) = matchWidth(this,b)
      val or = x | y
      val opMax =  b.getMax.max(this.max)   
      toMyUInt(or,opMax,-1)
    }
  }

  /** Add overflow -> wrap to max(this.getWidth,b.width) # of bits */
  def +% (b: MyUInt) : MyUInt = {
    this.used(); b.used()
    if (this.isLit && this.litValue() == 0) b
    else if (b.isLit && b.litValue() == 0) this
    else{
      val (x,y) = matchWidth(this,b)
      val sum = x+y
      val inMaxBits = MyUInt.toMax(x.getWidth)
      val opMax = inMaxBits.min(this.max + b.getMax)
      toMyUInt(sum,opMax,-1)
    }
  }
  
  /** Explicit add with bit growth 
    * Max determined by sum bitwidth = (this.getWidth,b.width) + 1
    */
  def +& (b: MyUInt) : MyUInt = {
    this.used(); b.used()
    val (x,y) = matchWidth(this,b)
    val zro = Bits(0,1)
    val sum = Cat(zro,x)+Cat(zro,y)
    toMyUInt(sum,MyUInt.toMax(x.getWidth+1),-1)
  }
  
  /** Add that determines optimal sum range, bitwidth */
  def + (b: MyUInt) : MyUInt = {
    this.used(); b.used()
    if (this.isLit && this.litValue() == 0) b
    else if (b.isLit && b.litValue() == 0) this
    else{
      val opMax = this.max + b.getMax
      toMyUInt((this +& b),opMax,-1)
    }
  }
  
  /** Subtract and wrap on negative result */
  def - (b: MyUInt) : MyUInt = {
    this.used(); b.used()
    if (b.isLit && b.litValue() == 0) this
    else{
      val (x,y) = matchWidth(this,b)
      val diff = x.toUInt-y.toUInt
      if ((this.max - b.getMax) < 0) Warn("Subtraction negative result will wrap.")
      toMyUInt(diff,this.max,-1)
    }
  }
  
  /** Multiply while determining optimal product bitwidth + range */
  def * (b: MyUInt) : MyUInt = {
    this.used(); b.used()
    if (this.isLit && this.litValue() == 0) MyUInt(0)
    else if (this.isLit && this.litValue() == 1) b
    else if (b.isLit && b.litValue() == 0) MyUInt(0)
    else if (b.isLit && b.litValue() == 1) this
    else {
      val res = this.toBits * b.toBits
      val opMax = this.max * b.getMax
      toMyUInt(res,opMax,-1)
    }
  }
  
  private def error(msg: String) : MyUInt = {throwException(msg); this}
  
  /** Base operators required by Num[T], Bits */
  def / (b: MyUInt) : MyUInt = error ("/ not allowed.")
  def % (b: MyUInt) : MyUInt = error ("% not allowed.")
  def unary_-() : MyUInt = error ("-this not allowed.")
  def <  (b: MyUInt): MyBool = {this.used(); b.used(); MyBool(newLogicalOp(b, "<"))}
  def <= (b: MyUInt): MyBool = {this.used(); b.used(); MyBool(newLogicalOp(b, "<="))}
  def >  (b: MyUInt): MyBool = {this.used(); b.used(); b < this}
  def >= (b: MyUInt): MyBool = {this.used(); b.used(); b <= this}
  
  def === (b: MyUInt): MyBool = {this.used(); b.used(); MyBool(this.toBits === b.toBits)}
  def =/= (b: MyUInt): MyBool = {this.used(); b.used(); MyBool(this.toBits != b.toBits)}
  
  /** Change INPUT to OUTPUT and OUTPUT to INPUT. NODIR stays the same. */
  override def flip: this.type = {
    dir match {
      case INPUT => dir = OUTPUT
      case OUTPUT => dir = INPUT
      case NODIR => dir = NODIR
    }
    this
  }
 
  /** Bit extraction @ index _bit_ */
  def extract(bit:Int): MyBool = {this.used(); MyBool(Extract(this,bit){Bool()})}
  def extract(bit:MyUInt): MyBool = {
    this.used(); bit.used(); 
    if (bit.isLit) MyBool(Extract(this,bit.litValue().toInt){Bool()})
    else MyBool(Extract(this,bit.toUInt){Bool()})
  }
  
  /** Extract range of bits, inclusive from hi to lo. optional max override.*/
  def extract(hi:Int, lo:Int): MyUInt = this.extract(hi,lo,-1)
  def extract(hi:Int, lo:Int, maxOverride:BigInt): MyUInt = {
    this.used()
    val res = Extract(this,hi,lo){UInt()}
    val bitMax = MyUInt.toMax(res.getWidth)
    toMyUInt(res,bitMax,maxOverride)
  }
  def extract(hi:MyUInt, lo:MyUInt): MyUInt = this.extract(hi,lo,-1)
  def extract(hi:MyUInt, lo:MyUInt, maxOverride:BigInt): MyUInt = {
    this.used(); hi.used(); lo.used(); 
    if (hi.isLit && lo.isLit) this.extract(hi.litValue().toInt,lo.litValue().toInt,maxOverride)
    else {
      val res = Extract(this,hi.toUInt,lo.toUInt){UInt()}
      val bitMax = MyUInt.toMax(res.getWidth)
      toMyUInt(res,bitMax,maxOverride)
    }
  }
  
  /** Shorten # of bits -- get rid of MSBs by forcing the generator to interpret a new (smaller, positive) max
    * CAREFUL: could eliminate useful bits!!! 
    */
  def shorten(newMax: BigInt): MyUInt = {
    toMyUInt(this,this.getMax,newMax)
  }
  
}
