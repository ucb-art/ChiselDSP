/** Base for new ChiselDSP types */
// TODO: Scaladoc
  
package ChiselDSP
import Chisel._
import scala.collection.mutable.Map

/** User override for whether to check delay */
object CheckDelay {
  private var check = true
  def on() {check = true}
  def off() {check = false}
  def get() : Boolean = check
}

/** Additional operations for Fixed in Qn.m notation (and therefore DSPDbl) */
abstract class DSPQnm[T <: DSPBits[T]] extends DSPNum[T] {

  // TODO: overflow handling

  /** Shorten fixed-point integer width to save hardware resources (doesn't act on DSPDbl) */
  def shortenTo(intWidth: Int) = this.asInstanceOf[T]

  /** Truncate to n bits */
  def $ (n: Int) = this.asInstanceOf[T]
  /** Round to n bits */
  def $$ (n: Int, of: OverflowType) = this.asInstanceOf[T]
  def $$ (n: Int) : T = this $$ (n, of = Wrap)
  /** Get integer portion as DSPFixed */
  def toInt(): DSPFixed = toInt(r = Truncate)
  def toInt(r: TrimType): DSPFixed

  def Q : String
}

/** Allow numeric operations */
abstract class DSPNum[T <: DSPBits[T]] extends DSPBits[T] {

  /** Don't allow non-2^n divides (not synthesizable on FPGA -- designer should think carefully!) */
  private[ChiselDSP] def /  (b: T): T = error("/ not allowed.").asInstanceOf[T]
  final def %  (b: T): T = error("% not allowed.").asInstanceOf[T]
  
  /** Add + subtract and different flavors (to be overridden) */
  def +  (b: T): T                // determines optimal # bits
  def +& (b: T): T = this + b     // (should) grow bit (except for DSPDbl)
  def +% (b: T): T = this + b     // (should) wrap (except for DSPDbl)
  def -  (b: T): T                // determines optimal # bits
  def -& (b: T): T = this - b     // (should) grow bit (except for DSPDbl)
  def -% (b: T): T = this - b     // (should) wrap (except for DSPDbl)
  
  def unary_-(): T = error("-x not allowed.").asInstanceOf[T]
  
  def *  (b: T): T
  
  /** Bit shifts as alternatives to explicit multiply,divide by 2^n */
  def << (n: Int): T
  def >> (n: Int): T
  def << (n: DSPUInt): T
  def >> (n: DSPUInt): T
  
  /** Numeric comparison */
  def <  (b: T): DSPBool
  def <= (b: T): DSPBool
  def >  (b: T): DSPBool
  def >= (b: T): DSPBool
  
  /** Shorthand for getting MSB */
  final def MSB(): DSPBool = {
    val out = DSPBool(this(getWidth-1))
    out.updateGeneric(this)
  }
  
  /** Sign of signal (needs overriding for DSPUInt) */
  def sign(): DSPBool = MSB()
  
  /** Return the absolute value of this # with the same type */
  def abs() : T = Mux(sign(),(-this).asInstanceOf[T],this.asInstanceOf[T])
  
  /** Return the min of 2 numbers */
  def min(b: T): T = Mux(this < b, this.asInstanceOf[T], b)
  /** Return the max of 2 numbers */
  def max(b: T): T = Mux(this < b, b, this.asInstanceOf[T])
  
}

/** ChiselDSP type info (associated with each signal)
  * rangeBits = max range allowed by signal bitwidth
  */
case class Info (
  var range:Map[String,BigInt]  = Map("min" -> BigInt(0), "max" -> BigInt(0)),
  var rangeBits:Map[String,BigInt]  = Map("min" -> BigInt(0), "max" -> BigInt(0)),
  var isUsed: Boolean = false,
  var isAssigned: Boolean = false,
  var dly:Int = 0
)

/** Custom Bits with type info */
abstract class DSPBits [T <: DSPBits[T]] extends Bits {

  /** Error out with message */
  final def error(msg: String) : this.type = {Error(msg); this}

  /** Width + other useful debug info */
  def infoString(): String = (getWidth + " bits")

  /** Info associated with signal, initialize tracked signal pipe delay on a per module basis */
  private var info = Info(dly = Module.current.asInstanceOf[DSPModule].inputDelay)

  /** Marks the signal as being used to prevent invalid future updates */
  final protected def use() {info.isUsed = true} 
  final def isUsed() : Boolean = getInfo.isUsed
  /** Marks that node has been assigned */
  final private[ChiselDSP] def assign(): T =  {
    info.isAssigned = true
    this.asInstanceOf[T]
  }
  final def isAssigned() : Boolean = getInfo.isAssigned
  /** Returns the signal. Marks it as used */
  final def doNothing() : this.type = {use(); this}
 
  /** Update ranging [min,max] -- subclasses need to compute rangeBits first */
  final protected def setRange(range: (BigInt,BigInt)): Unit = {
    if (range._1 > range._2) error("Range min must be <= max")
    info.range("max") = if(isAssigned) range._2.max(getInfo.range("max")) else range._2
    info.range("min") = if(isAssigned) range._1.min(getInfo.range("min")) else range._1
    if (getInfo.range("max") > getInfo.rangeBits("max")) {
      Warn("Warning, possible max > max bits overflow. Signals down the chain may be wrong.")
      info.range("max") = getInfo.rangeBits("max")
    }
    if (getInfo.range("min") < getInfo.rangeBits("min")) {
      Warn("Warning, possible min < min bits overflow. Signals down the chain may be wrong.")
      info.range("min") = getInfo.rangeBits("min")
    }
    if (getInfo.range("min") > getInfo.rangeBits("max")) {
      Warn("Warning, possible min > max bits overflow. Signals down the chain may be wrong.")
      info.range("min") = getInfo.rangeBits("max")
    }
    if (getInfo.range("max") < getInfo.rangeBits("min")) {
      Warn("Warning, possible max < min bits overflow. Signals down the chain may be wrong.")
      info.range("max") = getInfo.rangeBits("min")
    }

  }
  
  /** Get range */
  final def getRange():List[BigInt] = List(getInfo.range("min"),getInfo.range("max"))
  
  /** Returns range [min,max] as string */
  final protected def rangeString(fracWidth:Int = 0): String = {
    val (min,max) = (getInfo.range("min"),getInfo.range("max"))
    if (fracWidth == 0) "[" + min + "," + max + "]"
    else "[" + DSPFixed.toDouble(min,fracWidth) + "," + DSPFixed.toDouble(max,fracWidth) + "]"
  }
  
  /** Update ranging as determined by bitwidth + data type [min,max] */
  final protected def setRangeBits(range: (BigInt,BigInt)): Unit = {
    info.rangeBits("min") = range._1
    info.rangeBits("max") = range._2
  }

  /** Get tracked pipe delay */
  final def getDelay(): Int = getInfo.dly

  /** Get all meta info */
  final def getInfo(): Info = info.copy()

  /** Copy info */
  final private[ChiselDSP] def copyInfo[T <: DSPBits[_]](that: T) : T = {
    info = that.getInfo
    this.asInstanceOf[T]
  }

  /** Pass (range, etc.) info of input to output [this] -- DOES NOT PASS DELAY. */
  final protected def passThrough[T <: DSPBits[_]](in: T): T = {
    copyInfo(in)
    info.isUsed = false
    in.use()
    assign()
    this.asInstanceOf[T]
  }

  /** Pass delay of input + offset to output [this] -- input/output types don't need to match */
  final private[ChiselDSP] def passDelay[U <: DSPBits[_]](in:U, offset: Int = 0): T = {
    info.dly = in.getDelay + offset
    this.asInstanceOf[T]
  }
  
  /** Updates info for single in -> out regardless of types i.e. bit extraction [no ranging info] */ 
  final private[ChiselDSP] def updateGeneric[U <: DSPBits[_]](that: U): T = {
    that.use()
    assign()
    passDelay(that)
    this.asInstanceOf[T]
  }
  
  /** Pass info of input to output (2 inputs -> 1 output [this]) -- DOES NOT PASS RANGE */
  final private[ChiselDSP] def pass2to1[U <: DSPBits[_],V <: DSPBits[_]](in1: U, in2: V) : T = {
    in1.use(); in2.use()
    assign()
    val someLit = in1.isLit || in2.isLit
    if (in1.getDelay != in2.getDelay && !someLit) {
      if (CheckDelay.get)
        error("Operator inputs must have the same delay. Delays are " + in1.getDelay + ", " + in2.getDelay)
      // When you want to override delay check, if the delays aren't the same, take the min value
      else info.dly = in1.getDelay.min(in2.getDelay)
    }
    else info.dly = (if (!in1.isLit) in1 else in2).getDelay
    this.asInstanceOf[T]
  }
  
  /** Pass info of input to output (3 inputs -> 1 output [this]) -- DOES NOT PASS RANGE */
  final private[ChiselDSP] def pass3to1[U <: DSPBits[_],V <: DSPBits[_], W <: DSPBits[_]](in1: U, in2: V, in3: W): T =
  {
    in1.use(); in2.use(); in3.use()
    assign()
    val someLit12 = in1.isLit || in2.isLit
    val someLit23 = in2.isLit || in3.isLit
    val someLit13 = in1.isLit || in3.isLit
    if ((in1.getDelay != in2.getDelay && !someLit12) ||
        (in2.getDelay != in3.getDelay && !someLit23) ||
        (in1.getDelay != in3.getDelay && !someLit13)) {
      if (CheckDelay.get) error("Operator inputs must have the same delay. Delays are " + in1.getDelay + ", "
                                + in2.getDelay + ", " + in3.getDelay)
      else info.dly = List(in1,in2,in3).map(_.getDelay).min
    }
    else info.dly = (if (!in1.isLit) in1 else if (!in2.isLit) in2 else in3).getDelay
    this.asInstanceOf[T]
  }

  /** Performs checks and info updates with reassignment */
  final protected def reassign(that: T) {
    val thisDly = getDelay
    var dlyNoUpdate = false
    if ((isAssigned || isUsed) && (thisDly != that.getDelay) && !that.isLit) {
      if (CheckDelay.get) error("Delays of L (" + thisDly + "), R (" + that.getDelay
                                + ") in L := R should match if L was previously assigned or used.")
      // When you want to override delay check, if the delays aren't the same, don't update delay
      else dlyNoUpdate = true
    }
    if (isUsed && (that.getRange.max > getRange.max || that.getRange.min < getRange.min)){
      error("Previous lines of code have used L in L := R. To ensure range consistency, "
            + "L cannot be updated with an R of wider range. Move := earlier in the code!")
    }
    updateLimits(List2Tuple(that.getRange))
    updateGeneric(that)
    if (that.isLit || dlyNoUpdate) info.dly = thisDly
  }

  /** Handles how range is updated */
  private[ChiselDSP] def updateLimits(range: (BigInt,BigInt)) : Unit

  /** Change INPUT to OUTPUT and OUTPUT to INPUT. NODIR stays the same. */
  final override def flip: this.type = {
    dir match {
      case INPUT => dir = OUTPUT
      case OUTPUT => dir = INPUT
      case NODIR => dir = NODIR
    }
    this
  }

  /** Delay n clock cycles */
  final def pipe (n: Int, en: DSPBool = DSPBool(true)): T = {
    val res = {
      if (isLit || n == 0) this
      else {
        en.use()
        val temp = ShiftRegister(this,n,en.toBool).toBits
        val out = chiselCast(temp){this.cloneType}
        out.passThrough(this)
        out.passDelay(this,n)
      }
    }
    res.asInstanceOf[T]
  }

  /** Register that keeps track of additional info */
  final def reg(clock: Clock = null): T = {
    val res = {
      if (isLit) this
      else {
        val temp = (if (clock == null) RegNext(this) else RegNext(this,clock)).toBits
        val out = chiselCast(temp){this.cloneType}
        // Delay = 0 because we only care about explicit pipe delays
        out.passThrough(this)
      }
    }
    res.asInstanceOf[T]
  }

  /** Equality check */
  def === (b: T): DSPBool = {
    val out = DSPBool(this.toBits === b.toBits)
    out.pass2to1(this,b)
  }
  
  /** Inequality check */
  def =/= (b: T): DSPBool = {
    val out = DSPBool(this.toBits =/= b.toBits)
    out.pass2to1(this,b)
  }
  def != (b: T): DSPBool = (this =/= b)
  
  /** Select function: s = true -> this; else 0 */
  def ? (s: DSPBool) : T
  /** Custom bitwise or for muxing on type T */
  def /| (b: T) : T
  def | (b: T) : T = this /| b

  /** Enforce that bitwise operations return bits to explicitly destroy info */
  
  private val warn = " Additional info like ranges, delays, etc. will be lost."
  private val castMsg = "Please cast to Bits (x.toBits) before performing "
  
  /** Bitwise negate */
  final override def unary_~(): this.type = error(castMsg + "bitwise negate." + warn)
  /** Bitwise and */
  final override def & (b: Bits): this.type = error(castMsg + "bitwise and. " + warn)
  /** Bitwise or */
  final override def | (b: Bits): this.type = error(castMsg + "bitwise or. " + warn)                                 
  /** Bitwise xor */
  final override def ^ (b: Bits): this.type = error(castMsg + "bitwise xor. " + warn) 
  /** Shift left (unsigned) */
  final override def << (b: UInt): this.type = error("Please use DSPUInt or Int for left shift amount.")
  /** Cat bits */
  final override def ##[T <: Data](right: T): this.type = error(castMsg + "bit concatenation. " + warn)
  /** Set bit 'off' */
  final override def bitSet(off: UInt, dat: UInt): this.type = error(castMsg + "manual bit sets. " + warn)

  /** Pass parameters on assignment <> or := */
  private[ChiselDSP] def assign (b: T): T
  /** Bulk connections also pass parameters */
  override def <>(src: Node) : Unit = {
    val b = src.asInstanceOf[T]
    if (isAssigned && !b.isAssigned) b <> this
    else if (isAssigned && b.isAssigned) error ("Cannot bulk assign signal that was previously assigned.")
    else super.<>(assign(b))
  }

  // TODO: Check bundle := passes params properly

}
