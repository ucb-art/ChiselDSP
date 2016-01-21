/** Non base-2 support */

// TODO: Support mixed radix rather than just base n (needed for mod counters with + n where n > 1)

package ChiselDSP
import Chisel._

object MixRad {
  // TODO: Concatenate a generic list of Ints to Bits (each Int can have a different width)
  // Conversion between MixRad (val bases = List[Int]) and BaseN
  // Bits to Vec of varying widths
  // Constant to MixRad Vec ie 15 = 01,11,1 for Radix-4, Radix-2
  // 15 % 2 = 1 --> 15/2 = 7
  // 7 % 4 = 3 --> 7/4 = 1
  // 1 % 4 = 1
  // Mix radix sum
}

object BaseN {

  // Support a finite # of bases to make prime check, etc. easier
  val supportedBases = List(List(2,4),List(3),List(5),List(7),List(11))

  // TODO: Possibly reverse toIntList (?) and subsequent to index 0 = least significant digit
  // always check x, r, etc. are positive

  /** Converts a decimal representation of the number x into a List of
    * Ints representing the base-r interpretation of x (least significant digit on the right)
    */
  def toIntList(x: Int, r: Int): List[Int] = {
    if (x == 0) Nil else toIntList(x / r, r) :+ (x % r)
  }
  /** Zero pads List[Int] base-r representation */
  def toIntList(x: Int, r:Int, max:Int): List[Int] = {
    val intList = toIntList(x,r)
    val maxDigits = toIntList(max, r).length
    val fillDigits = (maxDigits - intList.length).max(0)
    val padding = List.fill(fillDigits)(0)
    padding ++ intList
  }

  /** Converts a decimal number x to an optionally padded List of DSPUInts
    * Note that the amount of padding is determined by the maximum number that should be represented */
  def toDSPUIntList(x: Int, r:Int, max: Int = -1): List[DSPUInt] = {
    val intList = if (max < 0) toIntList(x,r) else toIntList(x,r,max)
    intList.map(x => DSPUInt(x,r-1))
  }

  /** Converts a constant into BaseN Vec representation (least significant digit indexed with 0) */
  def apply(x: Int, r:Int): BaseN = apply(x,r,-1)
  def apply(x: Int, r:Int, max:Int): BaseN = {
    val temp = toDSPUIntList(x,r,max).reverse
    BaseN(temp,r)
  }

  // TODO: Check the concatenation preserves isLit

  /** Represent x in BaseN but stored as bits concatenated together (i.e. for memory)
    * Note that the bits are stored so that the least significant digit is the right-most to make
    * converting to BaseN Vec easier. Ex: toBits(14,3,242) --> 00 00 01 01 10
    */
  def toBits(x: Int, r:Int, max:Int = -1): Bits = {
    val digitWidth = DSPUInt.toBitWidth(r-1)
    val digits = toDSPUIntList(x,r,max).map(x => x.litValue())
    val lit = digits.tail.foldLeft(digits.head)((x,y) => (x << digitWidth) + y)
    UInt(lit,width=digits.length*digitWidth)
  }

  /** Converts a Bit representation of a number in base-r into a BaseN Vec representation with 0 indexing
    * the least significant digit
    */
  def apply(x: Bits, r:Int): BaseN ={
    val digitWidth = DSPUInt.toBitWidth(r-1)
    val numDigits = x.getWidth/digitWidth
    if (x.getWidth % digitWidth != 0) Error("# of bits should be a multiple of digit width")
    val temp = (0 until numDigits).map(i => DSPUInt(x((i+1)*digitWidth-1,i*digitWidth),r-1))
    BaseN(temp,r)
  }

  /** Create a new BaseN (to be assigned) specifying radix and max value */
  def apply(dir: IODirection, rad: Int, max: Int): BaseN = {
    val maxDigits = toIntList(max, rad).length
    val temp = (0 until maxDigits).map(i => DSPUInt(dir,rad-1))
    BaseN(temp,rad)
  }

  /** Converts list, etc. of DSPUInts to BaseN */
  def apply(elts: Iterable[DSPUInt], rad: Int): BaseN = {
    new BaseN(i => elts.head.cloneType,elts,rad=rad)
  }

  // TODO: Vec, BaseN Vec to Bits
  // TODO: intList to BaseN Vec, DSPUIntList to BaseN Vec, Vec to BaseN Vec (check element ranges < max radix)

}

// TODO: Ranging?, digit reverse with max digit specified, do I need an explicit mod?
// TODO: Better delay handling
/** BaseN type extends Vec */
class BaseN(gen: (Int) => DSPUInt, elts: Iterable[DSPUInt], val rad: Int) extends Vec(gen,elts){

  /** Clone type ! */
  override def cloneType: this.type = BaseN(elts.map(x => x.cloneType),rad).asInstanceOf[this.type]

  if (!BaseN.supportedBases.flatten.contains(rad)) Error("Radix not supported!")

  val digitWidth = DSPUInt.toBitWidth(rad-1)
  val bitWidth = digitWidth*length

  /** Check for same base + same digit length */
  def sameType(b: BaseN): Unit = {
    if (length != b.length) Error("BaseN Vec lengths must match!")
    if (rad != b.rad) Error("Operation can only be performed when both values have the same base!")
  }

  /** Helper function for handling radices that are powers of 2 */
  private def rad2NtoDSPUInt(): DSPUInt = {
    // TODO: Check Vec elements have the same delay, separate out 4^n1*2 case
    val max = DSPUInt.toMax(bitWidth)
    foreach{_.doNothing()}
    val res = tail.foldLeft(head.toUInt)((b,a) => Cat(a,b))
    val out = DSPUInt(res,max)
    out.passDelay(head,0)
  }

  /** Equality check */
  def === (b: BaseN): DSPBool = {
    sameType(b)
    val eqs = (this, b).zipped.map( _ === _ )
    eqs.tail.foldLeft(eqs.head)(_ & _)
  }

  /** Check that the BaseN value is a lit (all elements are lits) */
  def isBaseNLit(): Boolean = {
    val temp = map(_.isLit)
    temp.tail.foldLeft(temp.head)((x,y) => (x & y))
  }

  /** Sum (that always wraps) */
  def + (b: BaseN): BaseN = {
    sameType(b)
    // Any radix that is a power of 2 doesn't require mods (can rely on simple bit manipulation)
    if (rad % 2 == 0){
      // TODO: Handle + 1 Lit separately b/c BaseN doesn't track lits ? -- or make it track lits
      // Also if it's 4^n1*2, don't need the extra end bit
      val temp = rad2NtoDSPUInt +% b.rad2NtoDSPUInt
      val out = BaseN(temp,rad)
      out.foreach{_.passDelay(head,0)}
      out
    }
    else{
      // TODO: Optimize mode for fixed radix (doesn't need to be as generalized as for mixed radix)
      val r = DSPUInt(rad)
      // Intermediate sum digits (without carry)
      val tempSum = this.zip(b).map{case (x,y) => x + y}
      // Sum with base n carry chain (carryIn0 = 0)
      // Note that Mod takes carryIn_n + tempSum_n
      val resWithCarry = tempSum.tail.scanLeft(Mod(tempSum.head,r))((x,y) => {
        val carryIn = DSPUInt(x._2,1).passDelay(x._2,0)
        Mod( carryIn + y, r)
      })
      // Get sum digits
      val res = resWithCarry.map(x => x._1)
      BaseN(res,rad)
    }
  }

  // TODO: More generic than 4,2 (i.e. can be 8,4,2) -- rad % 2 == 0 && rad != 2
  // Also, should return MixedRad (?) rather than BaseN
  /** Converts Base 4 to Mixed Radix Base [4,...,4,2] with least significant base (2) indexed @ 0 */
  def toRad42(): BaseN = {
    if (rad != 4) Error("Conversion to mixed radix [4,...,4,2] requires that the input BaseN rad = 4")
    val intVal = rad2NtoDSPUInt
    // Special case for separating out radix 2
    val rad4Int = intVal >> 1
    // Base 4 representation should have digits of width 2
    val w = if (rad4Int.getWidth % 2 != 0) rad4Int.getWidth + 1 else rad4Int.getWidth
    val rad4IntPadded = DSPUInt(rad4Int,DSPUInt.toMax(w))
    val rad4s = BaseN(rad4IntPadded,rad)
    // Break out radix 2 (keep same width for Mux, etc. consistency)
    val rad2 = DSPUInt(intVal(0),rad-1)
    // Note: want to keep the same digit length
    val out = BaseN(rad2 :: rad4s.toList.init,rad = rad)
    if (out.length != length) Error("Conversion output should have the same # of digits")
    // TODO: Check delay for each element
    out.foreach{_.passDelay(head,0)}
    out
  }

  // TODO: Handle Iterable[T], Vec[T] in addition to BaseN
  /** Makes sure that reassignment only occurs when radices are the same */
  def <> (src: BaseN) : Unit = {
    sameType(src)
    super.<>(src)
  }
  def := (src: BaseN) : Unit = {
    sameType(src)
    super.:=(src)
  }

  /** Only keep the lowest numPrimeDigits # of (prime) digits and zero out the rest (equivalent to taking a Mod) */
  def maskWithMaxCheck(numPrimeDigits: DSPUInt): Tuple2[BaseN,DSPBool] = {
    if (rad %2 != 0 || rad == 2) {
      val (modOut,maxOut) = zipWithIndex.map {
        case (e,i) => {
          val activeDigit = numPrimeDigits > DSPUInt(i)
          val mod = Mux(activeDigit,e,DSPUInt(0,rad-1))
          val max = Mux(activeDigit,DSPUInt(rad-1),DSPUInt(0,rad-1))
          (mod,max)
        }
      }.unzip
      val res = BaseN(modOut,rad)
      val max = BaseN(maxOut,rad)
      val eqMax = res === max
      (res, eqMax)
    }
    else{
      // Stylize radix-4 as radix-2
      val intVal = rad2NtoDSPUInt.toBools
      val temp = intVal.zipWithIndex.map {
        case (e,i) => {
          val activeDigit = (numPrimeDigits > DSPUInt(i)).toBool
          val mod = Mux(activeDigit,e,Bool(false))
          val max = Mux(activeDigit,Bool(true),Bool(false))
          (mod.toUInt,max.toUInt)
        }
      }
      val (modOut,maxOut) = temp.tail.foldLeft(temp.head)( (b,a) => (Cat(a._1,b._1),Cat(a._2,b._2)))
      val eqMax = DSPBool(modOut === maxOut).passDelay(this.head,0)
      val out = BaseN(DSPUInt(modOut,DSPUInt.toMax(bitWidth)),rad)
      out.foreach{_.passDelay(head,0)}
      (out,eqMax)
    }
  }

  /** Pad Base N representation to numDigits # of digits */
  def padTo(numDigits: Int): BaseN = {
    val padLength = (numDigits-length).max(0)
    val pad = Vec(padLength,DSPUInt(0,max = rad-1))
    // Least significant digit indexed by 0
    BaseN(this ++ pad,rad = rad)
  }

  // TODO: For increments of 1, can check if the previous value was maxed by looking at ANDing all of the carry outs...,
  // Also, the above Max Check will not work for [9,...,9,3], etc.

}