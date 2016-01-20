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
  def apply(x: Int, r:Int, max:Int = -1): BaseN = {
    val temp = toDSPUIntList(x,r,max).reverse
    new BaseN(i => temp.head.cloneType,temp,rad=r)
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
    new BaseN(i => temp.head.cloneType,temp,rad=r)
  }

  /** Create a new BaseN (to be assigned) specifying radix and max value */
  def apply(dir: IODirection, rad: Int, max: Int): BaseN = {
    val maxDigits = toIntList(max, rad).length
    val temp = (0 until maxDigits).map(i => DSPUInt(dir,rad-1))
    new BaseN(i => temp.head.cloneType,temp,rad=rad)
  }

  // TODO: Vec, BaseN Vec to Bits
  // TODO: intList to BaseN Vec, DSPUIntList to BaseN Vec, Vec to BaseN Vec (check element ranges < max radix)

}

// TODO: Ranging?, digit reverse with max digit specified, do I need an explicit mod?
// TODO: Better delay handling
/** BaseN type extends Vec */
class BaseN(gen: (Int) => DSPUInt, elts: Iterable[DSPUInt], val rad: Int) extends Vec(gen,elts){

  val digitWidth = DSPUInt.toBitWidth(rad-1)
  val bitWidth = digitWidth*length

  /** Helper function for handling radices that are powers of 2 */
  private def rad2NtoDSPUInt(): DSPUInt = {
    // TODO: Check Vec elements have the same delay, separate out 4^n1*2 case
    val max = DSPUInt.toMax(bitWidth)
    val temp = reverse
    val res = temp.tail.foldLeft(temp.head.toUInt)((x,y) => Cat(x,y))
    val out = DSPUInt(res,max)
    out.passDelay(temp.head,0)
  }

  /** Equality check */
  def === (b: BaseN): DSPBool = {
    if (length != b.length) Error("BaseN Vec lengths must match!")
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
    if (rad != b.rad) Error("The two signals being summed must use the same radix!")
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
      new BaseN(i => res.head.cloneType,res,rad=rad)
    }
  }

  // TODO: More generic than 4,2 (i.e. can be 8,4,2) -- rad % 2 == 0 && rad != 2
  // Also, should return MixedRad rather than Vec
  /** Converts Base 4 to Mixed Radix Base [4,...,4,2] with least significant base (2) indexed @ 0 */
  def toRad42(): Vec[DSPUInt] = {
    if (rad != 4) Error("Conversion to mixed radix [4,...,4,2] requires that the input BaseN rad = 4")
    val intVal = rad2NtoDSPUInt
    // Special case for separating out radix 2
    val rad4Int = intVal >> 1
    // Base 4 representation should have digits of width 2
    val w = if (rad4Int.getWidth % 2 != 0) rad4Int.getWidth + 1 else rad4Int.getWidth
    val rad4IntPadded = DSPUInt(rad4Int,DSPUInt.toMax(w))
    val rad4s = BaseN(rad4IntPadded,rad)
    // Break out radix 2
    val rad2 = DSPUInt(intVal(0),1)
    val out = Vec(rad2 :: rad4s.toList)
    // TODO: Check delay for each element
    out.foreach{_.passDelay(head,0)}
    out
  }

  /** Only keep the lowest numDigits # of digits and zero out the rest (equivalent to taking a Mod) */
  def mask(numDigits: DSPUInt): BaseN = {
    val res = zipWithIndex.map{case (e,i) => {
      Mux(numDigits > DSPUInt(i),e,DSPUInt(0,max = rad-1))
    }}
    new BaseN(i => res.head.cloneType,res,rad=rad)
  }

}