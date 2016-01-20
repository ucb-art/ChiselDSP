package ChiselDSP
import Chisel._

// TODO: Make a Mixed Radix Counter

/** Base N counter parameters */
case class BaseNCountParams (
  rad: Int,                       // Radix
  maxCoprime: Int,                // Dictates maximum count value (maxCoprime-1)
  incMax: Int = 1,                // Largest increment (if 1, then use fixed increment)
  externalWrap: Boolean = false,  // Additional wrapping condition supported?
  inDelay: Int = 0                // Pipeline delay associated with inputs

)

/** IO for Base N counter */
class BaseNCountIO (p: BaseNCountParams) extends IOBundle {
  // Use constant 1 if it's just your standard + 1 counter (ignore inc input)
  val inc = if (p.incMax > 1) Some(BaseN(INPUT, rad = p.rad, max = p.maxCoprime-1)) else None
  val out = BaseN(OUTPUT, rad = p.rad, max = p.maxCoprime-1)
  // Use smallest prime base possible
  // TODO: Support more bases
  val primeBase = if (p.rad % 2 == 0) 2 else p.rad
  // Get the maximum number of prime base digits of the counter output
  val maxDigits = BaseN.toIntList(p.maxCoprime-1,primeBase).length
  // # of digits that contain part of the actual counter value (dictates mod result)
  val primeDigits = DSPUInt(INPUT,maxDigits)
}

/** Additional control signals for the Base N counter */
class BaseNCountCtrl (p: BaseNCountParams) extends IOBundle {
  val change = DSPBool(INPUT)
  val reset = DSPBool(INPUT)
  val wrap = if (p.externalWrap) Some(DSPBool(INPUT)) else None
}

/** Counterc that accumulates in Base N notation */
abstract class BaseNCounter (p: BaseNCountParams) extends DSPModule (inputDelay = p.inDelay) {

  override val io = new BaseNCountIO(p)
  val iCtrl = new BaseNCountCtrl(p)
  val oCtrl = new BaseNCountCtrl(p).flip

  val inc = io.inc.getOrElse( BaseN(1, p.rad, p.maxCoprime-1) )

  val zero = BaseN(0, p.rad, p.maxCoprime-1)
  // Get mod sum value
  val nextInSeq = io.out + inc
  val nextCount = if (p.externalWrap) Mux(iCtrl.wrap.get,zero,nextInSeq) else nextInSeq
  val newOnClk = Mux(iCtrl.change,nextCount,io.out)
  val count = Mux(iCtrl.reset,zero,newOnClk)
  // TODO: Check critical path with maskWithMaxCheck
  val (out,changeOut) = Reg(count).maskWithMaxCheck(io.primeDigits)
  io.out := out

  oCtrl.reset := iCtrl.reset
  // Next counter's update might depend on this counter looping around from the maximum
  oCtrl.change := changeOut

















}



/////////////////////////////////////////////////////////////////////////////////////////////////

/** Counter Flavors */

/** Mod counter
  * count_new = {0 if wrapCond; else (count + inc) % modN }
  * oCtrl.change indicates if mod will wrap on the next cycle (count + inc > modVal)
  */
/*object ModCounter{
  def apply(countMax: Int, incMax: Int, inputDelay: Int = 0, nameExt: String = "") : Counter = {
    val countParams = CountParams(countMax = countMax, incMax = incMax, wrapCtrl = External,
      countType = UpMod, inputDelay = inputDelay)
    DSPModule(new ModCounter(countParams), nameExt)
  }
}
class ModCounter(countParams: CountParams) extends Counter(countParams)
*/
/*
  DEFAULTS
  countMax:   Int,                      // Upper limit of counter range
  incMax:     Int       = 1,            // Upper limit of increment range
  resetVal:   Int       = 0,            // Value on reset
  wrapCtrl:   CtrlLoc   = Internal,     // Location of wrap control signal
  changeCtrl: CtrlLoc   = External,     // Location of counter update control signal
  countType:  CountType = Up,           // Count type/direction
  customWrap: Boolean   = false,        // Whether custom wrap to value exists
  inputDelay: Int       = 0             // Keep track of accumulated delay until module inputs
*/

