package ChiselDSP
import Chisel._

// TODO: Make a Mixed Radix Counter

/** Base N counter parameters */
case class BaseNCountParams (
  rad: Int,                       // Radix
  maxCoprime: Int,                // Dictates maximum count value (maxCoprime-1)
  incMax: Int = 1,                // Largest increment (if 1, then use fixed increment)
  externalWrap: Boolean = false,  // Additional wrapping condition supported?
  inDelay: Int = 0,               // Pipeline delay associated with inputs
  clkEn: Boolean = true,          // Whether additional enable is needed for slow clock support
  change: Boolean = true          // Whether additional enable/change (i.e. update on previous counter max) is needed

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
  val en = if (p.clkEn) Some(DSPBool(INPUT)) else None
  val change = if(p.change) Some(DSPBool(INPUT)) else None
  val reset = DSPBool(INPUT)
  val wrap = if (p.externalWrap) Some(DSPBool(INPUT)) else None
  val isMax = DSPBool(OUTPUT)
}

/** Counter that accumulates in Base N notation: clock ratio is used for slow/fast clock (i.e.
  * slow clock is 2x slower than nominal clock --> clkRatio = 2
  */
abstract class BaseNCounter (p: BaseNCountParams, clkRatio:Int) extends DSPModule (inputDelay = p.inDelay) {

  if (clkRatio < 1) Error("Clock ratio must be >= 1")
  // For reference
  val rad = p.rad

  override val io = new BaseNCountIO(p)
  val ctrl = new BaseNCountCtrl(p)

  val inc = io.inc.getOrElse( BaseN(1, p.rad, p.maxCoprime-1) )

  val zero = BaseN(0, p.rad, p.maxCoprime-1)
  // Get mod sum value
  val nextInSeq = io.out + inc
  val nextCount = if (p.externalWrap) Mux(ctrl.wrap.get,zero,nextInSeq) else nextInSeq
  val newOnClk = Mux(ctrl.change.getOrElse(DSPBool(true)) & ctrl.en.getOrElse(DSPBool(true)),nextCount,io.out)
  val count = Mux(ctrl.reset,zero,newOnClk)
  // TODO: Check critical path with maskWithMaxCheck
  val (out,eqMax) = count.maskWithMaxCheck(io.primeDigits)
  io.out := RegNext(out)

  // Next counter's update might depend on this counter looping around from the maximum
 ctrl.isMax := Pipe(RegNext(eqMax),clkRatio-1)

}

/////////////////////////////////////////////////////////////////////////////////////////////////

/** Counter Flavors */

/** Base N Increment (+1) Counter */
object BaseNIncCounter{
  def apply(rad: Int, maxCoprime: Int, clkRatio:Int, inputDelay: Int = 0, nameExt: String = "") : BaseNIncCounter = {
    val p = BaseNCountParams(rad = rad, maxCoprime = maxCoprime, inDelay = inputDelay)
    DSPModule(new BaseNIncCounter(p,clkRatio), nameExt)
  }
}
class BaseNIncCounter(p: BaseNCountParams, clkRatio:Int) extends BaseNCounter(p,clkRatio)

/** Base N Accumulator (runtime-dependent increments) with External Wrap */
object BaseNAccWithWrap{
  def apply(rad: Int, maxCoprime: Int, clkRatio: Int, inputDelay: Int = 0, nameExt: String = "") : BaseNAccWithWrap = {
    val p = BaseNCountParams(rad = rad, maxCoprime = maxCoprime, inDelay = inputDelay,
                             incMax = maxCoprime-1, externalWrap = true, change = false)
    DSPModule(new BaseNAccWithWrap(p,clkRatio), nameExt)
  }
}
class BaseNAccWithWrap(p: BaseNCountParams, clkRatio:Int) extends BaseNCounter(p,clkRatio)

/*
  DEFAULTS
  rad: Int,                       // Radix
  maxCoprime: Int,                // Dictates maximum count value (maxCoprime-1)
  incMax: Int = 1,                // Largest increment (if 1, then use fixed increment)
  externalWrap: Boolean = false,  // Additional wrapping condition supported?
  inDelay: Int = 0,               // Pipeline delay associated with inputs
  clkEn: Boolean = true,          // Whether additional enable is needed for slow clock support
  change: Boolean = true          // Whether additional enable/change (i.e. update on previous counter max) is needed
*/
