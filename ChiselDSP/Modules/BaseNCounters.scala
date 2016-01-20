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
  val (out,eqMax) = Reg(count).maskWithMaxCheck(io.primeDigits)
  io.out := out

  oCtrl.reset := iCtrl.reset
  // Next counter's update might depend on this counter looping around from the maximum
  oCtrl.change := eqMax & iCtrl.change

}

/////////////////////////////////////////////////////////////////////////////////////////////////

/** Counter Flavors */

/** Base N Increment (+1) Counter */
object BaseNIncCounter{
  def apply(rad: Int, maxCoprime: Int, inputDelay: Int = 0, nameExt: String = "") : BaseNIncCounter = {
    val p = BaseNCountParams(rad = rad, maxCoprime = maxCoprime, inDelay = inputDelay)
    DSPModule(new BaseNIncCounter(p), nameExt)
  }
}
class BaseNIncCounter(p: BaseNCountParams) extends BaseNCounter(p)

/** Base N Accumulator (runtime-dependent increments) with External Wrap */
object BaseNAccWithWrap{
  def apply(rad: Int, maxCoprime: Int, inputDelay: Int = 0, nameExt: String = "") : BaseNAccWithWrap = {
    val p = BaseNCountParams(rad = rad, maxCoprime = maxCoprime, inDelay = inputDelay,
                             incMax = maxCoprime-1, externalWrap = true)
    DSPModule(new BaseNAccWithWrap(p), nameExt)
  }
}
class BaseNAccWithWrap(p: BaseNCountParams) extends BaseNCounter(p)

/*
  DEFAULTS
  rad: Int,                       // Radix
  maxCoprime: Int,                // Dictates maximum count value (maxCoprime-1)
  incMax: Int = 1,                // Largest increment (if 1, then use fixed increment)
  externalWrap: Boolean = false,  // Additional wrapping condition supported?
  inDelay: Int = 0                // Pipeline delay associated with inputs
*/
