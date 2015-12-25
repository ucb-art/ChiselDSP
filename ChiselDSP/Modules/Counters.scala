/** Generator for different flavors of accumulators. 
  * Depending on type of Counter, some IO are not utilized.
  */

package ChiselDSP 
import Chisel._

/** Ctrl locations: 
  * External = Use external Ctrl signal
  * Internal = Use interal Ctrl signal (i.e. wrap when maxed out)
  * TieFalse = Fix Ctrl signal to false
  * TieTrue = Fix Ctrl signal to true
  */
abstract class CtrlLoc 
object External extends CtrlLoc
object Internal extends CtrlLoc
object TieFalse extends CtrlLoc
object TieTrue extends CtrlLoc

/** Count type:  
  * Up = always count up (count + inc)
  * Down = always count down (count - inc)
  * UpDown = count up/down (ctrl signal required)
  * UpMod = always count up, but mod with #
  */
abstract class CountType
object Up extends CountType
object Down extends CountType
object UpDown extends CountType
object UpMod extends CountType

/** Counter Generator parameters */
case class CountParams (
  countMax:   Int,                      // Upper limit of counter range
  incMax:     Int       = 1,            // Upper limit of increment range
  resetVal:   Int       = 0,            // Value on reset
  wrapCtrl:   CtrlLoc   = Internal,     // Location of wrap control signal
  changeCtrl: CtrlLoc   = External,     // Location of counter update control signal
  countType:  CountType = Up,           // Count type/direction
  customWrap: Boolean   = false,        // Whether custom wrap to value exists
  inputDelay: Int       = 0             // Keep track of accumulated delay until module inputs
){
  require (inputDelay >= 0, "Input delay must be non-negative")
  require (countMax >= 0, "Max counter value must be non-negative")
  require (resetVal >= 0 && resetVal <= countMax, "Counter reset should be [0,countMax]")
  require (incMax > 0 && incMax <= countMax, "Counter increment should be (0,countMax]")
  require (wrapCtrl != TieTrue, "Can't always wrap")
  require (changeCtrl == External || changeCtrl == TieTrue, "Either update on external signal or always update")  
  require (!((countType == UpDown || countType == Down) && (incMax > 1) && (!customWrap || wrapCtrl == Internal)), 
    "You must use a custom wrap condition and wrap to value if your counter delta is > 1"
    + " and you are possibly counting down")
  require (!(countType == Up && incMax > 1 && wrapCtrl == External && !customWrap),
    "When using an up counter with increment > 1, an external wrap condition cannot be used to trigger"
    + " counter to wrap to some __ internally defined value")
}

/** Counter control signals (I --> O can be passed through chain of counters) */
class CountCtrl (countParams: CountParams) extends IOBundle {
  val wrap = if (countParams.wrapCtrl == External) Some(DSPBool(INPUT)) else None
  val change = if (countParams.changeCtrl == External) Some(DSPBool(INPUT)) else None
  val reset = DSPBool(INPUT)
}

/** Counter IO */
class CountIO (countParams: CountParams) extends IOBundle {
  // Count up/down control signal
  val upDown = if (countParams.countType == UpDown) Some(DSPBool(INPUT)) else None
  // Counters usually increment by 1
  val inc = if (countParams.incMax != 1) Some(DSPUInt(INPUT,countParams.incMax)) else None
  // Counter wrap to value (up counters default wrap to 0)
  val wrapTo =  if (countParams.customWrap) Some(DSPUInt(INPUT,countParams.countMax)) else None
  // Counter default wrap condition is when count is maxed out (so need to know max)
  val max = {
    if (countParams.wrapCtrl == Internal && countParams.countType != UpMod) Some(DSPUInt(INPUT,countParams.countMax))
    else None
  }
  // n in x%n
  val modN = if (countParams.countType == UpMod) Some(DSPUInt(INPUT,countParams.countMax+1)) else None
  val out = DSPUInt(OUTPUT,countParams.countMax)
}

/** Counter template */
abstract class Counter(countParams: CountParams) extends DSPModule(inputDelay = countParams.inputDelay) {
  
  override val io = new CountIO(countParams)
  val iCtrl = new CountCtrl(countParams)
  val oCtrl = new CountCtrl(countParams).flip

  val inc = io.inc.getOrElse(DSPUInt(1))
  val max = io.max.getOrElse(DSPUInt(countParams.countMax))

  val eq0 = (io.out === DSPUInt(0))
  val eqMax = (io.out === max)
  
  val (upCustom, upCustomWrap) = Mod(io.out + inc, max + DSPUInt(1))
  val (modOut,overflow) = Mod(io.out + inc,io.modN.getOrElse(DSPUInt(0)))
  
  // Adapt wrap condition based off of type of counter if it isn't retrieved externally
  val wrap = countParams.wrapCtrl match {
    case Internal => {
      countParams.countType match {
        case UpDown => Mux(io.upDown.get, eq0, eqMax)
        case Down => eq0
        case Up => {
          // For >1 increments, custom wrap indicated by sum overflow on next count
          if (countParams.incMax > 1) upCustomWrap          
          else eqMax       
        }
        case UpMod => overflow
      }
    }
    case TieFalse => DSPBool(false)
    case TieTrue => DSPBool(true)
    case External => iCtrl.wrap.get
  }
 
  // Adapt wrap to value based off of type of counter if it isn't retrieved externally
  val wrapTo = {
    io.wrapTo.getOrElse(
      countParams.countType match {
        case UpDown => Mux(io.upDown.get,max, DSPUInt(0))
        case Down => max
        case _ => DSPUInt(0)
      }
    )
  }
  
  // If incrementing by 1 or using external wrap signals, add normally
  // But if incrementing by >1 and using internal wrap signals, do add mod (max + 1)
  val up = {
    if (countParams.incMax == 1 || (countParams.wrapCtrl == External && countParams.customWrap))
      (io.out + inc).shorten(countParams.countMax)
    else upCustom
  }
  
  val down = io.out - inc
  
  val nextInSeq = countParams.countType match {
    case UpDown => Mux(io.upDown.get,down,up)
    case Up => up
    case Down => down
    case UpMod => modOut
  }
  
  // When only internal wrap signals are used, note that mods already produce appropriately wrapped counter values
  val nextCount = {
    if (countParams.wrapCtrl == Internal && (countParams.countType == UpMod || 
       (countParams.countType == Up && countParams.incMax > 1 && !countParams.customWrap))) 
      nextInSeq
    else Mux(wrap,wrapTo,nextInSeq)
  }

  // Conditionally update (hold until update) or always update
  val newOnClk = countParams.changeCtrl match {
    case External => Mux(iCtrl.change.get,nextCount,io.out)
    case TieTrue => nextCount
  }

  val count = Mux(iCtrl.reset,DSPUInt(countParams.resetVal),newOnClk)
  io.out := count.reg()
  
  // When counters are chained, subsequent counter increments when current counter wraps
  if (countParams.changeCtrl == External) oCtrl.change.get := wrap & iCtrl.change.get
  if (countParams.wrapCtrl == External) oCtrl.wrap.get := wrap
  oCtrl.reset := iCtrl.reset
  
}

/////////////////////////////////////////////////////////////////////////////////////////////////

/** Counter Flavors */

/** Mod counter
  * count_new = {0 if wrapCond; else (count + inc) % modN }
  * oCtrl.change indicates if mod will wrap on the next cycle (count + inc > modVal)
  */
object ModCounter{
  def apply(countMax: Int, incMax: Int, inputDelay: Int = 0, nameExt: String = "") : Counter = {
    val countParams = CountParams(countMax = countMax, incMax = incMax, wrapCtrl = External,
                                  countType = UpMod, inputDelay = inputDelay)
    DSPModule(new ModCounter(countParams), nameExt)
  }
}
class ModCounter(countParams: CountParams) extends Counter(countParams)

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

