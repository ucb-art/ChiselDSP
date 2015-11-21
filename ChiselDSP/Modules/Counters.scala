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
  customWrap: Boolean   = false         // Whether custom wrap to value exists
){
  require (countMax >= 0, "Max counter value must be non-negative")
  require (resetVal >= 0 && resetVal <= countMax, "Counter reset should be [0,countMax]")
  require (incMax > 0 && incMax <= countMax, "Counter increment should be (0,countMax]")
  require (wrapCtrl != TieTrue, "Can't always wrap")
  require (changeCtrl == External || changeCtrl == TieTrue, "Either update on external signal or always update")  
  require (!((countType == UpDown || countType == Down) && (incMax > 1) && (!customWrap || wrapCtrl == Internal)), 
    "You must use a custom wrap condition and wrap to value if your counter delta is > 1 and you are possibly counting down")
  require (!(countType == Up && incMax > 1 && wrapCtrl == External && !customWrap),
    "When using an up counter with increment > 1, an external wrap condition cannot be used to trigger counter to wrap to some __ internally defined value")
}

/** Counter control signals (I --> O can be passed through chain of counters) */
class CountCtrl (myParams: CountParams) extends IOBundle {
  /** Decide whether node should be connected to an input or tied to constant */
  def ctrlGen(loc: CtrlLoc) : MyBool = {
    loc match {
      case External => MyBool(INPUT)
      case TieFalse => MyBool(false)
      case _ => MyBool(true)
    }
  }
  val wrap = ctrlGen(myParams.wrapCtrl)
  val change = ctrlGen(myParams.changeCtrl)
  val reset = MyBool(INPUT)
}

/** Counter IO */
class CountIO (myParams: CountParams) extends IOBundle {
  // Count up/down control signal
  val upDown = myParams.countType match {
    case UpDown => MyBool(INPUT)
    case Down => MyBool(true)
    case _ => MyBool(false)
  }
  // Counters usually increment by 1
  val inc = if (myParams.incMax == 1) MyUInt(1) else MyUInt(INPUT,myParams.incMax)
  // Counter wrap to value (up counters default wrap to 0)
  val wrapTo =  if (myParams.customWrap) MyUInt(INPUT,myParams.countMax) else MyUInt(0)
  // Counter default wrap condition is when count is maxed out (so need to know max)
  val max = if (myParams.wrapCtrl == Internal && myParams.countType != UpMod) MyUInt(INPUT,myParams.countMax) else MyUInt(myParams.countMax)
  // n in x%n
  val modN = if (myParams.countType == UpMod) MyUInt(INPUT,myParams.countMax+1) else MyUInt(0)
  val out = MyUInt(OUTPUT,myParams.countMax)
}

/** Counter template */
class Counter(myParams: CountParams) extends MyModule {
  
  val x = new CountIO(myParams); createIO(x)
  val iCtrl = new CountCtrl(myParams); createIO(iCtrl)
  val oCtrl = new CountCtrl(myParams).flip; createIO(oCtrl)
  
  val eq0 = (x.out === MyUInt(0))
  val eqMax = (x.out === x.max)
  
  val (upCustom, upCustomWrap) = MyMod(x.out + x.inc, x.max + MyUInt(1))
  val (modOut,overflow) = MyMod(x.out + x.inc,x.modN)
  
  // Adapt wrap condition based off of type of counter if it isn't retrieved externally
  val wrap = myParams.wrapCtrl match {
    case Internal => {
      myParams.countType match {
        case UpDown => MyMux(eqMax, eq0, x.upDown)
        case Down => eq0
        case Up => {
          if (myParams.incMax > 1) upCustomWrap          // For >1 increments, custom wrap indicated by sum overflow on next count
          else eqMax       
        }
        case UpMod => overflow
      }
    }
    case TieFalse => MyBool(false)
    case _ => iCtrl.wrap
  }
 
  // Adapt wrap to value based off of type of counter if it isn't retrieved externally
  val wrapTo = {
    if (myParams.customWrap) x.wrapTo
    else{
      myParams.countType match {
        case UpDown => MyMux(MyUInt(0),x.max,x.upDown)
        case Down => x.max
        case _ => MyUInt(0)
      }
    }
  }
  
  // If incrementing by 1 or using external wrap signals, add normally
  // But if incrementing by >1 and using internal wrap signals, do add mod (max + 1)
  val up = {
    if (myParams.incMax == 1 || (myParams.wrapCtrl == External && myParams.customWrap)) (x.out + x.inc).shorten(myParams.countMax)
    else upCustom
  }
  
  val down = x.out - x.inc
  
  val nextInSeq = myParams.countType match {
    case UpDown => MyMux(up, down, x.upDown)
    case Up => up
    case Down => down
    case UpMod => modOut
  }
  
  // When only internal wrap signals are used, note that mods already produce appropriately wrapped counter values
  val nextCount = {
    if (myParams.wrapCtrl == Internal && (myParams.countType == UpMod || (myParams.countType == Up && myParams.incMax > 1 && !myParams.customWrap))) 
      nextInSeq
    else MyMux(nextInSeq,wrapTo,wrap)
  }

  // Conditionally update (hold until update) or always update
  val newOnClk = myParams.changeCtrl match {
    case External => MyMux(x.out,nextCount,iCtrl.change)
    case TieTrue => nextCount
  }

  val count = MyMux(newOnClk,MyUInt(myParams.resetVal),iCtrl.reset)
  x.out := count.reg()
  
  // When counters are chained, subsequent counter increments when current counter wraps
  oCtrl.change := wrap & iCtrl.change
  
}

/////////////////////////////////////////////////////////////////////////////////////////////////

/** Counter Flavors */

/** Mod counter
  * count_new = {0 if wrapCond; else (count + inc) % modN }
  * oCtrl.change indicates if mod will wrap on the next cycle (count + inc > modVal)
  */
object ModCounter{
  def apply(countMax: Int, incMax: Int, nameExt: String = "") : Counter = {
    val myParams = CountParams(countMax = countMax, incMax = incMax, wrapCtrl = External, countType = UpMod)
    MyModule(new Counter(myParams), "ModCounter" + (if (nameExt == "") "" else ("_" + nameExt)))
  }
}

/*
  DEFAULTS
  countMax:   Int,                      // Upper limit of counter range
  incMax:     Int       = 1,            // Upper limit of increment range
  resetVal:   Int       = 0,            // Value on reset
  wrapCtrl:   CtrlLoc   = Internal,     // Location of wrap control signal
  changeCtrl: CtrlLoc   = External,     // Location of counter update control signal
  countType:  CountType = Up,           // Count type/direction
  customWrap: Boolean   = false         // Whether custom wrap to value exists
*/

