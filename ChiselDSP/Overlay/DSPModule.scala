/** Chisel Module Class Extension */
// TODO: Might not need "flatten" in x.flatten.map (clean up?)

package ChiselDSP
import scala.collection.mutable.Stack
import Chisel._

/** Module that allows passing in a generic type for DSP. Allows the designer to seamlessly switch
  * between DSPDbl and DSPFixed implementations for functional testing vs. fixed-point
  * characterization and optimization.
  */ 
abstract class GenDSPModule[T <: DSPQnm[T]](gen : => T, inputDelay:Int = 0, decoupledIO: Boolean = false,
                                            _clock: Option[Clock] = None, _reset: Option[Bool] = None
                                           ) extends DSPModule(inputDelay, decoupledIO, _clock, _reset) {

  val fixedParams = (gen.getIntWidth,gen.getFracWidth)

  // TODO: Cleaner conversion is use Lit with cloneType (need more options for cloneType)

  /** Converts a double value to a constant DSPFixed (using [intWidth,fracWidth] parameters)
    * or DSPDbl (ignoring parameters).
    */
  def double2T[A <: DSPQnm[A]](x: Double, fixedParams: (Int,Int)): T = {
    val out = gen.asInstanceOf[A] match {
      case f: DSPFixed => DSPFixed(x, fixedParams)
      case d: DSPDbl => DSPDbl(x)
      case _ => gen.error("Illegal generic type. Should be either DSPDbl or DSPFixed.")
    }
    out.asInstanceOf[T] 
  }
  def double2T[A <: DSPQnm[A]](x: Double, fracWidth: Int): T = {
    val out = gen.asInstanceOf[A] match {
      case f: DSPFixed => DSPFixed(x, fracWidth)
      case d: DSPDbl => DSPDbl(x)
      case _ => gen.error("Illegal generic type. Should be either DSPDbl or DSPFixed.")
    }
    out.asInstanceOf[T]
  }

  def double2T[A <: DSPQnm[A]](x: Double): T = {
    double2T(x,gen.getFracWidth)
  }

  /** Cast Chisel bits type as Gen (DSPFixed or DSPDbl)*/
  def genCast(x: Bits): T = {
    val res = chiselCast(x){gen.cloneType()}
    res.dir = x.dir
    res.assign()
  }

  /** Returns (Fixed, floating point) type used */
  def getType[A <: DSPQnm[A]](): String = {
    gen.asInstanceOf[A] match {
      case f: DSPFixed => "Fixed"
      case d: DSPDbl => "Double"
      case _ => Error("Illegal generic type. Should be either DSPDbl or DSPFixed."); ""
    }
  }
 
}

/** Special bundle for IO - should only be used with DSPModule and its child classes 
  * Adds itself to the current DSPModule's list of IOs to setup.
  * If checkOutDlyMatch = true, all assigned outputs of the bundle should have the same delay
  * Note that all inputs should have the same delay.
  */
abstract class IOBundle(val outDlyMatch: Boolean = false) extends Bundle {

  Module.current match {
    case a: DSPModule => a.ios.push(this)
    case _ =>
  }

  /** Name IO bundle + elements. For a custom bundle name,
    * setName should be called within the module.
    * Otherwise, the name is the Bundle's class name.
    * Dir indicates whether to include port direction in the name.
    * Name of IO pin = IOBundleName _ direction _ signalName
    */
  override def setName (name:String) = setName(name, dir = true)
  def setName(name:String, dir: Boolean) = label(name, isNamingIo = true, dir, custom = true)

  /** Set name used internally -- not user specified */
  private[ChiselDSP] def setNameIO(name:String) = label(name, isNamingIo = true, dir = true, custom = false)
  private def label (path: String, isNamingIo: Boolean, dir:Boolean, custom: Boolean) {
    val oldName = name
    if( !named && (name.isEmpty || (!path.isEmpty && name != path)) ) {
      name = path
      val prefix = if (name.length > 0) name + "_" else ""
      flatten.map( x =>
        {
          val dirStr = if (isNamingIo && dir) (if (x._2.dir == INPUT) "in" else "out") + "_" else ""
          val newName = prefix + dirStr + x._1
          val newName2 = prefix + dirStr + x._2.name.replace("_in","").replace("_out","")
          if (x._2.name.length == 0) x._2.nameIt(newName, isNamingIo)
          else if (x._1.contains(x._2.name) && !custom){
            x._2.named = false
            x._2.nameIt(newName2, isNamingIo)
          }
        }
      )
    }
    else if (custom) Warn("IO Bundle already named " + oldName + ". Cannot rename to " + path + ".")
    named = true
  }

  /** Checks to see that assigned outputs of the IO Bundle have the same delay; otherwise errors out */
  private[ChiselDSP] def checkOutDly(): Int ={
    if (outDlyMatch){
      val temp = flatten.map (x => x._2 match{
        case d : DSPBits[_] => if(d.dir == OUTPUT && d.isAssigned) d.getDelay() else -1
        case _ => -1
      })
      val dly = temp.distinct.filter(_ != -1)
      val numDistinct = dly.length
      if (numDistinct > 1) Error("Assigned IO Bundle outputs don't have the same delay")
      if (numDistinct == 1) dly.head else 0
    }
    else 0
  }

  // TODO: Get delay of elements of a Vec like you do for a bundle

  /** Get output delay of bundle elements if tracked elements should all have the same delay. Note that the user
    * can call the function before all of the outputs have been assigned, at which point it will only return
    * the delay of assigned outputs! Caution must be used!
    */
  def getOutDelay(): Int = {
    if (!outDlyMatch) Error("Cannot get IO Bundle output delay when not explicitly checked")
    checkOutDly()
  }

}

/** Adds functionality to Module (inputDelay used in Info instantiation under DSPTypes) */
abstract class DSPModule (val inputDelay:Int = 0, decoupledIO: Boolean = false, _clock: Option[Clock] = None,
                          _reset: Option[Bool] = None) extends ModuleOverride(_clock,_reset) {

  // Keeps track of IO bundles
  private[ChiselDSP] val ios = Stack[IOBundle]()
  
  // Optional I/O ready + valid
  class DecoupledIO extends IOBundle {
    val ready = if (decoupledIO) Some(DSPBool(INPUT)) else None
    val valid = if (decoupledIO) Some(DSPBool(INPUT)) else None
  }
  
  val decoupledI = new DecoupledIO()
  val decoupledO = new DecoupledIO().flip
  
  /** Convert list of potential IO pins (must be in a Bundle) to actual IO.  
    * Allows you to selectively set signals to be module IOs (with direction).
    */
  private def createIO[T <: IOBundle](m: => T): this.type = {
    m.flatten.map( x => {
      if (!x._2.isDirectionless && !x._2.isLit) addPinChiselDSP(x._2)
      else Error("Directionless Lit should not be in an IO Bundle. You can use Option-able bundles.")
    })
    this
  }

}

object DSPModule {

  /** Instantiates module with detailed name (from class/object name + [optional] extension) 
    * + adds IO pins as necessary 
    */
  def apply[T <: Module](m: => T, nameExt: String = ""): T = {
    val thisModule = Module(m)
    var currentName = thisModule.name
    if (currentName == "") currentName = thisModule.getClass.getName.toString.split('.').last
    val optName = if (nameExt == "") nameExt else "_" + nameExt
    val newName = currentName + optName
    // Name module
    thisModule.setModuleName(newName)
    // Name module (used for Verilog file name)
    thisModule.setName(newName)

    // Supports both Chisel.Module (do nothing) and DSPModule (custom IO)
    thisModule match {
      case a: DSPModule => {
        val ios2 = a.ios.clone
        while (!a.ios.isEmpty) {
          val ioSet = a.ios.pop
          // 'io' is handled separately in your normal Chisel fashion.
          if(!ioSet.equals(a.io)) {
            val ioName = ioSet.getClass.getName.toString.split('.').last.split('$').last
            ioSet.setNameIO(ioName)
          }
        }
        while (!ios2.isEmpty) {
          val ioSet = ios2.pop
          ioSet.checkOutDly()
          // Need to add pin after correctly designating pin name
          if(!ioSet.equals(a.io)) a.createIO(ioSet)
        }
      }
      case _ =>
    }

    thisModule
  }
 
}