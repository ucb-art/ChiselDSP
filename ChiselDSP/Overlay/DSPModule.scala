/** Chisel Module Class Extension */

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

  /** Converts a double value to a constant DSPFixed (using [intWidth,fracWidth] parameters)
    * or DSPDbl (ignoring parameters).
    */
  def double2T[A <: DSPQnm[A]](x: Double, fixedParams: (Int,Int) = null): T = {
    val default = (fixedParams == null)
    val out = gen.asInstanceOf[A] match {
      case f: DSPFixed => {
        val intWidth = if (default) f.getIntWidth else fixedParams._1
        val fracWidth = if (default) f.getFracWidth else fixedParams._2
        DSPFixed(x, (intWidth,fracWidth))
      }
      case d: DSPDbl => DSPDbl(x)
      case _ => gen.error("Illegal generic type. Should be either DSPDbl or DSPFixed.")
    }
    out.asInstanceOf[T] 
  }
  def double2T[A <: DSPQnm[A]](x: Double, fracWidth: Int): T = {
    val fixed = DSPFixed.toFixed(x, fracWidth)
    val intWidth = fixed.bitLength-fracWidth
    double2T(x,(intWidth,fracWidth))
  }

  /** Allows you to customize each T (DSPFixed or DSPDbl) for parameters like 
    * integer width and fractional width (in the DSPFixed case) 
    */
  def T[A <: DSPQnm[A]](dir: IODirection, fixedParams: (Int,Int) = null): T = {
    val default = (fixedParams == null)
    val out =  gen.asInstanceOf[A] match {
      case f: DSPFixed => {
        val intWidth = if (default) f.getIntWidth else fixedParams._1
        val fracWidth = if (default) f.getFracWidth else fixedParams._2
        DSPFixed(dir, (intWidth,fracWidth))
      }
      case d: DSPDbl => DSPDbl(dir)
      case _ => gen.error("Illegal generic type. Should be either DSPDbl or DSPFixed.")
    }
    out.asInstanceOf[T] 
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
  * Note that all inputs should have the same delay.
  */
abstract class IOBundle extends Bundle {
  Module.current.asInstanceOf[DSPModule].ios.push(this)

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

}

/** Adds functionality to Module */
abstract class DSPModule (val inputDelay:Int = 0, decoupledIO: Boolean = false, _clock: Option[Clock] = None,
                          _reset: Option[Bool] = None) extends ModuleOverride(_clock,_reset) {

  /** Adds support for DSPBool with when statements */
  //private[ChiselDSP] val switchKeysDSP = Stack[Bits]()
  //private[ChiselDSP] val whenCondsDSP = Stack[DSPBool]()
  //private[ChiselDSP] def hasWhenCondDSP: Boolean = !whenCondsDSP.isEmpty
  //private[ChiselDSP] def whenCondDSP: DSPBool = if (hasWhenCondDSP) whenCondsDSP.top else DSPBool(true)

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
    m.flatten.map( x =>
      if (!x._2.isDirectionless && !x._2.isLit) addPinChiselDSP(x._2)
      else Error("Directionless Lit should not be in an IO Bundle. You can use Option-able bundles.")
    )
    this
  }

}

object DSPModule {

  /** Instantiates module with detailed name (from class/object name + [optional] extension) 
    * + adds IO pins as necessary 
    */
  def apply[T <: DSPModule](m: => T, nameExt: String = ""): T = {
    val thisModule = Module(m)
    var currentName = thisModule.name
    if (currentName == "") currentName = thisModule.getClass.getName.toString.split('.').last
    val optName = if (nameExt == "") nameExt else "_" + nameExt
    thisModule.setModuleName(currentName + optName)
    val ios2 = thisModule.ios.clone
    while (!thisModule.ios.isEmpty) {
      val ioSet = thisModule.ios.pop
      // 'io' is handled separately in your normal Chisel fashion.
      if(!ioSet.equals(thisModule.io)) {
        val ioName = ioSet.getClass.getName.toString.split('.').last.split('$').last
        ioSet.setNameIO(ioName)
      }
    }
    while (!ios2.isEmpty) {
      val ioSet = ios2.pop
      // Need to add pin after correctly designating pin name
      if(!ioSet.equals(thisModule.io)) thisModule.createIO(ioSet)
    }
    thisModule
  }
 
}