/** Chisel Module Class Extension */

package ChiselDSP
import scala.collection.mutable.{Stack}
import Chisel._

/** Module that allows passing in a generic type for DSP. Allows the designer to seamlessly switch
  * between DSPDbl and DSPFixed implementations for functional testing vs. fixed-point
  * characterization and optimization.
  */ 
abstract class GenDSPModule[T <: DSPQnm[_]](gen : => T, decoupledIO: Boolean = false, 
                                            _clock: Option[Clock] = None, _reset: Option[Bool] = None
                                           ) extends DSPModule(decoupledIO, _clock, _reset) {

  /** Converts a double value to a constant DSPFixed (using [intWidth,fracWidth] parameters)
    * or DSPDbl (ignoring parameters).
    */
  def double2T[A <: DSPQnm[_]](x: Double, fixedParams: (Int,Int) = null): T = {
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
  def double2T[A <: DSPQnm[_]](x: Double, fracWidth: Int): T = {
    val fixed = DSPFixed.toFixed(x, fracWidth)
    val intWidth = fixed.bitLength-fracWidth
    double2T(x,(intWidth,fracWidth))
  }

  /** Allows you to customize each T (DSPFixed or DSPDbl) for parameters like 
    * integer width and fractional width (in the DSPFixed case) 
    */
  def T[A <: DSPQnm[_]](dir: IODirection, fixedParams: (Int,Int) = null): T = {
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
 
}

/** Special bundle for IO - should only be used with DSPModule and its child classes 
  * Adds itself to the current DSPModule's list of IOs to setup.
  * Note that all inputs should have the same delay.
  */
abstract class IOBundle (view: Seq[String] = Seq()) extends Bundle(view) {
  Module.current.asInstanceOf[DSPModule].ios.push(this)
}

/** Adds functionality to Module */
abstract class DSPModule (decoupledIO: Boolean = false, _clock: Option[Clock] = None, _reset: Option[Bool] = None)
                         extends ModuleOverride(_clock,_reset) {

  // Keeps track of IO bundles
  private[ChiselDSP] val ios = Stack[IOBundle]()

  // Fixed IO is blank -- use createIO with your custom bundles
  var io = new Bundle
  
  // Optional I/O ready + valid
  class DecoupledIO extends Bundle {
    val ready = if (decoupledIO) DSPBool(INPUT) else DSPBool(true)
    val valid = if (decoupledIO) DSPBool(INPUT) else DSPBool(true)
  }
  
  val decoupledI = new DecoupledIO()
  val decoupledO = new DecoupledIO().flip
  
  /** Convert list of potential IO pins (must be in a Bundle) to actual IO.  
    * Allows you to selectively set signals to be module IOs (with direction) 
    * or just constants (directionless).
    * Name of IO pin = IOBundleName _ direction _ signalName
    */
  private def createIO[T <: Bundle](m: => T): this.type = {
    val ioName = m.getClass.getName.toString.split('.').last.split('$').last
    m.flatten.map( x => 
      if (!x._2.isDirectionless && !x._2.isLit) 
        addPin(x._2, ioName + "_" + (if (x._2.dir == INPUT) "in" else "out") + "_" + x._1 )
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
    while (!thisModule.ios.isEmpty) {
      val ioset = thisModule.ios.pop()
      thisModule.createIO(ioset)
    } 
    thisModule.createIO(thisModule.decoupledI)
    thisModule.createIO(thisModule.decoupledO)   
  }
 
}
