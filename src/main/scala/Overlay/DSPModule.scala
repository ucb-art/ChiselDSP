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
    double2T(x,(gen.getIntWidth,gen.getFracWidth))
  }
  
  def double2T[A <: DSPQnm[A]](x: Double, gen: A): A = {
    val out = gen match {
      case f: DSPFixed => DSPFixed(x, (gen.getIntWidth,gen.getFracWidth))
      case d: DSPDbl => DSPDbl(x)
      case _ => gen.error("Illegal generic type. Should be either DSPDbl or DSPFixed.")
    }
    out.asInstanceOf[A]
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

    // TODO: Look into weird bug when you setModuleName of multiple modules to be the same -- Chisel doesn't
    // seem to always automatically infer that there's a name conflict ? This is a problem when you use the same class
    // but with different parameters... maybe should keep track of all names used and check for conflict?

    if (nameExt != "") {
      var currentName = thisModule.name
      if (currentName == "") currentName = thisModule.getClass.getName.toString.split('.').last
      val optName = if (nameExt == "") nameExt else "_" + nameExt
      val newName = currentName + optName
      // Name module
      thisModule.setModuleName(newName)
      // Name module (used for Verilog file name)
      thisModule.setName(newName)
    }

    // Supports both Chisel.Module (do nothing) and DSPModule (custom IO)
    thisModule match {
      case a: DSPModule => {
        val ios2 = a.ios.clone
        while (!a.ios.isEmpty) {
          val ioSet = a.ios.pop
          // 'io' is handled separately in your normal Chisel fashion.
          if(!ioSet.equals(a.io)) {
            val ioName = ioSet.getClass.getName.toString.split('.').last.split('$').last
            // If anonymous class, then ioName will be all digits
            ioSet.setNameIO(if (ioName forall Character.isDigit) "" else ioName)
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
