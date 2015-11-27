/** Chisel Module Class Extension */

package Chisel
import ChiselDSP._
import scala.collection.mutable.{Stack}

/** Module that allows passing in a generic type for DSP. Allows the designer to seamlessly switch
  * between MyDbl and MyFixed implementations for functional testing vs. fixed-point
  * characterization and optimization.
  */ 
abstract class DSPModule[T <: MyBits with MyNum[T]](gen : => T, decoupledIO: Boolean = false) extends MyModule(decoupledIO) {

  /** Converts a double value to a constant MyFixed (using [width,fracWidth] parameters) or MyDbl (ignoring parameters).
    * Note that both parameters should be specified at the same time.
    */
  def double2T[A <: MyBits with MyNum[A]](x: Double, fixedParams: (Int,Int) = null): T = {
    val default = (fixedParams == null)
    val out =  gen.asInstanceOf[A] match {
      case f: MyFixed => {
        val intWidth = if (default) f.getIntWidth else fixedParams._1
        val fracWidth = if (default) f.getFracWidth else fixedParams._2
        MyFixed(x, (intWidth,fracWidth))
      }
      case d: MyDbl => MyDbl(x)
      case _ => throwException("Illegal type.")
    }
    out.asInstanceOf[T] 
  }
  
  /** Allows you to customize each T (MyFixed or MyDbl) for parameters like width and fractional width (in the MyFixed case) */
  def T[A <: MyBits with MyNum[A]](dir: IODirection, fixedParams: (Int,Int) = null): T = {
    val default = (fixedParams == null)
    val out =  gen.asInstanceOf[A] match {
      case f: MyFixed => {
        val intWidth = if (default) f.getIntWidth else fixedParams._1
        val fracWidth = if (default) f.getFracWidth else fixedParams._2
        MyFixed(dir, (intWidth,fracWidth))
      }
      case d: MyDbl => MyDbl(dir)
      case _ => throwException("Illegal type.")
    }
    out.asInstanceOf[T] 
  }
 
}

/** Special bundle for IO - should only be used with MyModule and its child classes 
  * Adds itself to the current MyModule's list of IOs to setup
  */
abstract class IOBundle (view: Seq[String] = Seq()) extends Bundle(view) {
  Module.current.asInstanceOf[MyModule].ios.push(this)
}

/** Adds functionality to Module */
abstract class MyModule (decoupledIO: Boolean = false) extends Module {

  // Optional I/O ready + valid
  class DecoupledIO extends Bundle {
    val ready = if (decoupledIO) MyBool(INPUT) else MyBool(true)
    val valid = if (decoupledIO) MyBool(INPUT) else MyBool(true)
  }
  
  val decoupledI = new DecoupledIO()
  val decoupledO = new DecoupledIO().flip

  // Keeps track of IO bundles
  private[Chisel] val ios = Stack[IOBundle]()

  // Fixed IO is blank -- use createIO with your custom bundles
  var io = new Bundle() {}
  
  /** Easily add elements of aggregates (Vecs, Complex, etc.) to the list of debug nodes */
  def debug(data: Aggregate) : Unit = data.flatten.map(x => x._2) map (debug(_)) 
  
  /** Convert list of potential IO pins (must be in an IOBundle) to actual IO.  
    * Allows you to selectively set signals to be module IOs (with direction) or just constants (directionless).
    * Name of IO pin = IOBundleName _ direction _ signalName
    */
  private def createIO[T <: Bundle](m: => T): T = {
  
    /** Newer versions of Chisel have an addPin that doesn't work :( 
      * Add a pin with a name to the module
      * @param pin the I/O to add
      * @param name A name for the pin
      */
    def addPin[T <: Data](pin: T, name: String = "") = {
      val gen = pin
      io match {
        case b: Bundle => {
          for ((n, io) <- gen.flatten) {
            io.compOpt = Some(this)
            io.isIo = true
          }
          if (name != "") gen nameIt (name, true)
          b.elements += ((gen.name, gen))
        }
        case _ => // Is it possible?
      }
      gen
    } 
  
    val ioName = m.getClass.getName.toString.split('.').last.split('$').last
       
    m.flatten.map( x => if (!x._2.isDirectionless && !x._2.isLit) addPin(x._2, ioName + "_" + (if (x._2.dir == INPUT) "in" else "out") + "_" + x._1 ))
    m
  }
  
}

object MyModule {

  /** Instantiates module with detailed name (from class/object name + [optional] extension) + adds IO pins as necessary */
  def apply[T <: MyModule](m: => T, nameExt: String = "")(implicit p: Parameters = params): T = {
    val thisModule = Module(m)(p)
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
    thisModule
  }
  
  /** Optional Module params */
  private def params = if(Driver.parStack.isEmpty) Parameters.empty else Driver.parStack.top
  
}
