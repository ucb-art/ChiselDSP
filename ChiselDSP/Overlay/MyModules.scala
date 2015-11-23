/** Chisel Module Class Extension */

// package ChiselDSP
// import Chisel._

package Chisel
import ChiselDSP._
import scala.collection.mutable.{Stack}

/** Module that allows passing in a generic type for DSP. Allows the designer to seamlessly switch
  * between MyDbl and MyFixed implementations for functional testing vs. fixed-point
  * characterization and optimization.
  */ 
abstract class DSPModule[T <: Bits with MyNum[T]](gen : => T) extends MyModule {

  /** Converts a double value to a constant MyFixed (using [width,fracWidth] parameters) or MyDbl (ignoring parameters).
    * Note that both parameters should be specified at the same time.
    */
  def double2T[A <: Bits with MyNum[A]](x: Double, fixedParams: (Int,Int) = null): T = {
    val default = (fixedParams == null)
    val out =  gen.asInstanceOf[A] match {
      case f: MyFixed => {
        val width = if (default) f.getWidth else fixedParams._1
        val fracWidth = if (default) f.getFractionalWidth else fixedParams._2
        MyFixed(x, (width,fracWidth))
      }
      case d: MyDbl => MyDbl(x)
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
abstract class MyModule extends Module {

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
  def createIO[T <: IOBundle](m: => T): T = {
  
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
    thisModule
  }
  
  /** Optional Module params */
  private def params = if(Driver.parStack.isEmpty) Parameters.empty else Driver.parStack.top
  
}
