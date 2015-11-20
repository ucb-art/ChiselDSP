/** Chisel Module Class Extension */

package ChiselDSP
import Chisel._

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
      /*
      case f: MyFixed => {
        val width = if (default) f.width else fixedParams._1
        val fracWidth = if (default) f.fractionalWidth else fixedParams._2
        MyFixed(x, (width,fracWidth))
      }
      */
      case d: MyDbl => MyDbl(x)
      case _ => throwException("Illegal type.")
    }
    out.asInstanceOf[T] 
  }

}

/** Adds missing functionality to Chisel x.x.x version of Module */
abstract class MyModule extends Module {

  // Fixed IO is blank -- use createIO with your custom bundles
  var io = new Bundle() {}
  
  /** Easily add elements of aggregates (Vecs, Complex, etc.) to the list of debug nodes */
  def debug(data: Aggregate) : Unit = data.flatten.map(x => x._2) map (debug(_)) 
  
  /** Set module name missing in earlier versions of Chisel. Useful when the same module 
    * is repurposed for different tasks.
    */
  def setModuleName(n : String) { moduleName = n }
  
  /** Convert list of potential IO pins to actual IO.  
    * Allows you to selectively set signals to be module IOs (with direction) or just constants (directionless).
    * Name of IO pin = Aggregate name _ signal name
    */
  def createIO[T <: Bundle](m: => T): T = {
    val ioName = m.getClass.getName.toString.split('.').last
    m.flatten.map( x => if (!x._2.isDirectionless) addPin(x._2, ioName + "_" + (if (x._2.dir == INPUT) "in" else "out") + "_" + x._1 ))
    m
  }
  
}

object MyModule {

  /** Instantiates module with detailed name (from class/object name + [optional] extension) */
  def apply[T <: MyModule](m: => T, nameExt: String = "")(implicit p: Parameters = params): T = {
    val thisModule = Module(m)(p)
    var currentName = thisModule.name
    if (currentName == "") currentName = thisModule.getClass.getName.toString.split('.').last
    val optName = if (nameExt == "") nameExt else "_" + nameExt
    thisModule.setModuleName(currentName + optName)
    thisModule
  }
  
  /** Optional Module params */
  private def params = if(Driver.parStack.isEmpty) Parameters.empty else Driver.parStack.top
  
}
