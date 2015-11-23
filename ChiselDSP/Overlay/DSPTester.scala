/** Custom DSP tester -- shows values in Ints, Doubles instead of only hex */

package ChiselDSP
import Chisel._
import java.lang.Double.{longBitsToDouble, doubleToLongBits}

class DSPTester[+T <: Module](c: T, var traceOn: Boolean = true, var hexOn: Boolean = true) extends Tester(c, false){

  /** Differentiate treatment of signed, unsigned types -- Added ChiselDSP types */
  override def signed_fix(dtype: Bits, rv: BigInt): BigInt = {
    val w = dtype.needWidth()
    dtype match {
      /* Any "signed" node */
      case _: SInt | _ : Flo | _: Dbl | _: MyDbl | _: MyFixed => (if(rv >= (BigInt(1) << w - 1)) (rv - (BigInt(1) << w)) else rv)
      /* anything else (i.e., UInt) */
      case _ => (rv)
    }
  }
  
  /** Sort nodes with generic types for peeking */
  def myPeek (data: Bits) : Double = peek(data,true)
  private def peek (data: Bits, display: Boolean) : Double = {
    val resBits = super.peek(data.asInstanceOf[Bits])
    val res = resBits.toDouble
    val resDbl = longBitsToDouble(resBits.toLong)
    val hexString = if (hexOn) "(0x%x)".format(resBits) else ""
    val name = dumpName(data)
    val (s,out) =  data match {
      case d1: Dbl => ("  PEEK %s -> %f".format(name,resDbl), resDbl)
      case d2: MyDbl => ("  PEEK %s -> %f (%s)".format(name,resDbl,d2.printWidth()), resDbl)
      case u1: UInt => ("  PEEK %s -> %d %s".format(name,resBits,hexString), res)
      case u2: MyUInt => ("  PEEK %s -> %d %s %s".format(name,resBits,hexString,u2.printWidth()), res)
      case _ => ("  PEEK %s -> %s (%d bit(s))".format(name,hexString,data.getWidth), res)
    }
    if (traceOn && display) println(s)
    out
  }
  
  
  
  
  
  // Poke Dbl or Fixed
  def poke(node:Bits with MyNum[_], x: Double, display:Boolean = true){
    node match {
      //case f0: Fixed => poke(node.asInstanceOf[Bits], Fixed.toFixed(x,f0.getFractionalWidth()))
      case d0: MyDbl => poke(node.asInstanceOf[Bits], BigInt(doubleToLongBits(x)))
      //case _ => error(node)
    }
    peek(node,display) 
  }
  
  override def step(n: Int) {
    super.step(n)
    if (traceOn) println(s"STEP ${n} -> ${t+n}")
  }
  
  
  
  

  
  
 
   
  
  
  
  
  
  
  
  
  
  
 
  
 
  
  
  
  // other tace
  
  
//custom finifh (super)
  
  
 /* 
  def peek(data: MyFixed, display: Boolean) : Double = {
    //println("ttt")
    peek(data.asInstanceOf[Bits]).toDouble
  }
 
  
  */
  
  
  
//sint
  

  
  

  
  
}
