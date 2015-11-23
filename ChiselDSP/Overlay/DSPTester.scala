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
  def myPeek (data: Bits) : Double = peek(data,true,true)
  private def peek (data: Bits, display: Boolean, peek: Boolean) : Double = {
    val resBits = super.peek(data.asInstanceOf[Bits])
    val res = resBits.toDouble
    val resDbl = longBitsToDouble(resBits.toLong)
    val hexString = if (hexOn) "(0x%x)".format(resBits) else ""
    val name = dumpName(data)
    val command = if (peek) "PEEK" else "POKE"
    val beginning = "  %s %s -> ".format(command,name)
    val (s,out) =  data match {
      case d1: Dbl => ("%s%f".format(beginning,resDbl), resDbl)
      case d2: MyDbl => ("%s%f (%s)".format(beginning,resDbl,d2.printWidth()), resDbl)
      case u1: UInt => ("%s%d %s".format(beginning,resBits,hexString), res)
      case u2: MyUInt => ("%s%d %s %s".format(beginning,resBits,hexString,u2.printWidth()), res)
      case _ => ("%s%s (%d bit(s))".format(beginning,hexString,data.getWidth), res)
    }
    if (traceOn && display) println(s)
    out
  }
  
  
  
  
  
  
  
  
  override def poke(node: Bits, x: Int) : Unit = poke(node,x,true)
  override def poke(node: Dbl, x: Double) : Unit = poke(node,x,true)
  def poke(node: Bits, x: Double) : Unit = poke(node,x,true)
  private def poke(node:Bits, x: Double, display:Boolean){
    val nodeBits = node.asInstanceOf[Bits]
    node match {
      case d1: Dbl => super.poke(d1,x)
      case d2: MyDbl => super.poke(nodeBits, BigInt(doubleToLongBits(x)))
      case u2: MyUInt => super.poke(nodeBits,x.toInt)
      
    }
    peek(node,display,false) 
  }
  
 
  
  override def step(n: Int) {
    super.step(n)
    if (traceOn) println(s"STEP ${n} -> ${t+n}")
  }
  
  
  
  

  
  
 
   
  
  
  
  
  
  
  
  
  
  
 
  
 
  
  
  
  // other tace
  // agrregate map to my properly
  
//custom finifh (super)
  
  
 /* 
  def peek(data: MyFixed, display: Boolean) : Double = {
    //println("ttt")
    peek(data.asInstanceOf[Bits]).toDouble
  }
 
  
  */
  
  
  
//sint
  

  
  

  
  
}
