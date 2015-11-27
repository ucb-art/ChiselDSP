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
  
  
  // bits w/ just bigint
  
  /** Sort nodes with generic types for peeking */
  
  // peek dbl
  def peek(data: MyBool) : Double = peek(data,true,true)
  def peek (data: MyBits with MyNum[_]) : Double = peek(data,true,true)
  def peek (data: SInt) : Double = peek(data,true,true)
  private def peek (data: Bits, display: Boolean, peek: Boolean) : Double = {
    val resBits = super.peek(data.asInstanceOf[Bits])
    val res = resBits.toDouble
    val resDbl = longBitsToDouble(resBits.longValue)
    val hexString = if (hexOn) "(0x%x)".format(resBits) else ""
    val name = dumpName(data)
    val command = if (peek) "PEEK" else "POKE"
    val beginning = "  %s %s -> ".format(command,name)
    val (s,out) =  data match {
      case d1: Dbl => ("%s%f".format(beginning,resDbl), resDbl)
      case d2: MyDbl => ("%s%f (%s)".format(beginning,resDbl,d2.printWidth()), resDbl)
      case u1: UInt => ("%s%d %s".format(beginning,resBits,hexString), res)
      case u2: MyUInt => ("%s%d %s %s".format(beginning,resBits,hexString,u2.printWidth()), res)
      case f2: MyFixed => {
        //println("%x".format(resBits.flipBit(63)))
        //println(resBits.toBigInt)
        //val bl = resBits.bitLength
        //val blm = bl-1
        //println( blm + ",")
        
        
        val dbl = if (f2.isLit){
          val msb = if (resBits.testBit(resBits.bitLength-1)) 1 else 0
          val pad = msb.toString * (64-resBits.bitLength)
          val newResBits = pad ++ resBits.toString(2)
          println(newResBits+","+newResBits.length)
          MyFixed.toDouble(BigInt(newResBits,2).longValue,f2.getFracWidth())
        } else MyFixed.toDouble(resBits.longValue,f2.getFracWidth())
        ("%s%f %s %s".format(beginning,dbl,hexString,f2.printWidth()), dbl)
      }
      case s1: SInt => {
        
        println("sibt")
        
        if (s1.isLit || s1.dir == INPUT){
          val msb = if (resBits.testBit(resBits.bitLength-1)) 1 else 0
          val pad = msb.toString * (64-resBits.bitLength)
          val newResBits = pad ++ resBits.toString(2)
          println("www"+newResBits+","+newResBits.length)
          ("%s%d %s".format(beginning,BigInt(newResBits,2).longValue,hexString), resBits.longValue.toDouble)
        }
        else
        
        ("%s%d %s".format(beginning,resBits,hexString), resBits.longValue.toDouble)
      }
      case _ => ("%s%s (%d bit(s))".format(beginning,hexString,data.getWidth), res)
    }
    if (traceOn && display) println(s)
    out
  }
  
  
  
  
  
  
  
  override def poke(node: Bits, x: Boolean) : Unit = poke(node,if (x) 1 else 0,true)
  override def poke(node: Bits, x: Int) : Unit = poke(node,x,true)
  override def poke(node: Dbl, x: Double) : Unit = poke(node,x,true)
  def poke(node: Bits, x: Double) : Unit = poke(node,x,true)
  private def poke(node:Bits, x: Double, display:Boolean){
    if (!node.isTopLevelIO || node.dir != INPUT) {
      val dumpStack = Warn.dumpStack
      Warn.dumpStack = false
      Warn(s"  POKE NOT ALLOWED on ${dumpName(node)}",true)
      Warn.dumpStack = dumpStack
    }
    val nodeBits = node.asInstanceOf[Bits]
    node match {
      case d1: Dbl => super.poke(d1,x)
      case d2: MyDbl => super.poke(nodeBits, BigInt(doubleToLongBits(x)))
      case u2: MyUInt => super.poke(nodeBits,x.toInt)
      case f2: MyFixed => super.poke(nodeBits, MyFixed.toFixed(x,f2.getFracWidth()))
      case _ => super.poke(nodeBits,x.toInt)
      //case s1: SInt => super.poke(s1,x)
      
    }
    peek(node,display,false) 
  }
  
 
  
  override def step(n: Int) {
    super.step(n)
    if (traceOn) println(s"STEP ${n} -> ${t+n}")
  }
  
  
  
  

  
  
 // bits
   
  
  
  
  
  
  
  
  
  
  
 
  
 
  
  
  
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
