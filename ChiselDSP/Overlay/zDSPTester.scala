/** Custom DSP tester -- shows values in Ints, Doubles instead of only hex.
  * Handles Chisel data types and ChiselDSP data types.  
  * TODO: Aggregate peek/poke, expect, custom finish. 
  */

package ChiselDSP
import Chisel._
import java.lang.Double.{longBitsToDouble, doubleToLongBits}
import java.lang.Float.{intBitsToFloat, floatToIntBits}

class DSPTester[+T <: Module](c: T, var traceOn: Boolean = true, var hexOn: Boolean = true) extends Tester(c, false, 16){

  /** Differentiate treatment of signed, unsigned types -- Added ChiselDSP types. */
  override def signed_fix(dtype: Bits, rv: BigInt): BigInt = {
    val w = rv.bitLength.max(dtype.needWidth())
    val signrv = if(rv >= (BigInt(1) << w - 1)) (rv - (BigInt(1) << w)) else rv
    dtype match {
      /* Any "signed" node */
      case _: SInt | _ : Flo | _: Dbl | _: DSPDbl | _: Fixed | _: DSPFixed => signrv
      /* anything else (i.e., UInt) */
      case _ => rv
    }
  }
  
  /** Peek ChiselDSP types */
  def peek(data: DSPBool): Boolean = if (peek(data,true,true)._2 > 0) true else false
  def peek(data: DSPUInt): BigInt = peek(data,true,true)._2
  def peek(data: DSPDbl): Double = peek(data,true,true)._1
  def peek(data: DSPFixed): Double = peek(data,true,true)._1
  
  /** Peek Chisel types */
  def peek(data: Bool): Boolean = if (peek(data,true,true)._2 > 0) true else false
  def peek(data: UInt): BigInt = peek(data,true,true)._2
  def peek(data: SInt): BigInt = peek(data,true,true)._2
  override def peek(data: Dbl): Double = peek(data,true,true)._1
  override def peek(data: Flo): Float = peek(data,true,true)._1.floatValue
  def peek(data: Fixed): Double = peek(data,true,true)._1
  override def peek(data: Bits): BigInt = peek(data,true,true)._2
  
  /** Cluster some peek-specific processing */
  private def peekInit(data: Bits, peek: Boolean): Tuple2[BigInt,String] = {
    val res = super.peek(data)
    val resBits = if (data.isLit || (data.dir == INPUT && data.isTopLevelIO)) signed_fix(data, res) else res
    val command = if (peek) "PEEK" else "POKE"
    val infoStart = "  %s %s -> ".format(command,dumpName(data))
    (resBits, infoStart)
  }
  
  /** More general handling of peek for all possible data types */
  private def peek (data: Bits, display: Boolean, peek: Boolean) : Tuple2[Double,BigInt] = {
    val (resBits, infoStart) = peekInit(data,peek)
    val hexString = if (hexOn) "(0x%x) ".format(resBits) else ""
    val (info,outDbl) =  data match {
      case f0: Flo => {
        val resFlo = intBitsToFloat(resBits.toInt)
        ("%f".format(resFlo), resFlo.toDouble)
      }
      case _: Dbl | _: DSPDbl => { 
        val resDbl = longBitsToDouble(resBits.longValue)
        val ext = data match {
          case d2: DSPDbl => " (%s)".format(d2.infoString)
          case _ => ""
        }
        ("%f".format(resDbl) + ext,resDbl)
      }
      case _: UInt | _: DSPUInt | _: SInt => {                                      // Double value unused
        val ext = data match {
          case u2: DSPUInt => "%s".format(u2.infoString)
          case _ => ""
        }
        ("%d %s".format(resBits,hexString) + ext,0.0)
      }
      case _: Fixed | _DSPFixed => {                                                // Double value unused
        val resDbl = DSPFixed.toDouble(resBits.longValue,_.getFractionalWidth)
        val ext = data match {
          case f2: DSPFixed => " %s".format(f2.infoString)
          case _ => ""
        }
        ("%f %s".format(resDbl,hexString) + ext,resDbl)
      }
      case _ => ("0x%x (%d bit(s))".format(resBits,data.getWidth), 0.0)             // Double value unused
    }
    if (traceOn && display) println("%s".format(infoStart) + info)
    (outDbl,resBits)
  }
 
  /** Poke ChiselDSP types */
  def poke(node: DSPBool, x: Boolean) : Boolean = {
    val pokeRet = poke(node,int(x),true)
    if (pokeRet > 0) true else false
  }
  def poke(node: DSPUInt, x: BigInt): BigInt = poke(node, x, true)
  def poke(node: DSPDbl, x: Double): Double = poke(node, x, true)
  def poke(node: DSPFixed, x: Double): Double = poke(node, x, true)
  def poke(node: Fixed, x: Double): Double = poke(node, x, true)
  
  /** Poke Chisel types */
  override def poke(node: Bits, x: Boolean) : Unit = poke(node, int(x), true)
  override def poke(node: Bits, x: Int) : Unit = poke(node, BigInt(x), true)
  override def poke(node: Dbl, x: Double) : Unit = poke(node, x, true)
  override def poke(node: Flo, x: Float) : Unit = poke(node, x.doubleValue, true)
  override def poke(node: Bits, x: Long) : Unit = poke(node, BigInt(x), true)
  override def poke(node: Bits, x: BigInt) : Unit = poke(node, x, true)
 
  /** More general poke handling */
  private def poke(node:Bits, x: Double, display:Boolean): Double = {
    node match {
      case d1: Dbl => super.poke(d1,x)
      case d2: DSPDbl => super.poke(node, BigInt(doubleToLongBits(x)))
      case f0: Flo => super.poke(f0,x.floatValue)
      case _: DSPFixed | _: Fixed => super.poke(node, DSPFixed.toFixed(x,_.getFractionalWidth))
    }
    peek(node,display,false)._1
  }
  private def poke(node:Bits, x: BigInt, display:Boolean): BigInt = {
    super.poke(node,x)
    peek(node,display,false)._2
  }
   
  /** Peek at the value of some memory at an index
    * @param data Memory to inspect
    * @param off Offset in memory to look at */
  override def peekAt[T <: Bits](data: Mem[T], off: Int): BigInt = {
    val value = super.peekAt(data,off)
    if (traceOn) println(s"  PEEK ${dumpName(data)}[${off}] -> 0x${value.toString(16)}")
    value
  }
  
  /** Set the value of some memory
    * @param data The memory to write to
    * @param value The BigInt representing the bits to set
    * @param off The offset representing the index to write to memory
    */
  override def pokeAt[T <: Bits](data: Mem[T], value: BigInt, off: Int): Unit = {
    super.pokeAt(data,value,off)
    if (traceOn) println(s"  POKE ${dumpName(data)}[${off}] <- 0x${value.toString(16)}")
  }
  
  /** Step through tester n steps */
  def step() = step(1)
  override def step(n: Int) {
    val newN = t + n
    if (traceOn) println(s"STEP ${n} -> ${newN}")
    super.step(n) 
  }
  
  /** Hold reset for n cycles */
  override def reset(n: Int = 1) {
    if (traceOn) println(s"RESET ${n}")
    super.reset(n)
  }
  
}
