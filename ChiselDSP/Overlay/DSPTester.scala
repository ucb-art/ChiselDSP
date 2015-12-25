/** Custom DSP tester -- shows values in Ints, Doubles instead of only hex.
  * Handles Chisel data types and ChiselDSP data types.  
  * TODO: Aggregate peek/poke, expect, custom finish. 
  */

package ChiselDSP
import Chisel.{Complex => _, _}
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
  def peek(data: DSPQnm[_]): Double = peek(data,true,true)._1
  
  /** Peek Chisel types */
  def peek(data: Bool): Boolean = if (peek(data,true,true)._2 > 0) true else false
  def peek(data: UInt): BigInt = peek(data,true,true)._2
  def peek(data: SInt): BigInt = peek(data,true,true)._2
  override def peek(data: Dbl): Double = peek(data,true,true)._1
  override def peek(data: Flo): Float = peek(data,true,true)._1.floatValue
  def peek(data: Fixed): Double = peek(data,true,true)._1
  override def peek(data: Bits): BigInt = peek(data,true,true)._2

  /** Peek aggregate elements */
  override def peek(data: Aggregate): Array[BigInt] = data.flatten map (x => peek(x._2,true,true)._2)

  /** Peek complex*/
  def peek(data: ComplexBundle): ScalaComplex = peek(data,true,true)
  private def peek(data: ComplexBundle, display:Boolean, pk:Boolean): ScalaComplex = {
    val res = data.flatten map (x => peek(x._2,false,false)._1)
    val names = data.flatten map (x => dumpName(x._2))
    val name = names.head.replace("_real","")
    val command = if (pk) "PEEK" else "POKE"
    val out = Complex(res.head,res.last)
    if (traceOn && display) println("  %s %s -> %s %s".format(command,name,out.toString,data.Q))
    out
  }

  /** Convenient peek of a Vec of DSPBits */
  def peek[A <: DSPBits[A]](data: Vec[A]): Array[BigInt] = peek(data,true,true)
  private def peek[A <: DSPBits[A]](data: Vec[A], display:Boolean, pk:Boolean): Array[BigInt] = {
    val res = data.flatten.map(x => peek(x._2,false,false)._2).reverse
    val names = data.flatten.map(x => dumpName(x._2)).reverse
    val name = names.head.replace("_0","")
    val command = if (pk) "PEEK" else "POKE"
    if (traceOn && display) println("  %s %s -> [%s]".format(command,name,res.mkString("\t")))
    res
  }

  /** Cluster some peek-specific processing */
  private def peekInit(data: Bits, peek: Boolean): Tuple2[BigInt,String] = {
    val res = super.peek(data)
    val resBits = if (data.isLit || (data.dir == INPUT && data.isTopLevelIO)) signed_fix(data, res) else res
    val command = if (peek) "PEEK" else "POKE"
    val name = if (data.isLit) "*Lit*" else dumpName(data)
    val infoStart = "  %s %s -> ".format(command,name)
    (resBits, infoStart)
  }
  
  /** More general handling of peek for all possible data types */
  private def peek (data: Bits, display: Boolean, peek: Boolean) : Tuple2[Double,BigInt] = {
    val (resBits, infoStart) = peekInit(data,peek)
    val s = if (resBits < 0) "-" else ""
    val hexString = if (hexOn) "(" + s + "0x%x) ".format(resBits.abs) else ""
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
      case _: Fixed | _: DSPFixed => {                                              // Double value unused
        val (resDbl,ext) = data match {
          case f2: DSPFixed => (DSPFixed.toDouble(resBits.longValue, f2.getFracWidth) ," %s".format(f2.infoString))
          case f1: Fixed => (DSPFixed.toDouble(resBits.longValue, f1.getFractionalWidth),"")
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
  def poke(node: DSPQnm[_], x: Double): Double = poke(node, x, true)
  def poke(node: Fixed, x: Double): Double = poke(node, x, true)
  
  /** Poke Chisel types */
  override def poke(node: Bits, x: Boolean) : Unit = poke(node, int(x), true)
  override def poke(node: Bits, x: Int) : Unit = poke(node, BigInt(x), true)
  override def poke(node: Dbl, x: Double) : Unit = poke(node, x, true)
  override def poke(node: Flo, x: Float) : Unit = poke(node, x.doubleValue, true)
  override def poke(node: Bits, x: Long) : Unit = poke(node, BigInt(x), true)
  override def poke(node: Bits, x: BigInt) : Unit = poke(node, x, true)

  /** Poke Complex */
  def poke(node: ComplexBundle, x: ScalaComplex): ScalaComplex = poke(node,x,true)
  private def poke(node: ComplexBundle, x: ScalaComplex, display:Boolean): ScalaComplex = {
    node match {
      case a: Complex[_] => {
        poke (a.real, x.real, false)
        poke (a.imag, x.imag, false)
        peek (node, display, false)
      }
    }
  }

  /** Trace supressed */
  override def poke(data: Aggregate, x: Array[BigInt]): Unit = {
    Status ("  Aggregate poked. Pokes hidden.")
    super.poke(data,x)
  }
 
  /** More general poke handling */
  private def poke(node:Bits, x: Double, display:Boolean): Double = {
    node match {
      case d1: Dbl => super.poke(d1,x)
      case d2: DSPDbl => super.poke(node, BigInt(doubleToLongBits(x)))
      case f0: Flo => super.poke(f0,x.floatValue)
      case f1: Fixed => super.poke(node, DSPFixed.toFixed(x,f1.getFractionalWidth))
      case f2: DSPFixed => super.poke(node, DSPFixed.toFixed(x,f2.getFracWidth))
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
  def step() : Unit = step(1)
  override def step(n: Int) {
    val newT = t + n
    if (traceOn) println(s"STEP ${n}x -> ${newT}")
    super.step(n) 
  }
  
  /** Hold reset for n cycles */
  override def reset(n: Int = 1) {
    if (traceOn) println(s"RESET ${n}")
    super.reset(n)
  }

  /** Expects for UInt, SInt, DSPUInt, etc. (compare peek results with desired values) */
  override def expect(data: Bits, expected:Int): Boolean = expect(data,BigInt(expected))
  override def expect(data: Bits, expected:BigInt): Boolean = expect(data,expected,null,null)
  def expect(data: Bits, expected:BigInt, test: String, error:String): Boolean = {
    if (peek(data,true,true)._2 != expected) handleError(expected.toString, test,error)
    else true
  }


















  /** Expects for aggregates of UInt, SInt, DSPUInt, etc. */
  def expect[A <: Aggregate](data: A, expected: Array[Int]): Boolean = {
    val expectedBI = expected.map(x => BigInt(x))
    expect(data,expectedBI)
  }
  def expect[A <: Aggregate](data: A, expected: Array[BigInt]):Boolean = expect(data,expected,null,null)
  def expect(data: Aggregate, expected: Array[BigInt], test: String, error: String): Boolean = {
    Status("  >>>> Aggregate peek results:")
    val res = peek(data)
    compareExpectedSets(res,expected,test,error)
  }
  def expect[A <: DSPBits[A]](data: Vec[A], expected: Array[BigInt], test: String, error: String): Boolean = {
    val res = peek(data)
    compareExpectedSets(res,expected,test,error)
  }
  private def compareExpectedSets(res:Array[BigInt],expected:Array[BigInt],test:String, error:String) : Boolean = {
    if (!res.sameElements(expected)){
      val exp = "[" + expected.mkString("\t") + "]"
      handleError(exp,test,error)
    }
    else true
  }









  def handleError(exp: String, test: String, error: String): Boolean = {
    println(Console.RED + "  >>>> Does not match " + exp + Console.RESET)
    false
  }

  //def expect (data: Flo, expected: Float): Boolean
  //def expect (data: Dbl, expected: Double): Boolean
  //complex

}
