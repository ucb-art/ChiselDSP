/** Double class customizations */

package ChiselDSP
import Chisel._
import Node.fixWidth

object MyDbl {

  /** Creates a MyDbl object from a constant Double */
  def apply(x: Double): MyDbl = {
    val res = Lit(java.lang.Double.doubleToLongBits(x), 64){ MyDbl() }
    res.asDirectionless
    res
  }

  /** Creates a MyDbl object with specified IO direction */
  def apply(dir: IODirection = NODIR): MyDbl = {
    val res = new MyDbl();
    res.dir = dir;
    res.init("", fixWidth(64))
    res
  }
  
}

class MyDbl extends Bits with MyNum[MyDbl] {

  /** Print MyDbl width */
  def printWidth() : String = ("64-bit double")

  type T = MyDbl
  
  /** Convert Bits to a MyDbl by reinterpreting the Bits */
  private def toMyDbl(x: Bits) : MyDbl = chiselCast(x){MyDbl(x.dir)}

  override def fromNode(n: Node) = MyDbl(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
  
  override protected def colonEquals(that : Bits): Unit = that match {
    case d: MyDbl => super.colonEquals(d)
    case _ => illegalAssignment(that)
  }
  
  private var noUpdate = false  
  
  /** Marks the signal as being used to prevent future updates */
  private def used(): Unit = (noUpdate = true)   
  
  /** Returns the signal. Marks it as used */
  def doNothing() : MyDbl = {this.used(); this}
  
  /** Convert an Integer to MyDbl representation */
  override def fromInt(x: Int): this.type = MyDbl(x.toDouble).asInstanceOf[this.type]
  
  /** Ops using Dbl backend */
  def +   (b: MyDbl): MyDbl = {this.used(); b.used(); newBinaryOp(b, "d+")}
  def -   (b: MyDbl): MyDbl = {this.used(); b.used(); newBinaryOp(b, "d-")}
  def *   (b: MyDbl): MyDbl = {this.used(); b.used(); newBinaryOp(b, "d*")}
  def /   (b: MyDbl): MyDbl = {this.used(); b.used(); newBinaryOp(b, "d/")}
  def === (b: MyDbl): MyBool = {this.used(); b.used(); MyBool(newLogicalOp(b, "d=="))}
  def =/= (b: MyDbl): MyBool = {this.used(); b.used(); MyBool(newLogicalOp(b, "d!="))}
  def >   (b: MyDbl): MyBool = {this.used(); b.used(); MyBool(newLogicalOp(b, "d>"))}
  def <   (b: MyDbl): MyBool = {this.used(); b.used(); MyBool(newLogicalOp(b, "d<"))}
  def <=  (b: MyDbl): MyBool = {this.used(); b.used(); MyBool(newLogicalOp(b, "d<="))}
  def >=  (b: MyDbl): MyBool = {this.used(); b.used(); MyBool(newLogicalOp(b, "d>="))}
  def %   (b: MyDbl): MyDbl = {this.used(); b.used(); newBinaryOp(b, "d%")}
  def unary_-(): MyDbl = {this.used(); MyDbl(0.0)-this}
  
  /** Right shift n --> this/2^n */
  override def >> (n: Int) : MyDbl = {this.used(); this/MyDbl(math.pow(2,n))}
    
  /** Left shift n --> this*2^n */
  def << (n: Int) : MyDbl = {this.used(); this*MyDbl(math.pow(2,n))}
    
  /** Right shift variable n --> this/2^n */
  def >> (n: MyUInt) : MyDbl = {
    this.used(); n.used()
    if (n.getMax > 64) throwException("Can't divide by more than 2^64")
    val shiftLUT = Vec((0 until 65).map( x => MyDbl(math.pow(2,x))))
    this /shiftLUT(n.toUInt)
  }
  
  /** Left shift variable n --> this*2^n */
  def << (n: MyUInt) : MyDbl = {
    this.used(); n.used()
    if (n.getMax > 64) throwException("Can't multiply by more than 2^64")
    val shiftLUT = Vec((0 until 65).map( x => MyDbl(math.pow(2,x))))
    this*shiftLUT(n.toUInt)
  }
  
  /** Delay n clock cycles */
  def pipe (n: Int) : MyDbl = {this.used(); ShiftRegister(this,n)}
  
  /** Register */
  def reg(): MyDbl = {this.used(); Reg(next = this)}

  /** select ? this : 0 */
  override def ? (select: MyBool) : MyDbl = {this.used(); select.used(); toMyDbl(this & Fill(64,select.toBool))}
  
  /** Bitwise-OR. Use case: Mux, etc. with the expectation that one input is 0 */ 
  def /| (b: MyDbl) : MyDbl = {this.used(); b.used(); toMyDbl(this.toBits | b.toBits)}
  
  /** Change INPUT to OUTPUT and OUTPUT to INPUT. NODIR stays the same. */
  override def flip: this.type = {
    dir match {
      case INPUT => dir = OUTPUT
      case OUTPUT => dir = INPUT
      case NODIR => dir = NODIR
    }
    this
  }
  
}
