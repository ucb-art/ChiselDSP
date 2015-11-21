package ChiselDSP
import Chisel._

object NODIR extends IODirection{
  def apply() = null
  override def toString = "NODIR"
}

/*

object MyFixed {
  def apply(x: Double, params: (Int,Int)) : Unit = {}
}

class MyFixed(var fractionalWidth:Int = -1) extends Bits with MyNum[MyFixed] {

  override def >> (n:Int) : MyFixed = {this}
  override def ? (n:MyBool) : MyFixed = {this}
  def fromInt(x: Int):this.type = this
  def /| (b: MyFixed) : MyFixed = this
  def >> (b:MyUInt):MyFixed = this
  def <<(b:MyUInt):MyFixed = this
  def <<(n:Int):MyFixed = this
  def pipe(n:Int):MyFixed = this
  def / (b:MyFixed):MyFixed = this
  def > (b:MyFixed):MyBool = MyBool(true)
  def >= (b:MyFixed):MyBool =MyBool(true)
  def < (b:MyFixed):MyBool= MyBool(true)
  def <= (b:MyFixed):MyBool = MyBool(true)
  def - (b:MyFixed):MyFixed = this
  def % (b:MyFixed):MyFixed = this
  def + (b:MyFixed):MyFixed = this
  def * (b:MyFixed):MyFixed = this
  def unary_-():MyFixed = this
}*/
