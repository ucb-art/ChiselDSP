/** Custom Num[T]. Not all operators should be classified as legal
  * "Num" operators, but they're placed here (instead of in Bits) for ease of customization.
  */
  
package ChiselDSP
import Chisel._

abstract trait MyNum[T <: Data] {

  def << (n: Int): T
  def >> (n: Int): T
  def << (n: MyUInt): T
  def >> (n: MyUInt): T
  def ? (s: MyBool) : T
  def pipe (n: Int) : T
  def /| (b: T) : T
  
  def unary_-(): T
  def +  (b: T): T
  def *  (b: T): T
  def /  (b: T): T
  def %  (b: T): T
  def -  (b: T): T
  def <  (b: T): MyBool
  def <= (b: T): MyBool
  def >  (b: T): MyBool
  def >= (b: T): MyBool
  
}
