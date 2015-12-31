/** RegNext should support multiple clocks */

package Chisel

object RegNext {
  def apply[T <: Data](next: T): T = Reg[T](Some(next), Some(next), None, None)
  def apply[T <: Data](next: T, init: T): T = Reg[T](Some(next), Some(next), Some(init), None)
  def apply[T <: Data](next: T, clock: Clock): T = Reg[T](Some(next), Some(next), None, Some(clock))
  def apply[T <: Data](next: T, init: T, clock: Clock): T = Reg[T](Some(next), Some(next), Some(init), Some(clock))
}