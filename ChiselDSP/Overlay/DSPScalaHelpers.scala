/** Scala type conversion helpers */

package ChiselDSP
import Chisel._

object List2Tuple {
  /** List (with 2 values) to Tuple2 */
  def apply (x: List[BigInt]) : Tuple2[BigInt,BigInt] = {
    if (x.length != 2) Error("List2Tuple expects list of length 2.")
    (x.head,x.last)
  }
}

// TODO: Which is better? ZeroPadLit as string or shift by "digit width" for concatenation?
/** Returns a string with a Lit padded (with 0's) to the desired bitwidth */
object ZeroPadLit{
  def apply(x: BigInt,width:Int): String = {
    val stringVal = x.toString(2)
    if (stringVal.length > width) Error("Width cannot be smaller than length of x in radix-2 for zero padding")
    List.fill(width - stringVal.length)("0").mkString("").concat(stringVal)
  }
  def apply(x: Bits): String = {
    if (!x.isLit) Error("ZeroPadLit only works on literals!")
    val width = x.getWidth
    val stringVal = x.litValue().toString(2)
    List.fill(width - stringVal.length)("0").mkString("").concat(stringVal)
  }
}
