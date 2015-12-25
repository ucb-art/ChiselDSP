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
