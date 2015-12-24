/** Scala type conversion helpers */

package ChiselDSP

object List2Tuple {
  /** List (with 2 values) to Tuple2 */
  def apply (x: List[BigInt]) : Tuple2[BigInt,BigInt] = (x(0),x(1))
}
