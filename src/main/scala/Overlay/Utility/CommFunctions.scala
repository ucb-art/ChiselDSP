package ChiselDSP

object SER {
  /** Compare two lists of equal sizes, and find the ratio of mismatched elements over # of elements */
  def apply (a: List[Int], b: List[Int]) : Double = {
    if (a.length != b.length) Error("To compute SER, lists a and b should be the same length!")
    val numMismatches = a.zip(b).map{case (ae,be) => {if (ae != be) 1 else 0}}.sum 
    numMismatches.toDouble/a.length
  }
}