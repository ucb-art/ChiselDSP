package ChiselDSP
import scala.io.Source

object File2DblArray {
  /** Extract double array elements from file */
  def apply (fileName: String) : Array[Double] = {
    Source.fromFile(fileName).getLines.toList.map(_.toDouble).toArray  
  }
}

object File2IntArray {
  /** Extract integer array elements from file */
  def apply (fileName: String) : Array[Int] = {
    Source.fromFile(fileName).getLines.toList.map(_.toInt).toArray  
  }
}

object Data1D2File{
  /** 1D Int List to File */
  def apply(data: => List[Int],fileName: String) {
    val str = data.mkString("\n")
    scala.tools.nsc.io.File(fileName).writeAll(str)
  }
  /** 1D Dbl List to File */
  def apply(data: List[Double],fileName: String) {
    val str = data.mkString("\n")
    scala.tools.nsc.io.File(fileName).writeAll(str)
  }
}

object Data2D2File{
  /** 2D Int List to CSV */
  def apply(data: => List[List[Int]],fileName: String) {
    val str = data.map(n => n.mkString(",")).mkString("\n")
    scala.tools.nsc.io.File(fileName).writeAll(str)
  }
  /** 2D Dbl List to CSV */
  def apply(data: List[List[Double]],fileName: String) {
    val str = data.map(n => n.mkString(",")).mkString("\n")
    scala.tools.nsc.io.File(fileName).writeAll(str)
  }

}


    
