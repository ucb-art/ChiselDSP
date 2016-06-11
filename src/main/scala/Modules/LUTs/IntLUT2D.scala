package ChiselDSP
import Chisel._

/** LUT IO for 2D Int LUTs */
class IntLUT2DIO (depth: Int, colMax: List[Int]) extends IOBundle {
  val addr = DSPUInt(INPUT,(depth-1).max(0))
  val dout = Vec(colMax.map(x => DSPUInt(OUTPUT,x)))
}

/** Single LUT for a series of data that are addressed by the same signal */
class IntLUT2D (ints: List[List[Int]], inDelay: Int = 0) extends DSPModule (inputDelay = inDelay) {

  val depth = ints.length
  // First index of ints is depth, second is columns
  // Get the max of each column and figure out how many bits are needed to represent each max
  val colMax = ints.transpose.map( x => x.max)
  val colBits = colMax.map(x => DSPUInt.toBitWidth(x))
  override val io = new IntLUT2DIO (depth, colMax)
  val LUT = Vec(ints.map(x => {
    // Concatenate elements of a row so that the zero-indexed element is the right-most
    val row = x.zipWithIndex.map{ case (e,i) => {ZeroPadLit(DSPUInt(e,colMax(i)))}}
    val entry = row.tail.foldLeft(row.head)((b,a) => a.concat(b))
    Lit("b".concat(entry),entry.length){UInt(width=entry.length)}
  }))
  io.addr.doNothing()
  val readBits = LUT(io.addr.toUInt)

  // Example: if ColBits = 2,3,4
  // Then colBitsSums = 2,5,9
  // colMaxIdx = 1,4,8 = colBitsSums - 1
  // colMinIdx = 0,2,5 = colBitsSums - colBits
  // Break bits into Vec of DSPUInts via readBits(1,0) , readBits(4,2), readBits(8,4)
  val colBitsSums = colBits.tail.scanLeft(colBits.head)(_ + _)
  val colMaxIdx = colBitsSums.map(_ - 1)
  val colMinIdx = colBitsSums.zip(colBits).map{case (h,l) => h - l}
  val out = colMaxIdx.zip(colMinIdx).zipWithIndex.map{ case (bits,i) => DSPUInt(readBits(bits._1,bits._2), colMax(i))}
  out.foreach(x => x.passDelay(io.addr,0))
  if (depth > 0) io.dout := out

}

// TODO: Use in LUT
object IntLUT2DHelper{
  // Sizes the output Vec based off of inputs
  def getOutputType (ints: List[List[Int]]) : Vec[DSPUInt] = {
    val colMax = ints.transpose.map( x => x.max)  
    Vec(colMax.map(x => DSPUInt(OUTPUT,x)))
  }
}