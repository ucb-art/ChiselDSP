package ChiselDSP
import Chisel._

/** LUT IO for Int LUTs --> Vec of DSPBools */
class IntLUT2BoolsIO (depth: Int, boolLength: Int, numPorts: Int) extends IOBundle {
  val addr = Vec(numPorts,DSPUInt(INPUT,(depth-1).max(0)))
  val dout = Vec(numPorts,Vec(boolLength,DSPBool(OUTPUT)))
}

/** Single LUT for a series of data that are addressed by the same signal */
class IntLUT2Bools (ints: List[Int], numPorts: Int = 1, inDelay: Int = 0) extends DSPModule (inputDelay = inDelay) {

  if (numPorts < 1) Error("# of read ports must be >= 1")

  val depth = ints.length
  val max = ints.max
  val boolLength = DSPUInt.toBitWidth(max)
  override val io = new IntLUT2BoolsIO(depth,boolLength,numPorts)
  val LUT = Vec(ints.map(x => DSPUInt(x,max)))
  io.addr.foreach(x => x.doNothing())
  val readBits = Vec(io.addr.map(x => LUT(x.toUInt)))
  // TODO: Mod Vec to take DSPUInt
  val outVecBools = Vec(readBits.map(x => x.toBools))
  val outVecDSPBools = Vec(outVecBools.map(x =>
    Vec(x.map(y => {
      val res = DSPBool(y)
      res.passDelay(io.addr(0),0)
    }))
  ))
  if (depth > 0) io.dout := outVecDSPBools

}