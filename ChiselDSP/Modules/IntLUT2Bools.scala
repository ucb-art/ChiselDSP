package ChiselDSP
import Chisel._

/** LUT IO for Int LUTs --> Vec of DSPBools */
class IntLUT2BoolsIO (depth: Int, vecLength: Int) extends IOBundle {
  val addr = DSPUInt(INPUT,depth-1)
  val dout = Vec(vecLength,DSPBool(OUTPUT))
}

/** Single LUT for a series of data that are addressed by the same signal */
class IntLUT2Bools (ints: List[Int], inDelay: Int = 0) extends DSPModule (inputDelay = inDelay) {

  val depth = ints.length
  val max = ints.max
  val vecLength = DSPUInt.toBitWidth(max)
  override val io = new IntLUT2BoolsIO(depth,vecLength)
  val LUT = Vec(ints.map(x => DSPUInt(x,max)))
  io.addr.doNothing()
  val readBits = LUT(io.addr.toUInt)
  // TODO: Mod Vec to take DSPUInt
  val outVecBools = readBits.toBools
  val outVecDSPBools = outVecBools.map(x => DSPBool(x))
  outVecDSPBools.foreach(x => x.passDelay(io.addr,0))
  io.dout := outVecDSPBools

}