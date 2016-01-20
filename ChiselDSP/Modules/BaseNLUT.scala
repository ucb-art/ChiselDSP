package ChiselDSP
import Chisel._

// TODO: Make a Mixed Radix LUT

/** Base N LUT IO where
  * depth = depth of LUT
  * rad = base
  * max = maximum value in LUT
  */
class BaseNLUTIO (depth: Int, rad: Int, max: Int) extends IOBundle {
  val addr = DSPUInt(INPUT,depth-1)
  val dout = BaseN(OUTPUT,rad = rad, max = max)
}

/** LUT that calculates Base N outputs from a list of Ints */
class BaseNLUT (ints: List[Int], rad: Int, inDelay: Int = 0) extends DSPModule (inputDelay = inDelay) {

  // TODO: Make 0 length lists have optional IO = None

  val depth = ints.length
  // There should always be at least 1 digit
  val LUTmax = if (depth == 0) rad-1 else ints.max

  override val io = new BaseNLUTIO(depth,rad,LUTmax)

  // data width determined by max of values
  val LUT = Vec(ints.map(x => BaseN.toBits(x,r = rad, max = LUTmax)))
  io.addr.doNothing()
  val temp = if (depth == 0) BaseN(0,rad,LUTmax) else BaseN( LUT(io.addr.toUInt) ,rad)
  io.dout := temp
  io.dout.foreach{x => x.passDelay(io.addr,0)}

}