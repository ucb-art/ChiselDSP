package ChiselDSP
import Chisel._

/** Mixed Base LUT IO where
  * depth = depth of LUT
  * bases_max = (output bases, associated max values)
  */
class MixedBaseLUTIO (depth: Int, bases_max:List[(Int,Int)]) extends IOBundle {
  val addr = DSPUInt(INPUT,(depth-1).max(0))
  val dout = Vec(bases_max.map(x => BaseN(OUTPUT,rad = x._1, max = x._2)))
}

/** LUT that calculates Base X outputs from a list of (element, base) */
class MixedBaseLUT (elems: List[(Int,Int)], inDelay: Int = 0) extends DSPModule (inputDelay = inDelay) {

  // Get used bases and the max values associated with each base in the LUT (if max = 0, set it so
  // the output will at least be 1 digit wide)
  val usedBases = elems.map(_._2).distinct
  val bases_maxTemp = usedBases.zip(usedBases.map(x => elems.filter(_._2 == x).map(_._1).max))
  // Get rid of base 1 (always 0) for IO
  val bases_max = bases_maxTemp.map(x => {
    val newMax = if(x._2 == 0) x._1 - 1 else x._2
    (x._1,newMax)
  }).filter(_._1 != 1)

  val depth = elems.length

  override val io = new MixedBaseLUTIO(depth,bases_max)

  // Nominal # of bits to represent each element, maximum needed bits, LUT with guaranteed
  // correct padding
  val LUTNomBits = elems.map(x => {
    // Separate out case when base = 1 (trivial, but not supported by BaseN)
    if (x._2 == 1) UInt(0,width=1)
    else BaseN.toBits(x._1,r = x._2)
  })
  val maxBits = LUTNomBits.map(_.getWidth).max
  val LUT = Vec(LUTNomBits.map(x => {
    // TODO: Make helper function?
    val entry = ZeroPadLit(x.litValue(), maxBits)
    Lit("b".concat(entry), entry.length) {UInt(width = entry.length)}
  }))
  io.addr.doNothing()
  val LUTOut = LUT(io.addr.toUInt)

  // Match bitlength per base output requirement
  io.dout.foreach{x => {
    val padding = (x.bitWidth-LUTOut.getWidth).max(0)
    val temp = Cat( Fill(padding,UInt(0,width=1)),LUTOut ).toBits
    x := BaseN(temp,x.rad)
  }}
  io.dout.flatten.foreach{x => x._2.asInstanceOf[DSPUInt].passDelay(io.addr,0)}

}
