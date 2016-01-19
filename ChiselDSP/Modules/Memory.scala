package ChiselDSP
import Chisel._

// TODO: Support multiple read/write ports, try multiple clock domains, check combinational read, Register file

/** Memory IO where passThrough = whether to pass write data to read out on the next clock cycle if write
  * and read addresses are the same.
  */
class MemIO[T <: Data](gen : => T, depth: Int, conflictHandling: Boolean = true) extends IOBundle {
  val rAddr = DSPUInt(INPUT,depth-1)
  val wAddr = DSPUInt(INPUT,depth-1)
  val dIn = gen.cloneType.asInput
  val dOut = gen.cloneType.asOutput
  val WE = DSPBool(INPUT)
  val passThrough = if (conflictHandling) Some(DSPBool(INPUT)) else None
}

/** Single read/write port memory module where
  * @param gen is the type of data stored in memory
  * @param depth is the depth of the memory
  * @param outReg is true when read output is delayed 1 clock cycle else false when the read is combinational
  * @tparam T
  */
class Memory[T <: Data](gen : => T, depth: Int, outReg: Boolean = true, val conflictHandling: Boolean = true)
      extends DSPModule {

  val delay = if (outReg) 1 else 0

  override val io = new MemIO(gen,depth,conflictHandling)
  val mem = DSPMem(depth,gen,outReg)
  // Output is registered if outReg is true
  val dOut = mem.read(io.rAddr)
  // Sequential write with enable
  mem.write(io.wAddr, io.dIn, io.WE)

  // Conflict handling for sequential read where passThrough = whether to pass write data to
  // read out on the next clock cycle if write and read addresses are the same.
  val inDly = Pipe(io.dIn,delay)
  val reroute = ((io.rAddr === io.wAddr) & io.WE & io.passThrough.getOrElse(DSPBool(false))).pipe(delay)
  io.dOut := Mux(reroute,inDly,dOut)

}