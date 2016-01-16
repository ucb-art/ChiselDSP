package ChiselDSP
import Chisel.{Complex => _, _}

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
  * @param seqRd is true when read output is delayed 1 clock cycle else false when the read is combinational
  * @tparam T
  */
class Memory[T <: Data](gen : => T, depth: Int, seqRd: Boolean = true, val conflictHandling: Boolean = true)
      extends DSPModule {

  val delay = if (seqRd) 1 else 0

  override val io = new MemIO(gen,depth,conflictHandling)
  val mem = if (seqRd) SeqMem(depth, gen) else Mem(depth, gen)
  // Note for sequential read, read address (not data) is delayed
  val dOut = mem.read(io.rAddr.toUInt)
  dOut match {
    case d: DSPBits[_] => d.passDelay(io.rAddr,delay)
    case c: Complex[_] => c.passDelay(io.rAddr,delay)
    case _ =>
  }
  // Sequential write with enable
  when (io.WE.toBool) {
    mem.write(io.wAddr.toUInt, io.dIn)
  }

  // Conflict handling for sequential read where passThrough = whether to pass write data to
  // read out on the next clock cycle if write and read addresses are the same.
  if (seqRd && conflictHandling){
    val inDly = Pipe(io.dIn,delay)
    val reroute = ((io.rAddr === io.wAddr) & io.WE & io.passThrough.get).pipe(delay)
    io.dOut := Mux(reroute,inDly,dOut)
  }
  else io.dOut := dOut

}