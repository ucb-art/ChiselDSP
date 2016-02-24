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
  * @param seqRead is trea when the read address is delayed 1 clock cycle
  * @tparam T
  */
class Memory[T <: Data](gen : => T, depth: Int, outReg: Boolean = true, seqRead: Boolean = false,
                        val conflictHandling: Boolean = true,
                        inDelay: Int = 0) extends DSPModule(inputDelay = inDelay) {

  val preDly = (if (seqRead) 1 else 0)
  val postDly = (if (outReg) 1 else 0)
  val delay = preDly + postDly

  override val io = new MemIO(gen, depth, conflictHandling)

  val (dOut,wAddr,dIn,we) = {
    if (!seqRead) {
      // Out last by either 0 or 1 clock
      val mem = DSPMem(depth, gen, outReg)
      // Sequential write with enable
      mem.write(io.wAddr, io.dIn, io.WE)
      // Output is registered if outReg is true
      val out = mem.read(io.rAddr)
      (out,io.wAddr,io.dIn,io.WE)
    }
    else {
      // TODO: Make custom SeqMem that passes delay, range, etc.
      // Out late by 1 or 2 clocks
      val mem = SeqMem(depth, gen)
      val wAddrNew = io.wAddr.pipe(1)
      val dInNew = Pipe(io.dIn,1)
      val WENew = io.WE.pipe(1)
      mem.doWrite(wAddrNew.toUInt,WENew.toBool,dInNew,None)
      val temp = mem.read(io.rAddr.toUInt)
      val out = if (outReg) Pipe(temp,1) else temp
      (out,wAddrNew,dInNew,WENew)
    }
  }

  // Force reset signal to exist for module
  val passThroughTemp = io.passThrough.getOrElse(DSPBool(false)) & DSPBool(~reset)

  // Conflict handling for sequential read where passThrough = whether to pass write data to
  // read out on the next clock cycle if write and read addresses are the same.
  val inDly = Pipe(dIn,postDly)
  val reroute = ((wAddr === io.rAddr.pipe(preDly)) & we & passThroughTemp.pipe(preDly))
  io.dOut := Mux(reroute.pipe(postDly),inDly,dOut)

}