package ChiselDSP
import Chisel._

/** Memory with the output registered (different from SeqMem since seqRead = false) */
object DSPMem {
  def apply[T <: Data](n: Int, out: => T, outReg: Boolean = true): DSPMem[T] = {
    val gen = out.cloneType
    Chisel.Reg.validateGen(gen)
    new DSPMem(n, gen, outReg)
  }
}

class DSPMem[T <: Data](n: Int, out: T, outReg: Boolean) extends Mem[T](() => out, n, false, false) {
  /** Read from memory address (output delayed 1 clock cycle */
  def read(addr: DSPUInt): T = {
    addr.doNothing()
    val readVal = super.read(addr.toUInt)
    val dout = if (outReg) RegNext(readVal) else readVal
    val dly = if (outReg) 1 else 0
    (dout,out) match {
      case (b1: DSPBits[_], b2: DSPBits[_])  => {
        b1.copyInfo(b2)
        b1.passDelay(addr,dly)
        b1.assign()
      }
      case (c1: Complex[_], c2: Complex[_]) => {
        c1.real.copyInfo(c2.real)
        c1.imag.copyInfo(c2.imag)
        c1.real.assign()
        c1.imag.assign()
        c1.passDelay(addr,dly)
      }
      case _ =>
    }
    dout
  }
  override def read(addr: UInt): T = {Error("Read address should be of type DSPUInt"); out}
  /** Write to memory address */
  def write(addr: DSPUInt, dataIn: T, en: DSPBool): Unit = {
    addr.doNothing()
    doWrite(addr.toUInt, en.toBool, dataIn, None)
  }
  def write(addr: DSPUInt, dataIn: T): Unit = write(addr,dataIn,DSPBool(true))
  override def write(addr: UInt, dataIn: T): Unit = {Error("Write address should be of type DSPUInt"); out}

}