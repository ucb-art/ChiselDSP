package BaseNExample

// ------- Imports START -- DO NOT MODIFY BELOW
import Chisel.{Complex => _, Mux => _, Reg => _, RegNext => _, RegInit => _, Pipe => _, Mem => _, SeqMem => _,
               Module => _, ModuleOverride => _, when => _, switch => _, is => _, unless => _, Round => _,  _}
import ChiselDSP._
// ------- Imports END -- OK TO MODIFY BELOW

/** Module that supports both fixed and floating point testing */
class BaseNExample[T <: DSPQnm[T]](gen : => T) extends GenDSPModule (gen) {

  override val io = new IOBundle {
    val in1 = DSPUInt(INPUT,3)
  }

  println(BaseN.toIntList(14,3))
  val t1 = BaseN(14,3,242)
  println("t1 " + t1(0).isLit)
  debug(t1)
  val t2 = BaseN(14,3)
  debug(t2)
  val t3 = BaseN.toBits(14,3,242)
  debug(t3)
  val t4 = BaseN.toBits(14,3)
  debug(t4)
  val t5 = BaseN(t3,3)
  debug(t5)

  val t6 = BaseN(13,3,242) === t1
  debug(t6)
  val t7 = BaseN(7,4,32)
  debug(t7)
  val t8 = t7 + BaseN(2,4,32)
  debug(t8)
  val t9 = BaseN(4,3,242) + BaseN(10,3,242)
  debug(t9)

  val t10 = BaseN(23,4,32).toRad42()
  debug(t10)

  val x = DSPModule(new BaseNLUT(List(14,13),3))
  x.io.addr := DSPUInt(0)

  val t11 = x.io.dout.mask(DSPUInt(2))
  debug(t11)


}
