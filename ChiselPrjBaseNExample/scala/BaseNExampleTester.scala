package BaseNExample
import ChiselDSP._
import Chisel.{Complex => _, _}

/** Module tester that allows switching between fixed and floating point testing */
class BaseNExampleTests[T <: BaseNExample[_ <: DSPQnm[_]]](c: T) extends DSPTester(c) {

  base = 2
  peek(c.t1)
  peek(c.t2)
  peek(c.t3)
  peek(c.t4)
  peek(c.t5)
  c.t1.flatten.map( x => peek(x._2))
  peek(c.t6)
  peek(c.t7)
  peek(c.t8)
  peek(c.t9)
  peek(c.t10)
  peek(c.x.io.dout)
  peek(c.t11)
  peek(c.t12)
  peek(c.t13)
  peek(c.t14)
  peek(c.t15)
  peek(c.t16)
  peek(c.t17)
  peek(c.t18)
  peek(c.t19)

  peek(c.lut.io.dout)
}
