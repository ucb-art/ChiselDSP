package TBEx
import ChiselDSP._
import Chisel.{Complex => _, _}

/** Module tester that allows switching between fixed and floating point testing */
class TBExTests[T <: TBEx[_ <: DSPQnm[_]]](c: T) extends DSPTester(c) {

  poke(c.i.fix,-.25)

  poke(c.io.addVal,1)
  poke(c.io.upDown,false)
  step(2)

  expect(c.o.fix,-.25)

  poke(c.io.addVal,2)
  step(5)
  poke(c.io.upDown,true)
  poke(c.io.addVal,1)
  step(7)
  expect(c.io.count,251)
  expect(c.o.count3,-5)
  expect(c.o.count4,-5)
  step(7)
  expect(c.io.count,2)
  expect(c.o.count3,2)
  expect(c.o.count4,2)
  reset(5)

  poke(c.io.addVal,1)
  poke(c.io.upDown,false)
  step(2)
  poke(c.io.addVal,2)
  step(5)
  poke(c.io.upDown,true)
  poke(c.io.addVal,1)
  step(7)
  expect(c.io.count,251)
  step(7)
  expect(c.io.count,2)

}
