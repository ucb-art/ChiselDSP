package T____
import ChiselDSP._
import Chisel.{Complex => _, _}

/** Module tester that allows switching between fixed and floating point testing */
class T____Tests[T <: T____[_ <: DSPQnm[_]]](c: T) extends DSPTester(c) {

}