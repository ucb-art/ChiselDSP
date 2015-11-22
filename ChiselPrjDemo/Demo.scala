/* Exploring Chisel + Scala functionality */

package Demo

import Chisel._
import ChiselDSP._

/** See more extensive use-case in ChiselEnvironment/ChiselDSP/Modules/Counters.scala */
case class DemoParams (
  iMax: Int = 10,
  includeReset: Boolean = false
)

class DemoIO (myParams: DemoParams) extends IOBundle {
  val a = MyUInt(INPUT,myParams.iMax)
  val b = MyUInt(INPUT,myParams.iMax)
  val c = MyUInt(OUTPUT,2*myParams.iMax)
  val d = Vec.fill(3){MyUInt(OUTPUT,2*myParams.iMax)}
  val reset = if (myParams.includeReset) MyBool(INPUT) else MyBool(false) 
}

class Demo [ T <: Bits with MyNum[T] ](gen : => T, myParams: DemoParams) extends DSPModule (gen) {
  
  val x = new DemoIO(myParams); createIO(x)
  
  class ComplexIO extends IOBundle {
    val i_real = gen.asInput
    val i_imag = gen.asInput
    val o_real = gen.asOutput
    val o_imag = gen.asOutput
    val x = Dbl(INPUT)
  }
  
  val y = new ComplexIO; createIO(y)
  
  x.c := x.a + x.b
  
  /** Easily debug signals that aren't connected to output ports */
  val inner = x.a * x.b
  debug(inner)
  
  y.o_imag := y.i_real * double2T(0.5)
  y.o_real := y.i_imag * double2T(-1)
  
  val d = Vec.fill(3){MyUInt(OUTPUT,2*myParams.iMax)}
  
  val CounterTest = (0 until 3).map(x => ModCounter(10,4,"TestCounterName") )        
	CounterTest.zipWithIndex.foreach{ case(r,i) => {
	  r.x.inc := MyUInt(2)
	  r.x.modN := MyUInt(3)
	  r.iCtrl.change := MyBool(true)
	  r.iCtrl.reset := MyBool(x.reset)
	  r.iCtrl.wrap := MyBool(false)
	  d(i) := r.x.out
	  }
	}
	
	x.d := d
  
}

object Demo {
  def main(args: Array[String]): Unit = {
    val demoArgs = args.slice(1, args.length)
    val myParams = DemoParams(iMax = 20)    
    chiselMainTest(demoArgs, () => MyModule( new Demo[MyDbl]({MyDbl()}, myParams) )) {
      c => new DemoTests(c) 
    }
  }
}

class DemoTests[T <: Demo[_ <: Bits with MyNum[_]] ](c: T)  extends DSPTester(c) {

  poke(c.x.a,1)
  poke(c.x.b,4)
  step(1)
  peek(c.inner)
  peek(c.x.c)
  peek(c.x.d)
  peek(c.y.o_real)
  peek(c.y.o_imag)
  poke(c.y.x,0.5)

}


