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
  val d = Vec(3,MyUInt(OUTPUT,2*myParams.iMax))
  val reset = if (myParams.includeReset) MyBool(INPUT) else MyBool(false) 
}

class Demo [ T <: Bits with MyNum[T] ](gen : => T, myParams: DemoParams) extends DSPModule (gen) {
  
  val x = new DemoIO(myParams); createIO(x)
  
  class ComplexIO extends IOBundle {
    val i_real = gen.asInput
    val i_imag = gen.asInput
    val o_real = gen.asOutput
    val o_imag = gen.asOutput
    val i = Vec(3,gen.asInput)
    val o = Vec(3,gen.asOutput)
    val ctrl = MyBool(INPUT)
    val x = Dbl(INPUT)
    val ctrlO = MyBool(OUTPUT)
  }
  
  val y = new ComplexIO; createIO(y)
  
  y.o := MyPipe(y.i,5)
  
  x.c := x.a + x.b
  
  /** Easily debug signals that aren't connected to output ports */
  val inner = x.a * x.b
  debug(inner)
  
  y.o_imag := MyMux(y.i_real,y.i_imag,y.ctrl) * double2T(0.5) + double2T(-2.3)
  y.o_real := (y.i_imag >> 3) * double2T(-1) + double2T(3.3)
  
  val d = Vec(3,MyUInt(OUTPUT,2*myParams.iMax))
  
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
	
	val dblV2 = MyDbl(3.33); debug(dblV2)
	val dblV1 = Dbl(3.44); debug(dblV1)
	
	y.ctrlO := MyBool(true)
  
}

object Demo {
  def main(args: Array[String]): Unit = {
    val demoArgs = args.slice(1, args.length)
    val myParams = DemoParams(iMax = 20)    
    chiselMainTest(demoArgs, () => MyModule( new Demo({MyDbl()}, myParams) )) {
      c => new DemoTests(c) 
    }
  }
}

class DemoTests[T <: Demo[_ <: Bits with MyNum[_]] ](c: T)  extends DSPTester(c) {

  poke(c.y.i_real,0.5)
  poke(c.y.i_imag,-.2)
  poke(c.x.a,1)
  poke(c.x.b,4)
  step(1)
  myPeek(c.inner)
  myPeek(c.x.c)
  //peek(c.x.d)
  myPeek(c.y.o_real)
  myPeek(c.y.o_imag)
  poke(c.y.x,0.5)
  myPeek(c.dblV1)
  myPeek(c.dblV2)
  myPeek(c.y.ctrlO)
  //peek(c.y.i)
  //peek(c.y.o)
  
}


