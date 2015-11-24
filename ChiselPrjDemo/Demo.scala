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

case class DemodParams (
  demodMax: Int = 3,            // QPSK = 2 bits --> Range [0,3]
  frameSize: Int = 1000,        // Filler for now
  soft: Boolean = false,        // Hard/soft demod
  llrMax: Int = 1,              // Log likelihood max (hard demod by default)
  inBits: Int = 16,             // When switching to MyFixed, # of bits MyFixed should be (filler until I get MyFixed working)
  inNormalizeMax: Int = 1000    // Need some kind of normalization parameter for stuff more complicated than QPSK
)

class DemodIO[T <: Bits with MyNum[T] ](gen : => T, demodParams : DemodParams = DemodParams()) extends IOBundle {
  // Should either by signed MyFixed or MyDbl, set by gen
  val complexIn = MyComplex(gen,gen).asInput
  // When llrMax is 1, this is a hard decoder- i.e. the output is either a 1 or 0
  // When llrMax is bigger, this is a soft decoder, i.e. a large positive number is a very confident 0
  // and a small positive number is a less confident 0, negative numbers for 1s
  // (for this demod, we chose the sign of the LLR as positive <-> 0 bit b/c then the 2's complement sign
  //  bit of the LLR is the same as the hard decoder decision)
  // People aren't generally consistent about choosing positive LLRs to correspond to 0 or 1, so we choose
  // one with a conventient interpretation in this context
  // Start with the hard decoder
  // For hard QPSK, QAM demodMax will be different -- essentially the logic performs successive quaternary search
  val demodOut = Vec.fill(log2Up(demodParams.demodMax)) { MyBool(OUTPUT) } // only hard decisions, should really be a MyFixed with width = log2Up(demodParams.llrMax + 1)

  // Offset of input sample
  val offsetIn = MyUInt(INPUT,demodParams.frameSize-1)
  // If demodOut comes out n cycles after current complexInn, offsetOut should be offsetIn delayed n clocks (use pipe)
  val offsetOut = MyUInt(OUTPUT,demodParams.frameSize-1)
}

class Demo [ T <: Bits with MyNum[T] ](gen : => T, myParams: DemoParams, demodParams: DemodParams) extends DSPModule (gen) {
  val demodIO = new DemodIO(gen, demodParams)

  val x = new DemoIO(myParams)

  class ComplexIO extends IOBundle {
    val myDblTest = MyDbl(INPUT)
    val i_real = gen.asInput
    val i_imag = gen.asInput
    val o_real = gen.asOutput
    val o_imag = gen.asOutput
    val i = Vec(3,gen.asInput)
    val o = Vec(3,gen.asOutput)
    val ctrl = MyBool(INPUT)
    val x = Dbl(INPUT)
    val ctrlO = MyBool(OUTPUT)
    val r = gen.asOutput
    val t = new DemodIO(gen, demodParams)
  }

  val y = new ComplexIO()

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
  }}

  x.d := d

  val dblV2 = MyDbl(3.33) + y.myDblTest; debug(dblV2)
  val dblV1 = Dbl(3.44); debug(dblV1)

  y.ctrlO := MyBool(true)

}

object Demo {
  def main(args: Array[String]): Unit = {
    val demoArgs = args.slice(1, args.length)
    val myParams = DemoParams(iMax = 20)
    val demodParams = DemodParams()
    chiselMainTest(demoArgs, () => MyModule( new Demo({MyDbl()}, myParams, demodParams) )) {
      c => new DemoTests(c)
    }
  }
}

class DemoTests[T <: Demo[_ <: Bits with MyNum[_]] ](c: T)  extends DSPTester(c) {
  poke(c.y.t.complexIn.real,1.5)
  myPeek(c.y.t.demodOut(0))
  myPeek(c.y.t.demodOut(1))
  poke(c.demodIO.complexIn.real,1.5)
  poke(c.demodIO.complexIn.imag,-1)
  poke(c.y.myDblTest,0.5)
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


