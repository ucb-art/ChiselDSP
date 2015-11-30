/* Exploring Chisel + Scala functionality */

package Demo

import org.json4s._
import org.json4s.native.JsonMethods._
import scala.collection.mutable.Map



import Chisel._
//import ChiselDSP._

import ChiselDSP._

/** See more extensive use-case in ChiselEnvironment/ChiselDSP/Modules/Counters.scala */
case class DemoParams (
  iMax: Int = 10,
  includeReset: Boolean = false
)



case class JSONParams (
  FFTSizes: List[Int],
  Throughput: Double 
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
  llrMax: Int = 1000,           // Log likelihood for soft (filer, might change name, type, etc.)
  inBits: Int = 16,             // When switching to MyFixed, # of bits MyFixed should be (filler until I get MyFixed working)
  inNormalizeMax: Int = 1000    // Need some kind of normalization parameter for stuff more complicated than QPSK
)

class Demo [ T <: MyBits with MyNum[T] ](gen : => T, myParams: DemoParams, demodParams: DemodParams) extends DSPModule (gen, true) {

  class DemodIO extends IOBundle {
    val complexIn = MyComplex(gen,gen).asInput                                          // Should either by signed MyFixed or MyDbl passed in through gen
    val demodOut = MyUInt(OUTPUT,demodParams.demodMax)                                  // For hard QPSK, QAM demodMax will be different -- essentially the logic performs successive quaternary search
    val offsetIn = MyUInt(INPUT,demodParams.frameSize-1)                                // Offset of input sample 
    val offsetOut = MyUInt(OUTPUT,demodParams.frameSize-1)                              // If demodOut comes out n cycles after current complexInn, offsetOut should be offsetIn delayed n clocks (use pipe)
    val llrOut = if (demodParams.soft) MyUInt(OUTPUT,demodParams.llrMax) else MyUInt(0) // Use for a soft demod. Don't use for hard demod.
  }
  val demodIO = new DemodIO()
  val demodIOo = new DemodIO().flip
  
  val x = new DemoIO(myParams)
  
  class ComplexIO extends IOBundle {
    val myDblTest = MyDbl(INPUT)
    val i_real = T(INPUT,(20,20))
    val i_imag = gen.asInput
    val o_real = gen.asOutput
    val o_imag = gen.asOutput
    val i = Vec(3,gen.asInput)
    val o = Vec(3,gen.asOutput)
    val ctrl = MyBool(INPUT)
    val x = Dbl(INPUT)
    val ctrlO = MyBool(OUTPUT)
    val r = gen.asOutput
    //val t = new DemodIO()
    
    val n = SInt(INPUT,width=5)
    val m = SInt(OUTPUT,width=5)
    
    
  }
  
  case class test (
    var t:Int = 0,
    var s:Int = 1
  )
  
  var s = test(2,3)
  s.t = 4
  println(s)
  
  var www = Map("a" -> 3)
  www("a") = 10
  println(www)
  
  /*class xxx extends DSPBits[xxx] {
    override def fromInt(x: Int): this.type = (new xxx).asInstanceOf[this.type]
  }*/
  val tttttt = DSPBool(true)
  debug(tttttt)
  val rrrrrr = DSPBool(false)
  debug(rrrrrr)
  val sd = rrrrrr ? tttttt
  debug(sd)
  
  val r = DSPUInt(4) /| DSPUInt(5)
  debug(sd)
  
  val jj = double2T(-2.3); debug(jj)

  
  val y = new ComplexIO() 
  
  y.o := MyPipe(y.i,5)
  
  x.c := x.a + x.b
  
  /** Easily debug signals that aren't connected to output ports */
  val inner = x.a * x.b
  debug(inner)
  
  y.o_imag := double2T(2.3) //MyMux(y.i_real,y.i_imag,y.ctrl) * double2T(0.5) + double2T(-2.3)
  y.o_real := double2T(-2.3) //(y.i_imag >> 3) * double2T(-1) + double2T(3.3)
  
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
	
	val dblV2 = MyDbl(3.33) + y.myDblTest; debug(dblV2)
	val dblV1 = Dbl(3.44); debug(dblV1)
	
	y.ctrlO := MyBool(true)
  
  y.m := y.n
  
}

object Demo {
  def main(args: Array[String]): Unit = {
  
    implicit val formats = DefaultFormats 
  

    
    val p = """-params_(.*)""".r.findFirstMatchIn(args(0))
    println(p.get.group(1).toBoolean)
    
    val json = parse(scala.io.Source.fromFile("src/main/scala/Demo.json").getLines.mkString)
    val ttt = json.extract[JSONParams]
    println(ttt)
  
  
    val demoArgs = args.slice(1, args.length)
    val myParams = DemoParams(iMax = 20) 
    val demodParams = DemodParams()   
    chiselMainTest(demoArgs, () => MyModule( new Demo({MyFixed(INPUT,(5,4))}, myParams, demodParams) )) {
      c => new DemoTests(c) 
    }
  }
}

class DemoTests[T <: Demo[_ <: MyBits with MyNum[_]] ](c: T)  extends DSPTester(c) {
  //poke(c.y.t.complexIn.real,1.5)
  //peek(c.y.t.demodOut)
  poke(c.demodIO.complexIn.real,1.5)
  poke(c.demodIO.complexIn.imag,-1)
  poke(c.y.myDblTest,0.5)
  poke(c.y.i_real,0.522232323)
  poke(c.y.i_imag,-2.3)
  poke(c.x.a,1)
  poke(c.x.b,4)
  step(1)
  peek(c.inner)
  peek(c.x.c)
  //peek(c.x.d)
  peek(c.y.o_real)
  peek(c.y.o_imag)
  poke(c.y.x,0.5)
  peek(c.dblV1)
  peek(c.dblV2)
  peek(c.y.ctrlO)
  peek(c.demodIOo.complexIn.real)
  poke(c.y.n,-1)
  peek(c.y.m)
  peek(c.jj)
  poke(c.decoupledI.ready,false)


  //peek(c.y.i)
  //peek(c.y.o)
  
}


