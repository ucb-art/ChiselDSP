/** Demo using skeleton for QAM demod */

// ------- Imports START -- DO NOT MODIFY
import org.json4s._
import org.json4s.native.JsonMethods._
import Chisel._
import ChiselDSP._
// ------- Imports END -- OK TO MODIFY BELOW

package Demo___

/** Parameters externally passed via JSON file (but with defaults) */
case class JSONParams (
  QAMn: List[Int] = List(4,16,64),          // List of supported n-QAM i.e. 4-QAM (QPSK), 16-QAM, 64-QAM, etc.
  frameSizes: List[Int] = List(128,1024),   // Supported frame sizes (see FFT sizes needed) 
  softDemod: Boolean = false                // If true, should do LLR calc, otherwise hard demod
  iFixedIntBits: Int = 1                    // Fixed in Qn.m notation --> n (# of integer bits, not including sign)
  iFixedFracBits: Int = 14                  // Fixed in Qn.m notation --> m (# frational bits, determines LSB)
)

/** IO Bundle showing off data types and how to pass parameters (actual vals are meaningless) */
class DemoIO(jsonParams:JSONParams) extends IOBundle {
  // ChiselDSP types (preferred because they keep track of meta info + perform optimizations with them)
  val b2 = DSPBool(INPUT)
  val u2 = DSPUInt(INPUT,(3,20))                    // (min,max) range; also: DSPUInt(DIR,max) -> assumes min = 0
  val d2 = DSPDbl(INPUT)
  val f2 = DSPFixed(INPUT,(1,15))                   // (int,frac) widths
  // Normal Chisel types
  val b0 = Bits(INPUT,width=5)
  val b1 = Bool(INPUT)
  val d1 = UInt(INPUT,width=5)
  val s1 = SInt(INPUT,width=5)
  val f1 = Fixed(INPUT,width = 17, fracWidth = 15)  // width = int width + frac width + 1 (sign)
  val d1 = Dbl(INPUT)
  val f0 = Flo(INPUT)
  
  // Demonstrates customizable IO --> IO pin is not generated if a Literal/constant is assigned 
  val optionalIO = if (jsonParams.softDemod) DSPUInt(0) else DSPUInt(INPUT,(3,jsonParams.frameSizes.max))
  
  // Example of how to create Complex
  val complex0 = Complex(DSPDbl(INPUT),DSPDbl(INPUT))
}

/** Special way to create a module that uses a generic type to easily switch between Double/Fixed point testing.
  * The correct type 'gen' must be passed in main. The second argument (true) @ GenDSPModule(gen,true) indicates
  * DecoupledIO is desired.
  */
class Demo___ [T <: DSPQnm[_]](gen : => T, jsonParams: JSONParams) extends GenDSPModule (gen, true) {

  /** Inline IO Bundle allows module methods double2T (double --> literal)
    * and T (customized Fixed widths) to be directly used.
    * Note IO should be in IOBundle.
    */
  class Demo___IO extends IOBundle {
    // Input either signed DSPFixed or DSPDbl, as set by gen
    val symbolIn = Complex(gen.gen).asInput      
    // # of "hard" bits required is set by the maximum n-QAM supported 
    // (toBitWidth converts from an integer to # of bits required to represent it)
    // Note for 4-QAM, the UInt range is [0,3]
    // For Fixed, output notation is Qn.m (width = n + m + 1 for sign bit)
    // When performing hard decoding, n,m = 0, so each element of the vec should
    // only be 1 bit wide (using the sign bit)
    // When performing hard decoding, n = 0, m = ??? -- you should determine what ??? is (15 is a placeholder)
    val m = if (jsonParams.soft) 15 else 0
    val demodOut = Vec(DSPUInt.toBitWidth(jsonParams.QAMn.max-1), T(OUTPUT,(0,m)))
    // If the bits of demodOut are interpreted as signed BigInt rather than fixed (i.e. renormalize wrt LSB), 
    // a large positive number is a very confident 0, and a small positive number is a less confident 0.
    // Negative #'s are associated with confidence for being a 1. We chose the sign of the LLR as positive <-> 0
    // bit because then the 2's complement sign bit of the LLR is the same as the hard decoder decision.
    // People aren't generally consistent about choosing positive LLRs to correspond to 0 or 1, so we choose
    // one with a conventient interpretation in this context.
    // Offset of the input sample relative to frame size (needs to support up to max frame size)
    val offsetIn = DSPUInt(INPUT,jsonParams.frameSizes.max-1)
    // If symbolIn --> corresponding demodOut takes n cycles, offsetOut should be offsetIn delayed n clocks
    val offsetOut = DSPUInt(OUTPUT,jsonParams.frameSizes.max-1)
  }
  
  // Instantiate IO objects
  val demoIO = new Demo___IO
  val i = new DemoIO
  // Creates a new instante of DemoIO with port directions flipped (i.e. to output)
  val o = new DemoIO.flip
  
  // Delay offset by how long it takes to finish computation (n)
  // Note: Instead of doing Reg(x) or Pipe(x,n) do x.reg() or x.pipe(n) to keep meta info
  val demoIO.offsetOut := demoIO.offsetIn.pipe(5)
  
  class LitBundle extends Bundle {
    // Create literals (that can be peeked)
    val b2 = DSPBool(true)
    val u2 = DSPUInt(17)                    
    val d2a = DSPDbl(-3.333)
    val d2b = DSPDbl(3.333)
    val f2a = DSPFixed(-0.78,(1,15))    
    val f2b = DSPFixed(0.78,(1,15))                 
    val b1 = Bool(true)
    val d1 = UInt(17,width=5)
    val s1a = SInt(-10,width=5)
    val s1b = SInt(10,width=5)
    val f1a = Fixed(-1.222,width = 17, fracWidth = 15)  
    val f1b = Fixed(1.222,width = 17, fracWidth = 15)  
    val d1a = Dbl(-3.33)
    val d1b = Dbl(3.33)
    val f0a = Flo(-3.33)
    val f0b = Flo(3.33)
    
    // Create literal from gen type {DSPFixed or DSPDouble} -- can override
    // default fixed intWidth, fracWidth by doing double2T(#,(intWidth,fracWidth))
    val gen0 = double2T(-1.3)
    val genb = double2T(1.3)
  }
  val lits = new LitBundle
  // Easily debug internal signals that aren't connected to output ports 
  // Can wrap signals in an aggregate (Vec, Bundle) to just do debug(aggregateName) OR
  // can just debug(signalName) i.e. debug(b2) if it wasn't in LitBundle
  debug(lits)
  
  // Shorthand to connect all DemoIO inputs to outputs
  i <> o
 
  // You can reassign to nodes; last assignment takes precedence
  // This is how you access individual [real, imag] components of complex
  // Operator types should match (gen should match gen, DSPDbl should match DSPDbl)
  // Arithmetic shift oeprations, normal +,-,* (no divide), Mux c ? tc : fc
  o.complex0.imag := (i.complex0.real >> 3) + DSPDbl(3)
  o.complex0.real := i.complex0.imag * DSPDbl(3) + Mux(i.b2,i.complex0.imag,i.complex0.real)

  // You can make use of DecoupledIO (ready,valid), which you enabled in the Module creation
  decoupledO.ready := decoupledI.ready
  decoupledO.valid := decoupledI.valid










 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 // Vec/complex [ either way]: reg, sla
 //mem,lut
  // when

// note modcounter is DSPModule, not GenDSPModule (no fixed type)
  
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

//declaration

class DemoTests[T <: Demo[_ <: MyBits with MyNum[_]] ](c: T)  extends DSPTester(c) {
// peek bundle
//peek bits, complex, vec
//setp

//reset
//poke(complex, bits)
  
}


