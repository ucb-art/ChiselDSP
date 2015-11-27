/** Utility functions */

package ChiselDSP
import Chisel._

/** Short [MyUInt] MyMod (x,n,[optional]dly)
  * Returns (x % n) for x <= 2n-1 
  * & overflow flag set when (x > n)
  * dly: output delay
  */
object MyMod {
  def apply(x:MyUInt, n:MyUInt) : Tuple2[MyUInt,MyBool] = apply(x,n,0)
  def apply(x:MyUInt, n:MyUInt, dly:Int) : Tuple2[MyUInt,MyBool] = {
    val xValidMax = 2*n.getMax-1
    if (x.getMax > xValidMax) 
      throwException("Cannot use short mod x%n for given range of inputs. x <= 2n-1 required."
      + " (x,n) max --> (" + x.getMax + "," + n.getMax + ")")
    val (res,overflow) = {
      if (x.getMax < n.getMax) {
        n.doNothing()
        (x.pipe(dly),MyBool(false))                     // No mod needed when x < n
      }    
      else {
        val newx = MyUInt(x,xValidMax)
        val diff = (newx - n).pipe(dly)               // No FPGA retiming issue @ carry chain output
        val sel = ~diff.extract(newx.getWidth-1)      // x >= n --> mod = x-n; otherwise mod = x
        val mod = MyMux(x.pipe(dly),diff,sel)
        (MyUInt(mod,n.getMax-1),sel)
      } 
    }
    (res,overflow)
  }
}

/** MyMux (a,b,s)
  * s = (false,true) --> (a,b)
  */
object MyMux {
  def apply [T <: MyBits with MyNum[T]] (a: T, b: T, sel: MyBool) : T = (a ? !sel) /| (b ? sel)
  def apply (a: MyBool, b: MyBool, sel: MyBool) : MyBool = (a ? !sel) /| (b ? sel)
}

/** MyPipe (x, n)
  * Delay all signals in x by n cycles
  */
object MyPipe {
  def apply [T <: MyBits with MyNum[T]] (x: Vec[_], n:Int) : Vec[_] = {
    Vec(x.map(
      _ match {
        case t : T => t.pipe(n) 
        case v : Vec[_] => apply(v,n)
      }
    ))
  }
}
