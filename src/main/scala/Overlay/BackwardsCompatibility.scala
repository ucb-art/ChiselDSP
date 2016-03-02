package ChiselDSP
import Chisel._

object BackwardsCompatibility {

  // Note that implicit conversion can only be done in one direction i.e. DSPX to X
  // (in the process, you lose metadata)

  /** DSPBool should be backwards compatible with Bool */
  implicit def dspBoolToBool(x: DSPBool): Bool = x.toBool
  /** DSPUInt should be backwards compatible with UInt */
  implicit def dspUIntToUInt(x: DSPUInt): UInt = x.toUInt
  /** DSPFixed is backwards compatible with SInt (reinterprets LSB as 1 and loses metadata) */
  implicit def dspFixedToSInt(x: DSPFixed): SInt = x.toSInt
  /** DSPFixed is backwards compatible with Fixed */
  implicit def dspFixedToFixed(x: DSPFixed): Fixed = {
    chiselCast(x) {Fixed(x.dir, x.getWidth, x.getFracWidth)}
  }
  /** DSPDbl is backwards compatible with Dbl */
  implicit def dspDblToDbl(x: DSPDbl): Dbl = {
    chiselCast(x){Dbl(x.dir)}
  }

}