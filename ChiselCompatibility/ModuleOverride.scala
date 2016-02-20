package Chisel
import ChiselDSP.Error

abstract class ModuleOverride(_clock: Option[Clock] = None, _reset: Option[Bool] = None)
                             extends Module(_clock,_reset) {

  /** Newer versions of Chisel have an addPin that clones the node.
    * We need one that doesn't for CreateIO to work (connect internal logic with new IO). 
    * Add a pin with a name to the module
    * @param pin the I/O to add
    * @param name A name for the pin
    */
  protected def addPinChiselDSP[T <: Data](pin: T, name: String = "") {
    io match {
      case b: Bundle => assignIO(pin,name,b)
      case _ => Error("IO should be contained in an IOBundle or Bundle called io.")
    }
  }

  // TODO: Move accessors to Chisel.Module?
  /** For ChiselDSP to get resets, clocks */
  def getResets = resets
  def getClocks = clocks

  private def assignIO[T <: Data](pin: T, name: String, b: Bundle): Unit = {
    for ((n, io) <- pin.flatten) {
      io.compOpt = Some(this)
      io.isIo = true
    }
    if (name != "") pin nameIt (name, true)
    b.elements += ((pin.name, pin))
  }

  // Fixed IO is blank -- use createIO with your custom bundles or override io
  val io = new Bundle

}
