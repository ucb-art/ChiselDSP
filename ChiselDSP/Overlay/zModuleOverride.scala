/** Changes to Chisel Module */

package Chisel

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
      case b: Bundle => {
        for ((n, io) <- pin.flatten) {
          io.compOpt = Some(this)
          io.isIo = true
        }
        if (name != "") pin nameIt (name, true)
        b.elements += ((pin.name, pin))
      }
      case _ => 
    }
  }

}

