package ChiselDSP
import Chisel._

/** Special bundle for IO - should only be used with DSPModule and its child classes
  * Adds itself to the current DSPModule's list of IOs to setup.
  * If checkOutDlyMatch = true, all assigned outputs of the bundle should have the same delay
  * Note that all inputs should have the same delay.
  */
abstract class IOBundle(val outDlyMatch: Boolean = false) extends Bundle {

  Module.current match {
    case a: DSPModule => a.ios.push(this)
    case _ =>
  }

  /** Name IO bundle + elements. For a custom bundle name,
    * setName should be called within the module.
    * Otherwise, the name is the Bundle's class name.
    * Dir indicates whether to include port direction in the name.
    * Name of IO pin = IOBundleName _ direction _ signalName
    */
  override def setName (name:String) = setName(name, dir = true)
  def setName(name:String, dir: Boolean) = label(name, isNamingIo = true, dir, custom = true)

  /** Set name used internally -- not user specified */
  private[ChiselDSP] def setNameIO(name:String) = label(name, isNamingIo = true, dir = true, custom = false)
  private def label (path: String, isNamingIo: Boolean, dir:Boolean, custom: Boolean) {
    val oldName = name
    if( !named && (name.isEmpty || (!path.isEmpty && name != path)) ) {
      name = path
      val prefix = if (name.length > 0) name + "_" else ""
      flatten.map( x =>
      {
        val dirStr = if (isNamingIo && dir) (if (x._2.dir == INPUT) "in" else "out") + "_" else ""
        val newName = prefix + dirStr + x._1
        val newName2 = prefix + dirStr + x._2.name.replace("_in","").replace("_out","")
        if (x._2.name.length == 0) x._2.nameIt(newName, isNamingIo)
        else if (x._1.contains(x._2.name) && !custom){
          x._2.named = false
          x._2.nameIt(newName2, isNamingIo)
        }
      }
      )
    }
    else if (custom) Warn("IO Bundle already named " + oldName + ". Cannot rename to " + path + ".")
    named = true
  }

  /** Checks to see that assigned outputs of the IO Bundle have the same delay; otherwise errors out */
  private[ChiselDSP] def checkOutDly(): Int ={
    if (outDlyMatch){
      val temp = flatten.map (x => x._2 match{
        case d : DSPBits[_] => if(d.dir == OUTPUT && d.isAssigned) d.getDelay() else -1
        case _ => -1
      })
      val dly = temp.distinct.filter(_ != -1)
      val numDistinct = dly.length
      if (numDistinct > 1) Error("Assigned IO Bundle outputs don't have the same delay")
      if (numDistinct == 1) dly.head else 0
    }
    else 0
  }

  // TODO: Get delay of elements of a Vec like you do for a bundle

  /** Get output delay of bundle elements if tracked elements should all have the same delay. Note that the user
    * can call the function before all of the outputs have been assigned, at which point it will only return
    * the delay of assigned outputs! Caution must be used!
    */
  def getOutDelay(): Int = {
    if (!outDlyMatch) Error("Cannot get IO Bundle output delay when not explicitly checked")
    checkOutDly()
  }

  // TODO: Merge?
  /** Get max of output delays regardless of match */
  def getMaxOutDelay(): Int = {
    val temp = flatten.map (x => x._2 match{
        case d : DSPBits[_] => if(d.dir == OUTPUT && d.isAssigned) d.getDelay() else -1
        case _ => -1
      })
    temp.distinct.filter(_ != -1).max
  }

}