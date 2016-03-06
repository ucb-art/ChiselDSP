package ChiselDSP
import Chisel._

/** An object to create conditional logic (See Chisel when)
  * LIMITATION: Cannot track delay matching between block elements and
  * when conditions.
  */
object when {
  /** @param cond condition to execute upon
    * @param block a section of logic to enable if cond is true */
  def apply(cond: Bool)(block: => Unit): when = Chisel.when(cond){ block }
  def apply(cond: DSPBool)(block: => Unit): whenDSP = {
    cond.doNothing()
    Chisel.when.execWhen(cond.toBool){ block }
    new whenDSP(cond)
  }
}

/** A class representing the when block (supports ChiselDSP) */
class whenDSP (prevCond: DSPBool) {
  /** execute block when alternative cond is true */
  def elsewhen (cond: DSPBool)(block: => Unit): whenDSP = {
    Chisel.when.execWhen((!prevCond & cond).toBool){ block }
    new whenDSP(prevCond | cond)
  }

  /** Invalid: elsewhen condition should be DSPBool */
  def elsewhen (cond: Bool)(block: => Unit): whenDSP = {
    Error ("Condition in elsewhen should be of type DSPBool to match when condition type")
    this.asInstanceOf[whenDSP]
  }

  /** execute block by default */
  def otherwise (block: => Unit) {
    val chiselWhen = new Chisel.when(prevCond.toBool)
    chiselWhen.otherwise(block)
  }
}

/** This is identical to when with the condition inverted */
object unless {
  def apply(c: Bool)(block: => Unit) = Chisel.unless(c){block}
  def apply(c: DSPBool)(block: => Unit) {
    when (!c) { block }
  }
}