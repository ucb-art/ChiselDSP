package ChiselDSP
import Chisel._

/** An object to create conditional logic (See Chisel when for example usage) */
object when {
  /** Execute a when block */
  private[ChiselDSP] def execWhen(cond: Bool)(block: => Unit) {
    Module.current.whenConds.push(Module.current.whenCond && cond)
    block
    Module.current.whenConds.pop()
  }
  private[ChiselDSP] def execWhen(cond: DSPBool)(block: => Unit) {
    Module.current.whenCondsDSP.push(Module.current.whenCondDSP && cond)
    block
    Module.current.whenCondsDSP.pop()
  }
  /** @param cond condition to execute upon
    * @param block a section of logic to enable if cond is true */
  def apply(cond: DSPBool)(block: => Unit): whenDSP = {
    cond.use()
    execWhen(cond){ block }
    new whenDSP(cond)
  }
  def apply(cond: Bool)(block: => Unit): when = {
    execWhen(cond){ block }
    new when(cond)
  }
}

/** A class representing the when block (supports ChiselDSP types) */
class whenDSP (prevCond: DSPBool) {
  /** execute block when alternative cond is true */
  def elsewhen (cond: DSPBool)(block: => Unit): whenDSP = {
    cond.use(); prevCond.use()
    when.execWhen(!prevCond & cond){ block }
    new when(prevCond || cond)
  }
  /** execute block by default */
  def otherwise (block: => Unit) {
    val cond = !prevCond
    cond.canBeUsedAsDefault = !Module.current.hasWhenCond
    when.execWhen(cond){ block }
  }
}

/** This is identical to [[Chisel.when when]] with the condition inverted */
object unless {
  def apply(c: Bool)(block: => Unit) {
    when (!c) { block }
  }
}

/** Conditional logic to form a switch block
  * @example
  * {{{ ... // default values here
  * switch ( myState ) {
  *   is( state1 ) {
  *     ... // some logic here
  *   }
  *   is( state2 ) {
  *     ... // some logic here
  *   }
  * } }}}*/
object switch {
  def apply(c: Bits)(block: => Unit) {
    Module.current.switchKeys.push(c)
    block
    Module.current.switchKeys.pop()
  }
}

/** An object for separate cases in [[Chisel.switch switch]]
  * It is equivalent to a [[Chisel.when$ when]] block comparing to the condition
  * Use outside of a switch statement is illegal */
object is {
  def apply(v: BitPat)(block: => Unit): Unit =
    when (v === switchCond) { block }
  def apply(v: Bits)(block: => Unit): Unit =
    apply(Seq(v))(block)
  def apply(v: Bits, vr: Bits*)(block: => Unit): Unit =
    apply(v :: vr.toList)(block)
  def apply(v: Iterable[Bits])(block: => Unit): Unit =
    when (v.map(_ === switchCond).fold(Bool(false))(_||_)) { block }

  private def switchCond = {
    if (Module.current.switchKeys.isEmpty) {
      ChiselError.error("The 'is' keyword may not be used outside of a switch.")
      Bits(0)
    } else Module.current.switchKeys.top
  }
}