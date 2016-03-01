/** Clock parameters */

package ChiselDSP

object Clock {
  private[ChiselDSP] var opts: ClockParams = ClockParams()
  def getPeriodx100ps(): Int = opts.periodx100ps
}

/** Default clock customizations */
case class ClockParams (
 periodx100ps: Int = 20
)