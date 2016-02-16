package BaseNExample
import ChiselDSP._

/** Composition of generator input parameters */
case class GeneratorParams(complex: ComplexParams = ComplexParams(), clock: ClockParams = ClockParams()
                          ) extends JSONParams(complex, clock)

object Main {

  def main(args: Array[String]): Unit = {

    // Generator parameters + fixed/double mode setup
    val (isFixed,p) = Init({GeneratorParams()}, jsonName = "BaseNExample", args = args)

    // Setup module + tester
    val demoArgs = args.slice(1, args.length)
    if (isFixed)
      Chisel.chiselMainTest(demoArgs, () => DSPModule(new BaseNExample({DSPFixed()}))) { c => new BaseNExampleTests(c) }
    else
      Chisel.chiselMainTest(demoArgs, () => DSPModule(new BaseNExample({DSPDbl()}))) { c => new BaseNExampleTests(c) }

  }

}
