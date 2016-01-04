package T____
import ChiselDSP._

/** Composition of generator input parameters */
case class GeneratorParams(complex: ComplexParams = ComplexParams()
                          ) extends JSONParams(complex)

object T____ {

  def main(args: Array[String]): Unit = {

    // Generator parameters + fixed/double mode setup
    val (isFixed,p) = Init({GeneratorParams()}, jsonName = "T____", args = args)

    // Setup module + tester
    val demoArgs = args.slice(1, args.length)
    if (isFixed)
      chiselMainTest(demoArgs, () => DSPModule(new T____({DSPFixed()}))) { c => new DemoXXXTests(c) }
    else
      chiselMainTest(demoArgs, () => DSPModule(new T____({DSPDbl()}))) { c => new DemoXXXTests(c) }

  }

}