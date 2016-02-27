ChiselDSP Development Environment
===================

This repository hopefully serves as a good starting point for making and easily testing your various ChiselDSP generators *(1 generator at a time)*. See [UC Berkeley Chisel](https://chisel.eecs.berkeley.edu) homepage for more information about Chisel.

----------

Key Enhancements
===============

Key ChiselDSP library enhancements over base Chisel (albeit at the expense of coding style restrictions & verbosity--enforces *good practice*!):

 1. Better range inference (DSP is more easily designed with numbers that have ranges, not bitwidths!)
 > You declare DSP☆ types with `range=(min,max)` instead of `width=numBits`. Operations (+, -, ×, other newer operators--see respective [**DSP☆.scala** type files ](ChiselDSP/Overlay/DSPTypes)) will automatically match widths (and decimal points) and determine required output bitwidths.

 2. Optionable IO (Generator parameters determine if certain input/output ports are actually used in Verilog).
 > In IO bundles, you can include an optional port (created when some condition is met) via `val yourIOPort = if (cond) Some(DSP☆(IODirection,range)) else None`. To use *yourIOPort* in the body of your code, you can write `io.yourIOPort.getOrElse(val_if_no_Port)`. Additionally, your IO bundle does not have to be `io`. You can have multiple unnested IO bundles by extending class **IOBundle** instead of Bundle. Note that **DSPFixed** is declared as `DSPFixed(IODirection,(integerWidth,fractionalWidth),range)`.

 3. Pipeline delay checking (Isn't it annoying when the delays of two signals into an operation don't line up because you forgot to delay a corresponding signal in your haste to close timing?)

 4. Enhanced support for designing and testing DSP with generic types (i.e. switching between **DSPDbl** for verifying functional correctness with double-precision floating point and **DSPFixed** for evaluating fixed-point design metrics by changing a single **sbt run** parameter).
> Inside any class that extends **GenDSPModule**, any `gen` will conform to the `Fixed=true/false` option used when running `make`. To create a new IO or internal node of type **gen** with `fixedParams=(integerWidth,fractionalWidth)`, use `gen.cloneType(fixedParams)` where the arguments are optional (defaults to integer and fractional widths indicated in the JSON file). If you want to specify a literal (or constant) of type **gen** within your module, use `double2T(yourConstant,fixedParams)`. Likewise, you can leave out *fixedParams* if you want to use defaults.

 5. Supports parameterization from external sources via JSON (i.e. in theory, configuration options for your generator can be passed in from a web interface, like [Spiral](http://www.spiral.net/hardware/dftgen.html)). This is achieved with the help of [Json4s](http://json4s.org).

 6. More useful and universal testing platform for numeric types!
 > Numbers are displayed in their correct formats instead of hex for peek, poke, and expect operations. Additionally, if your tester extends **DSPTester**, you can optionally dump your test sequence to a **Verilog testbench** file for functional verification on all simulation platforms (i.e. Xilinx, Altera, etc. instead of only VCS). The tolerance of comparisons with expected values can also be changed via `DSPTester.setTol(floTol = decimal_tolerance,
                     fixedTol = number_of_bits)`.

 7. **Miscellaneous additional features**
	 - Wide range of LUT modules for ease of generating lookup tables from pre-calculated constants (no intermediate representation)
	 - Memory modules that abstract out confusion associated with Chisel Mem
	 - Generates useful helper files with each Verilog output (constraints, generator parameters used, etc.).
	 - Easier to rename modules & signals and have renaming actually succeed.
	 - Expanding Support for non-base-2 math.
	 - Adds support for numerical processing in the Chisel Environment via [Breeze](https://github.com/scalanlp/breeze).

----------

Getting Started
===============

1. Setup **sbt** (see ["Installing SBT"](http://www.scala-sbt.org/0.13/docs/Setup.html) for support).

2. Run `make reset` in the base **ChiselEnvironment/** directory.
> Among other things, this creates symbolic links to **ChiselEnvironment/ChiselDSP/** files in the **ChiselEnvironment/ChiselProject/sbt/** directory, so that your project can be compiled with ChiselDSP features. If you want to clean up your working directory at any given point, run `make reset` again. This will *not* delete any of the scala files you have written.

3. To get started on your project, in the base **ChiselEnvironment/** directory, type `make prj PRJ=☆` where **☆** is your desired project (and top module) name. *Caution: do not change the project folder name!*
>This will create a folder called **ChiselEnvironment/ChiselPrj☆** populated with template files to help you get started quickly. The two subdirectories within your project folder that you will be modifying are:
- **resources/** contains a JSON file called **☆.json**, which enables you to pass compile-time parameters to your Chisel-based DSP generator. Note that you can pass parameters to your generator via case class instances (of *GeneratorParams*) and bypass the JSON file if you so please. You *must* provide defaults for **complex** and **clock** parameters either via the JSON file or via an instance of *GeneratorParams*. Note that defaults can be overridden on a case-by-case basis.
     - **intBits** and **fracBits** specify the # of integer bits and # of fractional bits respectively used to represent a fixed-point number. This affects the accuracy of your circuit!
     - **mulPipe** and **addPipe** specify the # of pipeline registers that should be automatically added after a fixed-point multiply or add operation. The registers should be added for retiming to meet throughput requirements with a specific ASIC/FPGA platform. Note that the *addPipe* value does not need to be an integer. By default, ChiselDSP will take the *floor* of the *addPipe* value for pipelining.
     - **overflowType** lets you specify one of several overflow handling methods when ChiselDSP's range-checking determines your circuit might overflow: *Grow (right output guaranteed), Saturate (mux that saturates when overflow is detected), Wrap (output has the same bitwidth as the inputs, without guaranteeing correctness of result)*--to be expanded.
     - **trimType** lets you specify one of several rounding modes when your trim the LSBs of your fixed-point signal: *NoTrim (does not trim), Truncate (truncates to designated # of bits), Round (round half up to designated # of bits)*--to be expanded.
     - **mulFracGrowth**: Note that the product of two DSPFixed numbers with non-zero *fractionalWidth* will be trimmed according to **trimType** so that the product will have fractionalWidth = # of fractional bits of the inputs + *mulFracGrowth* additional fractional bits (when *trimType != NoTrim*).
     - **periodx100ps** lets you specify the worst case **clock** period the design needs to meet (for generating Verilog testbench and constraints files). Note that the clock period desired = periodx100ps × 100ps (i.e. 39 translates to 3.9ns).
- **scala/**
     - **☆Tester.scala** allows you to test your top module via `reset(number_of_cycles), step(number_of_cycles), poke(input,value), peek(output), expect(output,expected_value)` via the C++ backend.
     - **☆.scala** is a template to get you started on your top module. Add IO's with **IOBundle**. Notice that **gen** is passed into your module.
     - **Main.scala** contains the bare minimum require to compile your top module. A case class called **GeneratorParams** must be included, with at least the fields `complex: ComplexParams` and `clock: ClockParams` (and default values). Additional fields specific to your generator should also be added here for parameterization. The code `val (isFixed,p) = Init({GeneratorParams()}, jsonName = "☆", args = args)` is required to setup ChiselDSP to work with your generator and input parameters. Note that assigning an empty string to *jsonName* will cause the defaults of `GeneratorParams()` to be used. Finally, either a top module using fixed-point or using double-precision floating point will be compiled based off of the **isFixed** flag, which is set based off of the **sbt run** parameter used. In order for the top module to access (and therefore do anything with) your parameters, you should pass the parameter as an argument to it.

4. To easily add a new & empty **DSPModule** called ★ to your project, in **ChiselEnvironment/** use `make module PRJ=☆ MODNAME=★`. If you want the module to extend **GenDSPModule** instead (so that you can use **gen**), use `make genmodule PRJ=☆ MODNAME=★`.

> **Note #1:** The directory **ChiselEnvironment/ChiselPrj☆/** is untracked in the **ChiselEnvironment** repository. Please start a new Git repo within your project directory to track changes.
>
> **Note #2:** For examples of ChiselDSP-specific code, refer to the [Demo](ChiselPrjDemo/scala/Demo.scala).

----------

Testing & Verilog Generation
===============

- `make debug PRJ=☆ FIXED=true/false` runs your instance of **DSPTester** and displays results on the console. The version of the module tested either uses *DSPFixed* or *DSPDbl* for *gen* depending on *FIXED*.

- `make asic PRJ=☆` generates a version of the Verilog RTL that is best suited for ASICs. Essentially, memories are blackboxed and a script in **ChiselEnvironment/VerilogHelpers/vlsi_mem_gen** is used to replace the black boxes with **SRAM** from an appropriate technology. Note that the script is not publicly available as it is technology/vendor specific. The output is located in **ChiselEnvironment/ChiselPrj☆/asic/**. The files generated are:
     - **☆.conf** details SRAM configuration i.e. depth, width, & ports for all memories required (used by *vlsi_mem_gen*)
     - **☆.v** is your top module's Verilog RTL (with SRAMs swapped in)
     - **generator_out.json** saves the parameters used for this generated design
     - **Makefrag_prj** is a helper file that specifies the name of the top module and clock period in ns (which can be included in the *Makefile* of your *vlsi* setup)

- `make asic_tb PRJ=☆` does the same as `make asic`, but also runs `make debug` and generates an additional **tb.v**, which is your Chisel testbench translated into a Verilog testbench (with fixed-point IO).

- `make fpga PRJ=☆` generates a version of the Verilog RTL that is best suited for FPGAs. The memories are written as **registers**, and BRAM can be inferred. The output is located in **ChiselEnvironment/ChiselPrj☆/fpga/**. The files generated are:
     - **constraints.xdc** specifies the clock constraints used for synthesis & place/route
     - **☆.v** is your top module's Verilog RTL (where registers are used for memory)
     - **generator_out.json**: See above.

- `make fpga_tb PRJ=☆` does the same as `make fpga`, but also runs `make debug` and generates a **tb.v**.

> **Note #1:** You should never write your own code into **ChiselEnvironment/ChiselPrj☆/fpga/** or **ChiselEnvironment/ChiselPrj☆/asic/**. They will be overwritten.
>
> **Note #2:** To save you some typing, whenever you specify `PRJ=☆`, the **Makefile** will be updated. If you're working on the same project, you can omit `PRJ=☆` in your `make` command.

----------

Submit Pull Requests!
=====================

This is a (potentially buggy) work in progress. Please submit **pull requests** if you have a fix to my code,  a different/better way of doing something, or a new feature you want other people to be able to use.

If you notice something is broken but you don't have a fix, would like a new feature, or want to have a discussion about doing something a different way (different directory structure, composition vs. inheritance for Chisel types, etc.), please create a new **issue**.

We'd like to hear from *you*, the users, about how we can increase your coding productivity, so comment away! :)

>**Note**: This setup currently doesn't support multi-project aggregates (designs consisting of multiple sub-projects stitched together via a top-level design specified by `PRJ=☆` in your Makefile). You can get a multi-project aggregate up and running by modifying  **ChiselEnvironment/ChiselProject/sbt/build.sbt** and adding the sub-project scala files to **ChiselEnvironment/ChiselProject/sbt/♢/src/main/scala**. Alternatively, set **ChiselEnvironment/ChiselProject/sbt/♢/src/main/scala** as a symbolic link to your sub-project's **ChiselEnvironment/ChiselPrj♢** directory. To add a sub-project **♢** (depending on sub-project **♧**) to your build.sbt, use `lazy val ♢ = yourSBTProject.dependsOn(♧).aggregate(♧)`. See [sbt docs](http://www.scala-sbt.org/0.13/docs/Multi-Project.html) for more information. In **build.sbt**, you will also have to modify the dependencies of the **root** project to include the additional sub-projects. When you have multiple main classes, sbt will prompt you to choose one to run.

----------

Check Out ChiselDSP!
====================

Unfortunately, features I've built into ChiselDSP are not well documented outside of the code. **ChiselEnvironment/ChiselDSP** is broken up into 2 sub-directories.

 - **Overlay**: Custom Chisel types specific to ChiselDSP and utility functions. Most of the custom types are modeled after pre-existing Chisel types. Therefore, the constructs are similar and most of the operators are retained (but there are more options!).
 - **Modules**: Useful *generalized* modules for DSP hardware generation.

>**Note**: Files in **ChiselEnvironment/ChiselCompatibility/** are only meant to remedy any compatibility issues between base Chisel and ChiselDSP.

----------

ChiselDSP Types
====================

For more information, see [here](ChiselDSP/Overlay/DSPTypes)) . ChiselDSP types are backwards compatible with their respective Chisel types (via implicit conversion), but Chisel types must be cast to their equivalent ChiselDSP types (since Chisel types have no knowledge of range, etc.).

 - **DSPBool**
 - **DSPUInt**
 - **DSPFixed**
 - **DSPSInt** (subset of *DSPFixed* with *fracWidth=0*)
 - **DSPDbl** (shares the same operators as *DSPFixed* so the two can be interchanged with *gen*)
 - **DSPComplex** (created via `Complex(real,imag)` where real and imag must both either be *DSPFixed* or *DSPDbl*)

> **Note #1**: Pipelining and registering are built into the type so that pipeline delays (specifically for a node y `val y = x.pipe(number_of_cycles)`) are tracked. Normal signal registering is untracked, but you can use `val y = x.reg()` for such an operation.
>
> **Note #2**: Another useful (new) operator is `x ? cond` which will return *x* if *cond* is true else 0. This was used to build the ChiselDSP **Mux**.

----------

A Note on Memories...
====================

For FPGA's, registering only the read address or only the read data out should allow you to pass behavioral simulation **but** if you try to synthesize with your memories as BRAM, post-synthesis functional simulation most likely fail. It seems to pass if you synthesize your memories into distributed RAM by (in *Vivado*) going to **Synthesis Settings**, **More Options** and using the directive `-ram_style distributed`. Note that distributed RAM can behave like a register file, where read can be performed asynchronously.

In general, however, it's best (safest) to register both the read address and read data out. When you instantiate your ChiselDSP **Memory**, use `seqRead = true, outReg = true`. **seqRead** determines whether to register the read address; **outReg** determines whether to register the data out. That way, post-synthesis functional simulation will also pass with BRAM.

> **Note**: In general, to force the type of RAM Xilinx should infer, use `-ram_style ☆` where ☆ is **block**, **auto**, or **distributed**. Check out [Dillon Engineering](http://www.dilloneng.com/inferring-block-ram-vs-distributed-ram-in-xst-and-precision.html#/) for more information on RAM inference.

For ASIC, currently the Memory wrapper only supports dual-ported memories, and because most SRAMs need to be initialized, the Chisel **reset** is used for the SRAM reset signal!

----------

Functional Programming 101
====================

Check out [Twitter's Scala School](https://twitter.github.io/scala_school/)!

To be continued...