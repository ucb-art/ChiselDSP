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

 4. Enhanced support for designing and testing DSP with generic types (i.e. switching between **DSPDbl** for verifying functional correctness and and **DSPFixed** for evaluating fixed-point design metrics by changing a single **sbt run** parameter).
> Inside any class that extends **GenDSPModule**, any `gen` will conform to the `Fixed=true/false` option used when running `make`. To create a new IO or internal node of type **gen** with `fixedParams=(integerWidth,fractionalWidth)`, use `gen.cloneType(fixedParams)` where the arguments are optional (defaults to integer and fractional widths indicated in the JSON file). If you want to specify a literal (or constant) of type **gen** within your module, use `double2T(yourConstant,fixedParams)`. Likewise, you can leave out *fixedParams* if you want to use defaults.

 5. Supports parameterization from external sources via JSON (i.e. in theory, configuration options for your generator can be passed in from a web interface, like [Spiral](http://www.spiral.net/hardware/dftgen.html)). This is achieved with the help of [Json4s](http://json4s.org).

 6. More useful and universal testing platform for numeric types!
 > Numbers are displayed in their correct formats instead of hex for peek, poke, and expect operations. Additionally, if your tester extends **DSPTester**, you can optionally dump your test sequence to a **Verilog testbench** file for functional verification on all simulation platforms (i.e. Xilinx, Altera, etc. instead of only VCS). The tolerance of comparisons with expected values can also be changed via `DSPTester.setTol(floTol = decimal_tolerance,
                     fixedTol = number_of_bits)`.




 5. Wide range of LUT modules for ease of generating lookup tables from pre-calculated constants (no intermediate representation)

clearer memory. baseN. generates more compete helper files (constraints, etc.)  easier to rename modules, signals, more  breeze
move types appropriately


----------

Getting Started
===============

 - Setup **sbt** (see ["Installing SBT"](http://www.scala-sbt.org/0.13/docs/Setup.html) for support)
 - Run `make reset` in the base **ChiselEnvironment/** directory.

> Among other things, this creates symbolic links to **ChiselEnvironment/ChiselDSP/** files in the **ChiselEnvironment/ChiselProject/sbt/** directory, so that your project can be compiled with ChiselDSP features. If you want to clean up your working directory at any given point, run `make reset` again. This will *not* delete any of the scala files you have written.

- To get started on your project, in the base **ChiselEnvironment/** directory, type `make prj PRJ=☆` where **☆** is your desired project (and top module) name.

>This will create a folder called **ChiselEnvironment/ChiselPrj☆** populated with template files to help you get started quickly. The two subdirectories within your project folder that you will be modifying are:
>**resources/** contains a JSON file called **☆.json**, which enables you to pass compile-time parameters to your Chisel-based DSP generator. Note that you can pass parameters to your generator via case class instances (of *GeneratorParams*) and bypass the JSON file if you so please. You *must* provide defaults for **complex** and **clock** parameters either via the JSON file or via an instance of *GeneratorParams*. Note that ChiselDSP allows you to test your DSP Generators either in *fixed-point* or *double-precision floating point*. *intBits* and *fracBits* specify the # of integer bits and # of fractional bits respectively used to represent a fixed-point number. This affects the accuracy of your circuit! *mulPipe* and *addPipe* specify the # of pipeline registers that should be automatically added after a fixed-point multiply or add operation. The registers are added for retiming to meet throughput requirements. Note that the *addPipe* value does not need to be an integer. By default, ChiselDSP will take the *floor* of the *addPipe* value for pipelining. *overflowType* lets you specify one of several overflow handling methods when ChiselDSP's range-checking determines if your circuit will overflow: *Grow, Saturate, Wrap* (to be expanded). *trimType* lets you specify one of several rounding modes when your trim the LSBs of your fixed-point signal: *NoTrim, Truncate, Round* (to be expanded).

defaults can be all overwritten. mulFracGrowth. periodx100ps.
 4. To compile your Chisel project into Verilog, run `make vlsi PRJ="XXX"` in the base **ChiselEnvironment/** directory. For convenience, you can setup **XXX** to be your default project by editing the line `PRJ="XXX"` in **ChiselEnvironment/Makefile**. Then you can simply run `make vlsi`. You can run Chisel in debug mode with `make debug PRJ="XXX"` or `make debug` on the default project. 
 5. Generated verilog will be located in **ChiselEnvironment/VerilogXXX/**
 
 >**Note #1**: Source files in **ChiselEnvironment/ChiselPrjXXX/** are untracked in the *Chisel Environment* repository. Please start a new Git repo within your project source directory to track your project changes. 
 
----------

Submit Pull Requests!
=====================

This is a (potentially buggy) work in progress. Please submit **pull requests** if you have a fix to my broken code,  a different/better way of doing something, or a new feature you want other people to be able to use. 

If you notice something is broken but you don't have a fix, would like a new feature, or want to have a discussion about doing something a different way (different directory structure, composition vs. inheritance for Chisel types), please create a new **issue**.

We'd like to hear from *you*, the users, about how we can increase your coding productivity, so comment away! :)

>**Note #2**: This setup currently doesn't support multi-project aggregates (designs consisting of multiple sub-projects stitched together via a top-level design specified by `PRJ="XXX"` in your Makefile). You can get a multi-project aggregate up and running by modifying  **ChiselEnvironment/ChiselProject/sbt/build.sbt** and adding the sub-project scala files to **ChiselEnvironment/ChiselProject/sbt/YYY/src/main/scala**. Alternatively, set .../scala as a symbolic link to your sub-project's **ChiselEnvironment/ChiselPrjYYY** directory. To add a sub-project **YYY** (depending on sub-project **ZZZ**) to your build.sbt, use `lazy val YYY = project.dependsOn(ZZZ)`. See [sbt docs](http://www.scala-sbt.org/0.13/tutorial/Multi-Project.html "sbt docs") for more information. In build.sbt, you will also have to modify the dependencies of the **root** project to include the additional sub-projects. When you have multiple main classes, sbt will prompt you to choose one to run. 

----------

Check Out ChiselDSP!
====================

Unfortunately, features I've built into ChiselDSP are not well documented outside of the code. **ChiselEnvironment/ChiselDSP** is broken up into 2 sub-directories. 

 - **Overlay**: Custom Chisel types specific to ChiselDSP and utility functions. Most of the custom types are modeled after pre-existing Chisel types. Therefore, the constructs are similar and most of the operators are retained. 
 - **Modules**: Useful *generalized* modules for DSP hardware generation
 
>**Note #3**: Files in **ChiselEnvironment/ChiselCompatibility/** are only meant to remedy any compatibility issues between base Chisel and ChiselDSP.  

ChiselDSP Types
---------------

 - **MyBool**
	- `MyBool(IODirection)`, `MyBool(Boolean)`, `MyBool(Bits)` where the last construct is used for casting Chisel Bits to MyBool
	- `x.pipe(n)` where n is # of delays
	- `x.reg()` as an alternative to Reg(x)
	- `&`, `?` (alternative to `&`), `|`, `/|` (alternative to `|`), `^` (XOR), `!` (invert)
		 
		 
To be continued...
	


