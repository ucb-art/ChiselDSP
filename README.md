OUTDATED! :(

ChiselDSP Development Environment
===================

This repository hopefully serves as a good starting point for making and easily testing your various Chisel DSP generators *(1 generator at a time)*.

----------

Getting Started
===============

 1. Setup **sbt**
 2. Run `make setup` in the base **ChiselEnvironment/** directory. This creates symbolic links to **ChiselEnvironment/ChiselDSP/** files in the **ChiselEnvironment/ChiselProject/sbt/** directory, so that your project can be compiled using ChiselDSP!
 3. To get started on your project, in the base **ChiselEnvironment/** directory, type `mkdir ChiselPrjXXX` where XXX is your project name. All of your project **scala** files should be placed in **ChiselPrjXXX**.
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
	


