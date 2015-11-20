PRJ="Demo"

vlsi:
	mkdir -p vlsi/generated-src
	cd sbt; sbt "run -params --backend v --targetDir ../vlsi/generated-src"; mv ../vlsi/generated-src/*.v ../../Verilog$(PRJ)/. 

test:
	mkdir -p test/generated-src
	cd sbt; sbt "run -params --test --genHarness --compile --targetDir ../test/generated-src"

debug:
	mkdir -p test/generated-src
	cd sbt; sbt "run -params --test --genHarness --compile --debug --vcd --targetDir ../test/generated-src" | tee console_out

clean:
	rm -rf sbt/target sbt/project vlsi/generated-src/* test/generated-src/* sbt/console_out sbt/*.h sbt/*.cpp 
	rm -rf target sbt/ChiselCompatibility/target sbt/ChiselDSP_Modules/target sbt/ChiselDSP_Overlay/target

.PHONY: vlsi test

