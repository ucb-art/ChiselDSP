PRJ = FFT
FIXED = false

vlsi:
	mkdir -p vlsi/generated-src
	cd sbt; rm generator_out.json ;\
	sbt "run -params_true --backend v --targetDir ../vlsi/generated-src"; mv ../vlsi/generated-src/*.v ../../Verilog${PRJ}/. ;\
	mv generator_out.json ../../Verilog${PRJ}/.

test:
	mkdir -p test/generated-src
	cd sbt; sbt "run -params_${FIXED} --test --genHarness --compile --targetDir ../test/generated-src"

debug:
	mkdir -p test/generated-src
	cd sbt; sbt "run -params_${FIXED} --test --genHarness --compile --debug --vcd --targetDir ../test/generated-src" | tee console_out

clean:
	rm -rf sbt/target sbt/project vlsi/generated-src/* test/generated-src/* sbt/console_out sbt/*.h sbt/*.cpp 
	rm -rf target sbt/ChiselCompatibility/target sbt/ChiselDSP_Modules/target sbt/ChiselDSP_Overlay/target
	rm -rf sbt/sim.start

.PHONY: vlsi test

