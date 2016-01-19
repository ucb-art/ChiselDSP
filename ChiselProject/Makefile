PRJ = FFT
FIXED = false
BACKEND = v

fpga:
	mkdir -p ../Verilog${PRJ}/fpga ; rm -f ../Verilog${PRJ}/fpga/* ;\
	make vlsi BACKEND=fpga ;\
	mv vlsi/generated-src/*.v ../Verilog${PRJ}/fpga/. ;\
	mv vlsi/generated-src/*.conf ../Verilog${PRJ}/fpga/. ;\
	mv sbt/generator_out.json ../Verilog${PRJ}/fpga/.

asic:
	mkdir -p ../Verilog${PRJ}/asic ; rm -f ../Verilog${PRJ}/asic/* ;\
	make vlsi BACKEND=v ;\
	mv vlsi/generated-src/*.v ../Verilog${PRJ}/asic/. ;\
	mv vlsi/generated-src/*.conf ../Verilog${PRJ}/asic/. ;\
	mv sbt/generator_out.json ../Verilog${PRJ}/asic/.

vlsi:
	mkdir -p vlsi/generated-src ; rm -f vlsi/generated-src/*.v ; rm -f vlsi/generated-src/*.conf ;\
    cd sbt; rm -f generator_out.json ;\
    sbt "run -params_true --debugMem --backend ${BACKEND} --targetDir ../vlsi/generated-src "

test:
	mkdir -p test/generated-src
	cd sbt; sbt "run -params_${FIXED} --test --debugMem --genHarness --compile --targetDir ../test/generated-src "

debug:
	mkdir -p test/generated-src
	cd sbt; sbt "run -params_${FIXED} --test --debugMem --genHarness --compile --debug --vcd --targetDir ../test/generated-src " | tee console_out

clean:
	rm -rf sbt/target sbt/project vlsi/generated-src/* test/generated-src/* sbt/console_out sbt/*.h sbt/*.cpp 
	rm -rf target sbt/ChiselCompatibility/target sbt/ChiselDSP_Modules/target sbt/ChiselDSP_Overlay/target
	rm -rf sbt/sim.start

.PHONY: vlsi test

