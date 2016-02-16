PRJ = TBEx
FIXED = true
MEM = --inlineMem

fpga:
	mkdir -p ../Verilog${PRJ}/fpga ; rm -f ../Verilog${PRJ}/fpga/* ;\
	make vlsi MEM=--inlineMem ;\
	mv vlsi/generated-src/*.v ../Verilog${PRJ}/fpga/. ;\
	mv sbt/generator_out.json ../Verilog${PRJ}/fpga/.

asic:
	mkdir -p ../Verilog${PRJ}/asic ; rm -f ../Verilog${PRJ}/asic/* ;\
	make vlsi MEM=--noInlineMem ;\
	mv vlsi/generated-src/*.v ../Verilog${PRJ}/asic/. ;\
	mv sbt/generator_out.json ../Verilog${PRJ}/asic/. ;\
	if [ -f vlsi/generated-src/${PRJ}.conf ]; then \
	    mv vlsi/generated-src/${PRJ}.conf ../Verilog${PRJ}/asic/. ;\
	fi

vlsi:
	mkdir -p vlsi/generated-src ; rm -f vlsi/generated-src/*.v ; rm -f vlsi/generated-src/*.conf ;\
    cd sbt; rm -f generator_out.json ;\
    sbt "run -params_true ${MEM} --genHarness --backend v --targetDir ../vlsi/generated-src "

test:
	mkdir -p ../Verilog${PRJ} ; rm -f ../Verilog${PRJ}/*.xdc ; rm -f ../Verilog${PRJ}/*.v ;\
	mkdir -p test/generated-src
	cd sbt; sbt "run -params_${FIXED} --test --debugMem --genHarness --compile --targetDir ../test/generated-src " ;\
	mv tb.v ../../Verilog${PRJ}/. ; mv constraints.xdc ../../Verilog${PRJ}/.

debug:
	mkdir -p ../Verilog${PRJ} ; rm -f ../Verilog${PRJ}/*.xdc ; rm -f ../Verilog${PRJ}/*.v ;\
	mkdir -p test/generated-src
	cd sbt; sbt "run -params_${FIXED} --test --debugMem --genHarness --compile --debug --vcd --targetDir ../test/generated-src " | tee console_out ;\
	mv tb.v ../../Verilog${PRJ}/. ; mv constraints.xdc ../../Verilog${PRJ}/.

clean:
	rm -rf sbt/target sbt/project vlsi/generated-src/* test/generated-src/* sbt/console_out sbt/*.h sbt/*.cpp 
	rm -rf target sbt/ChiselCompatibility/target sbt/ChiselDSP_Modules/target sbt/ChiselDSP_Overlay/target
	rm -rf sbt/sim.start

.PHONY: vlsi test

