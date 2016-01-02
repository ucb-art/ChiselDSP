# Run 'make setup' first!

setup:
	cd ChiselProject/sbt/ChiselCompatibility/src/main; rm -f scala; ln -s ../../../../../ChiselCompatibility scala
	cd ChiselProject/sbt/ChiselDSP_Modules/src/main; rm -f scala; ln -s ../../../../../ChiselDSP/Modules scala
	cd ChiselProject/sbt/ChiselDSP_Overlay/src/main; rm -f scala; ln -s ../../../../../ChiselDSP/Overlay scala
	
wash:
	find . -name "*~" -type f -delete
	cd ChiselProject/sbt/ChiselCompatibility/src/main; rm -f scala
	cd ChiselProject/sbt/ChiselDSP_Modules/src/main; rm -f scala
	cd ChiselProject/sbt/ChiselDSP_Overlay/src/main; rm -f scala
	cd ChiselProject/sbt/src/main; rm -f scala; rm -f resources
	
# Chisel source folders should be named ChiselPrjXXX where you should set PRJ = "XXX"
# FIXED = "true" implies running tests, generating Verilog for fixed point operations. Otherwise,
# floating point simulation is performed. 
PRJ = Demo
FIXED = false

# Syntax example: make vlsi PRJ=XXX

vlsi:
	mkdir Verilog${PRJ}; cd ChiselProject/sbt/src/main; rm -f scala; ln -s ../../../../ChiselPrj${PRJ}/scala/ scala; \
	rm -f resources; ln -s ../../../../ChiselPrj${PRJ}/resources/ resources; \
	cd ../..; sed -i -e 's/2.2.27/latest.release/g' build.sbt; \
	cd ..; rm vlsi/generated-src/*.v; make vlsi PRJ=${PRJ}
	
debug:
	cd ChiselProject/sbt/src/main; rm -f scala; ln -s ../../../../ChiselPrj${PRJ}/scala/ scala; \
    rm -f resources; ln -s ../../../../ChiselPrj${PRJ}/resources/ resources; \
	cd ../..; sed -i -e 's/2.2.27/latest.release/g' build.sbt; \
	cd ..; make debug PRJ=${PRJ} FIXED=${FIXED}
	
clean:
	find . -name "*.phd" -type f -delete
	find . -name "javacore*" -type f -delete
	find . -name "*.trc" -type f -delete
	find . -name "*.dmp" -type f -delete
	cd ChiselProject; make clean
	rm -rf target

reset:
	make clean; make wash; make setup

.PHONY: vlsi debug clean 




