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
	cd ChiselProject/sbt/src/main; rm -f scala
	
# Chisel source folders should be named ChiselPrjXXX where you should set PRJ = "XXX"
PRJ = "Demo"   

# Syntax example: make vlsi PRJ="XXX"

vlsi:
	mkdir Verilog$(PRJ); cd ChiselProject/sbt/src/main; rm -f scala; ln -s ../../../../ChiselPrj$(PRJ) scala; \
	cd ../..; sed -i -e 's/2.2.27/latest.release/g' build.sbt; \
	cd ..; rm vlsi/generated-src/*.v; make vlsi PRJ=$(PRJ)
	
debug:
	cd ChiselProject/sbt/src/main; rm -f scala; ln -s ../../../../ChiselPrj$(PRJ) scala; \
	cd ../..; sed -i -e 's/2.2.27/latest.release/g' build.sbt; \
	cd ..; make debug
	
clean:
	cd ChiselProject; make clean

.PHONY: vlsi debug clean 




