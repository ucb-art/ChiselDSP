# Setup environment with 'make reset'
reset:
	make clean; make scrub; make setup

# Clean Chisel generated files
clean:
	find . -name "*.phd" -type f -delete
	find . -name "javacore*" -type f -delete
	find . -name "*.trc" -type f -delete
	find . -name "*.dmp" -type f -delete
	cd ChiselProject; make clean
	rm -rf target

# Remove all project environment files
scrub:
	find . -name "*~" -type f -delete
	cd ChiselProject/sbt/ChiselCompatibility/src/main; rm -f scala
	cd ChiselProject/sbt/ChiselDSP_Modules/src/main; rm -f scala
	cd ChiselProject/sbt/ChiselDSP_Overlay/src/main; rm -f scala
	cd ChiselProject/sbt/src/main; rm -f scala; rm -f resources

# Setup ChiselDSP in project environment
setup:
	cd ChiselProject/sbt/ChiselCompatibility/src/main; rm -f scala; ln -s ../../../../../ChiselCompatibility scala
	cd ChiselProject/sbt/ChiselDSP_Modules/src/main; rm -f scala; ln -s ../../../../../ChiselDSP/Modules scala
	cd ChiselProject/sbt/ChiselDSP_Overlay/src/main; rm -f scala; ln -s ../../../../../ChiselDSP/Overlay scala
	
# Setup project templates
prj:
	mkdir -p ChiselPrj{$PRJ}; cp ChiselDSP/Project/* ChiselPrj{$PRJ}/. ; \
	grep -rl 'T____' ChiselPrj{$PRJ}/ | xargs sed -i 's/T____/{$PRJ}/g' ; \
	grep -rl '***IMPORTS***' ChiselPrj{$PRJ}/ | xargs sed -i 's/***IMPORTS***/${cat ChiselDSP/Project/imports.txt}/g' ;\
	cd ChiselPrj{$PRJ} ; mv resources/T____.json resources/{$PRJ}.json ; mv scala/T____.scala resources/{$PRJ}.scala ; \
	mv scala/T____Tester.scala scala/{$PRJ}Tester.scala ; \






	
# Chisel source folders should be named ChiselPrjXXX where you should set PRJ = "XXX"
# FIXED = "true" implies running tests, generating Verilog for fixed point operations. Otherwise,
# floating point simulation is performed. 
PRJ = FFT
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
	


.PHONY: vlsi debug clean 




