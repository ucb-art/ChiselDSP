# TODO: Swap out imports to latest every time 'make debug', 'make vlsi' are run; error out if import Chisel._ is used

# Project Name
PRJ = FFT
# True -> tests with fixed point, else tests with double precision floating point
FIXED = false

# Setup environment with 'make reset'
reset:
	make clean; make scrub; make setup

# Clean Chisel generated files
clean:
	find . -name "*.phd" -type f -delete ;\
	find . -name "javacore*" -type f -delete ;\
	find . -name "*.trc" -type f -delete ;\
	find . -name "*.dmp" -type f -delete ;\
	cd ChiselProject; make clean ;\
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
	cd ChiselProject/sbt/ChiselCompatibility/src/main; ln -s ../../../../../ChiselCompatibility scala
	cd ChiselProject/sbt/ChiselDSP_Modules/src/main; ln -s ../../../../../ChiselDSP/Modules scala
	cd ChiselProject/sbt/ChiselDSP_Overlay/src/main; ln -s ../../../../../ChiselDSP/Overlay scala
	
# Setup project templates
prj:
	make default; if [ -d "ChiselPrj${PRJ}" ]; then exit 1; fi ;\
	mkdir -p ChiselPrj${PRJ}; mkdir -p Verilog${PRJ} ; \
	cp -R ChiselDSP/Project/* ChiselPrj${PRJ}/. ; \
	grep -rl '**IMPORTS' ChiselPrj${PRJ}/ | \
	xargs sed -i '.old' -e '/**IMPORTS/ {' -e 'r ChiselPrj${PRJ}/imports.txt' -e 'd' -e '}' ;\
	grep -rl 'T____' ChiselPrj${PRJ}/ | xargs sed -i '.old' -e 's/T____/${PRJ}/g' ; \
	cd ChiselPrj${PRJ} ; mv resources/T____.json resources/${PRJ}.json ; mv scala/T____.scala scala/${PRJ}.scala ; \
	mv scala/T____Tester.scala scala/${PRJ}Tester.scala ; \
	cd ..; find . -name "*.old" -type f -delete

# Name of module to create
MODNAME = XXX

# Make creating new module files easier
module:
	make default; if [ -f "ChiselPrj${PRJ}/scala/${MODNAME}.scala" ]; then exit 1; fi ;\
	cd ChiselPrj${PRJ}; cp templates/S____.scala scala/${MODNAME}.scala ; \
	sed -i '.old' -e 's/S____/${MODNAME}/g' scala/${MODNAME}.scala ; \
	find . -name "*.old" -type f -delete

# Make creating new generic module files easier (to support float/fixed)
genmodule:
	make default; if [ -f "ChiselPrj${PRJ}/scala/${MODNAME}.scala" ]; then exit 1; fi ;\
	cd ChiselPrj${PRJ}; cp templates/C____.scala scala/${MODNAME}.scala ; \
	sed -i '.old' -e 's/C____/${MODNAME}/g' scala/${MODNAME}.scala ; \
	find . -name "*.old" -type f -delete

# Compile to Verilog
vlsi:
	make default; make link; \
	cd ChiselProject ; rm vlsi/generated-src/*.v; make vlsi PRJ=${PRJ}

# Run Chisel Debug (fixed or not should be specified)
debug:
	make default; make link; \
	cd ChiselProject ; make debug PRJ=${PRJ} FIXED=${FIXED}
	
default:
	sed -i '.old' -e 's/^FIXED = .*/FIXED = ${FIXED}/g' Makefile; \
	sed -i '.old' -e 's/^FIXED = .*/FIXED = ${FIXED}/g' ChiselProject/Makefile; \
	sed -i '.old' -e 's/^PRJ = .*/PRJ = ${PRJ}/g' Makefile; \
	sed -i '.old' -e 's/^PRJ = .*/PRJ = ${PRJ}/g' ChiselProject/Makefile; find . -name "*.old" -type f -delete

link:
	cd ChiselProject/sbt/src/main; rm -f scala; ln -s ../../../../ChiselPrj${PRJ}/scala/ scala; \
    rm -f resources; ln -s ../../../../ChiselPrj${PRJ}/resources/ resources

.PHONY: vlsi debug clean