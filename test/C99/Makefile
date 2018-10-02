PREPROCESSOR=$(CC) -E
C_FRONT = ../../C-FrontEnd/src/C_Front
OMNI_JAR1=../../C-BackEnd/build/om-c-back.jar
OMNI_JAR2=../../XcodeML-Common/build/om-common.jar
C_BACK=java -Xmx200m -Xms200m -cp ${OMNI_JAR2}:${OMNI_JAR1} xcodeml.c.util.omx2c
TESTS     = $(wildcard *.c)
OUTPUT    = $(TESTS:.c=.x)
INTERFILE = $(TESTS:.c=.i)

.PHONY: clean all default
all default: $(OUTPUT)
.SUFFIXES: .i .c .x

.c.i:
	$(PREPROCESSOR) $< | $(C_FRONT) | $(C_BACK) > $@

.i.x: $(INTERFILE)
	$(CC) -fopenmp $< -o $@
	./$@

clean:
	rm -f *.i *.o *.x
