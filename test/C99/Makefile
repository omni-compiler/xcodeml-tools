PREPROCESSOR=gcc -E
C_FRONT = ../../C-FrontEnd/src/C_Front
C_Back  = ../../Driver/bin/C_Back
TESTS   = $(wildcard *.c)
OUTPUT  = $(TESTS:.c=.x)

.PHONY: clean all default
all default: $(OUTPUT)
.SUFFIXES: .x .c

.c.x:
	$(PREPROCESSOR) $< | $(C_FRONT) | sh $(C_Back) > $@

clean:
	rm -f $(OUTPUT)
