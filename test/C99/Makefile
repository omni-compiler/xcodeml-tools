PREPROCESSOR=$(CC) -E
C_FRONT = ../../C-FrontEnd/src/C_Front
TESTS   = $(wildcard *.c)
OUTPUT  = $(TESTS:.c=.i)

.PHONY: clean all default
all default: $(OUTPUT)
.SUFFIXES: .i .c

.c.i:
	$(PREPROCESSOR) $< | $(C_FRONT) > $@

clean:
	rm -f $(OUTPUT)
