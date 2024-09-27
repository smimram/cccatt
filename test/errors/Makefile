CCCATT = ../../cccatt

all: test

test:
	@for i in $(wildcard *.cccatt); do \
	  if $(CCCATT) $$i ; then \
	    echo "- passed: $$i"; \
	    exit 1; \
          else \
            echo "- failed as expected: $$i"; \
	  fi \
	done
