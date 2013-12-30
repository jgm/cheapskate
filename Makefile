SHELL = /bin/bash
PROG ?= dist/build/cheapskate/cheapskate
BENCHPROGS ?= "pandoc -fmarkdown_strict+autolink_bare_uris+fenced_code_blocks+intraword_underscores"
SOURCES=bin/cheapskate.hs Text/Cheapskate.hs Text/Cheapskate/Parse.hs Text/Cheapskate/Types.hs Text/Cheapskate/Inlines.hs Text/Cheapskate/Util.hs Text/ParserCombinators.hs

.PHONY: prof test bench linecount clean fuzztest

$(PROG): $(SOURCES)
	cabal configure --user && cabal build

prof:
	cabal configure --enable-library-profiling --enable-executable-profiling --user && cabal build ; \
	  echo "To profile:  $(PROG) +RTS -pa -V0.0002 -RTS"

test:
	make -C tests --quiet clean all

fuzztest:
	cat /dev/random | head -c 100000 | iconv -f latin1 -t utf-8 | time $(PROG) >/dev/null ; \
	cat /dev/random | head -c 1000000 | iconv -f latin1 -t utf-8 | time $(PROG) >/dev/null ; \
	cat /dev/random | head -c 10000000 | iconv -f latin1 -t utf-8 | time $(PROG) >/dev/null

bench:
	for prog in $(PROG) $(BENCHPROGS); do \
	   echo; \
	   echo "Benchmarking $$prog"; \
	     time for i in tests/*/*.markdown; do \
	       cat $$i | $$prog >/dev/null ; \
	       done ; \
	done

linecount:
	@echo "Non-comment, non-blank lines:" ; \
	grep '^[^-]' Cheapskate.hs Cheapskate/*.hs | wc -l

clean:
	cabal clean && make -C tests clean
