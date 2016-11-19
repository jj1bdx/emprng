# If stock `make` is GNU Make, use `make`; otherwise use `gmake`
GNUMAKE=@`sh -c 'if [ ! -z \`which gmake\` -a -x \`which gmake\` ]; then echo gmake; else echo make; fi'`

TARGETMAKEFILE=	./Makefile.emprng

all:
	$(GNUMAKE) -f $(TARGETMAKEFILE) $@

.DEFAULT:
	$(GNUMAKE) -f $(TARGETMAKEFILE) $@
