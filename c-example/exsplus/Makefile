# Makefile of test sequence generator for xorshiftplus128.c
# Written by Kenji Rikitake
# License: CC0 / public domain

#CC = clang
#CFLAGS = -O0 -g -Wall
CFLAGS = -O2

OBJECTS = $(SOURCES:.c=.o)
SOURCES = xorshift116plus.c test.c test-jump.c test-jump-gen.c
TESTOBJECTS = xorshift116plus.o test.o
TESTEXEC = test
TESTJUMPOBJECTS = xorshift116plus.o test-jump.o
TESTJUMPEXEC = test-jump
TESTJUMPGENOBJECTS = xorshift116plus.o test-jump-gen.o
TESTJUMPGENEXEC = test-jump-gen

all: $(OBJECTS) $(TESTEXEC) $(TESTJUMPEXEC) $(TESTJUMPGENEXEC)

$(TESTEXEC): $(TESTOBJECTS)
	$(CC) $(LDFLAGS) $(TESTOBJECTS) -o $@

$(TESTJUMPEXEC): $(TESTJUMPOBJECTS)
	$(CC) $(LDFLAGS) $(TESTJUMPOBJECTS) -o $@

$(TESTJUMPGENEXEC): $(TESTJUMPGENOBJECTS)
	$(CC) $(LDFLAGS) $(TESTJUMPGENOBJECTS) -o $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *~
