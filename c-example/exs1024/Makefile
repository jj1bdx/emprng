# Makefile of test sequence generator for xorshift64star.c
# Written by Kenji Rikitake
# License: CC0 / public domain

#CC = clang
#CFLAGS = -O0 -g -Wall
CFLAGS = -O2

OBJECTS = $(SOURCES:.c=.o)
SOURCES = xorshift1024star.c test.c test-jump.c test-jump-gen.c
TESTOBJECTS = xorshift1024star.o test.o
TESTEXEC = test
TESTJUMPOBJECTS = xorshift1024star.o test-jump.o
TESTJUMPEXEC = test-jump
TESTJUMPGENOBJECTS = xorshift1024star.o test-jump-gen.o
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
