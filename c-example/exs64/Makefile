# Makefile of test sequence generator for xorshift64star.c
# Written by Kenji Rikitake
# License: CC0 / public domain

#CC = clang
#CFLAGS = -O0 -g -Wall
CFLAGS = -O2

SOURCES = xorshift64star.c test.c
OBJECTS = $(SOURCES:.c=.o)
EXEC = test

all: $(OBJECTS) $(EXEC)

$(EXEC): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *~
