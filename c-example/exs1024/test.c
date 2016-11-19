/*
 * test sequence generator for xorshift64star.c
 * Written by Kenji Rikitake
 * License: CC0 / public domain
 * NOTE: use C99 or later
 */

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

uint64_t next(void);

extern uint64_t s[16];
extern int p;

void print_s(void)
{
    printf("p = %d ", p);
	for(int j = 0; j < 16; j++) {
    	printf("s[%d] = %" PRIu64 " ", j, s[j]);
	}
	printf("\n");
}

int main(void)
{
    int i;

    p = 0;
    s[0] = 0x0123456789abcdefULL;
    s[1] = 0x123456789abcdef0ULL;
    s[2] = 0x23456789abcdef01ULL;
    s[3] = 0x3456789abcdef012ULL;
    s[4] = 0x456789abcdef0123ULL;
    s[5] = 0x56789abcdef01234ULL;
    s[6] = 0x6789abcdef012345ULL;
    s[7] = 0x789abcdef0123456ULL;
    s[8] = 0x89abcdef01234567ULL;
    s[9] = 0x9abcdef012345678ULL;
    s[10] = 0xabcdef0123456789ULL;
    s[11] = 0xbcdef0123456789aULL;
    s[12] = 0xcdef0123456789abULL;
    s[13] = 0xdef0123456789abcULL;
    s[14] = 0xef0123456789abcdULL;
    s[15] = 0xf0123456789abcdeULL;
	print_s();

    for (i = 0; i < 1000; i ++) {
        printf("next = %" PRIu64 " ", next());
		print_s();
    }
}
