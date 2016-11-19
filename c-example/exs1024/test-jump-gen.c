/*
 * test sequence generator for jump function of xorshift1024star.c
 * Written by Kenji Rikitake
 * License: CC0 / public domain
 * NOTE: use C99 or later
 */

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

uint64_t next(void);
void jump(void);

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
    uint64_t r;

    p = 0;
    s[0] = 12345678ULL;
    s[1] = 12345678ULL;
    s[2] = 12345678ULL;
    s[3] = 12345678ULL;
    s[4] = 12345678ULL;
    s[5] = 12345678ULL;
    s[6] = 12345678ULL;
    s[7] = 12345678ULL;
    s[8] = 12345678ULL;
    s[9] = 12345678ULL;
    s[10] = 12345678ULL;
    s[11] = 12345678ULL;
    s[12] = 12345678ULL;
    s[13] = 12345678ULL;
    s[14] = 12345678ULL;
    s[15] = 12345678ULL;

    for (i = 1000; i > 0; i--) {
        next();
        jump();
        r = (next() % 18446744073709551615ULL) + 1ULL;
        if ((i % 10) == 0) {
            printf("%" PRIu64 ",\n", r);
        }
    }
}
