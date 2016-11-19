/*
 * test sequence generator for jump function of xorshiftplus128.c
 * Written by Kenji Rikitake
 * License: CC0 / public domain
 * NOTE: use C99 or later
 */

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

uint64_t next(void);
void jump(void);

extern uint64_t s[2];

void print_s(void)
{
    printf("s[0] = %" PRIu64 " s[1] = %" PRIu64 "\n",
            s[0], s[1]);
}

int main(void)
{
    int i;
    uint64_t r;

    s[0] = (uint64_t)12345678ULL;
    s[1] = (uint64_t)12345678ULL;

    for (i = 1000; i > 0; i--) {
        next();
        jump();
        r = (next() % 288230376151711743ULL) + 1ULL;
        if ((i % 10) == 0) {
            printf("%" PRIu64 ",\n", r); 
        }
    }
}
