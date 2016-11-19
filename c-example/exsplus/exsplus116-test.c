/*
 * test sequence generator for xorshiftplus128.c
 * specialized for dieharder test
 * To compile:
 * cc -O3 -o exsplus116-test exsplus116-test.c xorshift116plus.c
 * usage: 
 * ./exsplus116-test | dieharder -a -g 200
 * Written by Kenji Rikitake
 * License: CC0 / public domain
 * NOTE: use C99 or later
 */

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

uint64_t next(void);

/* split into bytes */
union u64bit {
    uint64_t u64;
    uint8_t c[8];
};

extern uint64_t s[2];

int main(void)
{
    int i;
    union u64bit u;

    s[0] = (uint64_t)287716055029699555ULL;
    s[1] = (uint64_t)144656421928717457ULL;

    while (1) {
        u.u64 = next();
        /* use the lower 7 bytes / 56 bits */
        for(i = 0; i < 7; i++) {
            putchar((int)u.c[i]);
        }
    }
}
