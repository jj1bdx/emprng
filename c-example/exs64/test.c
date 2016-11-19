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

extern uint64_t x;

int main(void)
{
    int i;

    x = (uint64_t)1234567890123456789ULL;

    for (i = 0; i < 1000; i ++) {
        printf("next = %" PRIu64 "\n", next());
    }
}
