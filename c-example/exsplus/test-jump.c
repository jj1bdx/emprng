/*
 * test sequence generator for xorshiftplus128.c
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

    s[0] = (uint64_t)287716055029699555ULL;
    s[1] = (uint64_t)144656421928717457ULL;
    print_s();

    for (i = 0; i < 100; i++) {
        printf("next = %" PRIu64 " ", next());
        print_s();
        jump();
    }
}
