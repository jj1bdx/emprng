#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>

#define PRIME1 (30269)
#define PRIME2 (30307)
#define PRIME3 (30323)
#define MULT ((uint64_t)PRIME1 * (uint64_t)PRIME2 * (uint64_t)PRIME3)

int main(int argc, char *argv[]) {
    uint32_t x, y, z;
    uint64_t s;

    for (x = 1; x < PRIME1; x++) {
        printf("x = %d\n", x);
        for (y = 1; y < PRIME2; y++) {
            for (z = 1; z < PRIME3; z++) {
                s = (((uint64_t)x * (uint64_t)PRIME2 * (uint64_t)PRIME3) +
                     ((uint64_t)y * (uint64_t)PRIME3 * (uint64_t)PRIME1) +
                     ((uint64_t)z * (uint64_t)PRIME1 * (uint64_t)PRIME2));
                if (s % MULT == 0) {
                    printf("x = %d, y = %d, z = %d, s = %" PRIu64 "\n", x, y, z, s);
                }
            }
        }
    }
}
