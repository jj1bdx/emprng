/*
 * The MIT License (MIT)
 * 
 * Copyright (c) 2015 Kenji Rikitake.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/* range-check.c: a brute-force testing code
 * to check if the base calculated value of AS183
 * will be zero or not */

#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>

#define PRIME1 (30269)
#define PRIME2 (30307)
#define PRIME3 (30323)
#define MULT ((uint64_t)PRIME1 * (uint64_t)PRIME2 * (uint64_t)PRIME3)

int main(int argc, char *argv[]) {
    uint32_t x, y, z;
    uint64_t s, v1, v2;

    v1 = MULT; // one
    v2 = MULT + MULT; // two

    for (x = 1; x < PRIME1; x++) {
        printf("x = %d\n", x);
        for (y = 1; y < PRIME2; y++) {
            for (z = 1; z < PRIME3; z++) {
                s = (((uint64_t)x * (uint64_t)PRIME2 * (uint64_t)PRIME3) +
                     ((uint64_t)y * (uint64_t)PRIME3 * (uint64_t)PRIME1) +
                     ((uint64_t)z * (uint64_t)PRIME1 * (uint64_t)PRIME2));
                if ((s == v1) || (s == v2)) {
                    printf("x = %d, y = %d, z = %d, s = %" PRIu64 "\n", x, y, z, s);
                }
            }
        }
    }
}
