/* Written in 2014 by Sebastiano Vigna (vigna@acm.org) and 
   Kenji Rikitake (kenji.rikitake@acm.org).

To the extent possible under law, the author has dedicated all copyright
and related and neighboring rights to this software to the public domain
worldwide. This software is distributed without any warranty.

See <http://creativecommons.org/publicdomain/zero/1.0/>. */

#include <stdint.h>

#define UINT58MASK (uint64_t)((1ULL << 58) - 1)

/* This generator is a variant of xorshift128+ for dynamic languages, such
   as Erlang, that can use only 58 bits of a 64-bit integer. Only the lower
   58 bits of each state word are valid (the upper six are zeroes).
   
   This generator passes BigCrush without systematic failures, but due to
   the relatively short period it is acceptable only for applications with
   a mild amount of parallelism; otherwise, use a xorshift1024* generator.

   Note that the lowest bit of this generator is an LSFR, and thus it is
   slightly less random than the other bits. We suggest to use a sign test
   to extract a random Boolean value.

   The state must be seeded so that the lower 58 bits of s[0] and s[1]
   are not all zeroes, and the upper 6 bits are zeroes. If you have a
   nonzero 64-bit seed, we suggest to pass it twice through MurmurHash3's
   avalanching function and take the lower 58 bits, taking care that they
   are not all zeroes (you can apply the avalanching function again if
   this happens). */

uint64_t s[2];

uint64_t next(void) {
	uint64_t s1 = s[0];
	const uint64_t s0 = s[1];
	s[0] = s0;
	s1 ^= (s1 << 24) & UINT58MASK; // a
	s[1] = s1 ^ s0 ^ (s1 >> 11) ^ (s0 >> 41); // b, c
	return (s[1] + s0) & UINT58MASK;
}


/* This is the jump function for the generator. It is equivalent
   to 2^64 calls to next(); it can be used to generate 2^52
   non-overlapping subsequences for parallel computations. */

void jump(void) {
	static const uint64_t JUMP[] = { 0x302f8ea6bc32c797, 0x000d174a83e17de2 };

	uint64_t s0 = 0;
	uint64_t s1 = 0;
	for(int i = 0; i < sizeof JUMP / sizeof *JUMP; i++)
		for(int b = 0; b < 64; b++) {
			if (JUMP[i] & 1ULL << b) {
				s0 ^= s[0];
				s1 ^= s[1];
			}
			next();
		}

	s[0] = s0;
	s[1] = s1;
}
