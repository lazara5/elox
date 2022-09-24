/* MIT License
 *
 * Copyright (c) 2022 Tyge Løvset, NORCE, www.norceresearch.no
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef ELOX_RAND_H
#define ELOX_RAND_H

#include "elox/common.h"

#include <stdint.h>

typedef struct stc64 { uint64_t state[5]; } stc64_t;
typedef struct stc64_uniformf { double lower, range; } stc64_uniformf_t;

/* PRNG stc64.
 * Very fast PRNG suited for parallel usage with Weyl-sequence parameter.
 * 320-bit state, 256 bit is mutable.
 * Noticable faster than xoshiro and pcg, slighly slower than wyrand64 and
 * Romu, but these have restricted capacity for larger parallel jobs or unknown minimum periods.
 * stc64 supports 2^63 unique threads with a minimum 2^64 period lengths each.
 * Passes all statistical tests, e.g PractRand and correlation tests, i.e. interleaved
 * streams with one-bit diff state. Even the 16-bit version (LR=6, RS=5, LS=3) passes
 * PractRand to multiple TB input.
 */

static inline uint64_t stc64_rand(stc64_t *rng);

static inline void stc64_with_seq(stc64_t *rng, uint64_t seed, uint64_t seq ELOX_UNUSED) {
	*rng = (stc64_t){{seed+0x26aa069ea2fb1a4d, seed+0x70c72c95cd592d04,
					  seed+0x504f333d3aa0b359, seed, seed<<1 | 1}};
	for (int i = 0; i < 6; ++i)
		stc64_rand(rng);
}

static inline void stc64_init(stc64_t *rng, uint64_t seed) {
	stc64_with_seq(rng, seed, seed + 0x3504f333d3aa0b37);
}

static inline uint64_t stc64_rand(stc64_t *rng) {
	uint64_t *s = rng->state; enum {LR=24, RS=11, LS=3};
	const uint64_t result = (s[0] ^ (s[3] += s[4])) + s[1];
	s[0] = s[1] ^ (s[1] >> RS);
	s[1] = s[2] + (s[2] << LS);
	s[2] = ((s[2] << LR) | (s[2] >> (64 - LR))) + result;
	return result;
}

/* Float64 random number in range [0.0, 1.0). */
static inline double stc64_randf(stc64_t *rng) {
	union {uint64_t i; double f;} u = {0x3FF0000000000000ull | (stc64_rand(rng) >> 12)};
	return u.f - 1.0;
}

/* Float64 uniform distributed RNG, range [low, high). */
static double stc64_uniformf(stc64_t *rng, stc64_uniformf_t *dist) {
	return stc64_randf(rng) * dist->range + dist->lower;
}

#endif // ELOX_RAND_H
