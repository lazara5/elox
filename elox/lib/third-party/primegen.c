// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Based on Howard Hinnant's implementation, see https://stackoverflow.com/a/5694432

#include <elox/third-party/primegen.h>

#include <stdbool.h>

// handle all next_prime(i) for i in [1, 210), special case 0
static const unsigned small_primes[] =
{
	0,
	2,
	3,
	5,
	7,
	11,
	13,
	17,
	19,
	23,
	29,
	31,
	37,
	41,
	43,
	47,
	53,
	59,
	61,
	67,
	71,
	73,
	79,
	83,
	89,
	97,
	101,
	103,
	107,
	109,
	113,
	127,
	131,
	137,
	139,
	149,
	151,
	157,
	163,
	167,
	173,
	179,
	181,
	191,
	193,
	197,
	199,
	211
};

// potential primes = 210*k + indices[i], k >= 1
//   these numbers are not divisible by 2, 3, 5 or 7
//   (or any integer 2 <= j <= 10 for that matter).
static const unsigned indices[] =
{
	1,
	11,
	13,
	17,
	19,
	23,
	29,
	31,
	37,
	41,
	43,
	47,
	53,
	59,
	61,
	67,
	71,
	73,
	79,
	83,
	89,
	97,
	101,
	103,
	107,
	109,
	113,
	121,
	127,
	131,
	137,
	139,
	143,
	149,
	151,
	157,
	163,
	167,
	169,
	173,
	179,
	181,
	187,
	191,
	193,
	197,
	199,
	209
};

static int lower_bound(const unsigned int *a, int n, unsigned x) {
	int l = 0;
	int h = n;
	while (l < h) {
		int mid =  l + (h - l) / 2;
		if (x <= a[mid]) {
			h = mid;
		} else {
			l = mid + 1;
		}
	}
	return l;
}

// Returns:  If n == 0, returns 0.  Else returns the lowest prime number that
// is greater than or equal to n.
//
// The algorithm creates a list of small primes, plus an open-ended list of
// potential primes.  All prime numbers are potential prime numbers.  However
// some potential prime numbers are not prime.  In an ideal world, all potential
// prime numbers would be prime.  Candidate prime numbers are chosen as the next
// highest potential prime.  Then this number is tested for prime by dividing it
// by all potential prime numbers less than the sqrt of the candidate.
//
// This implementation defines potential primes as those numbers not divisible
// by 2, 3, 5, and 7.  Other (common) implementations define potential primes
// as those not divisible by 2.  A few other implementations define potential
// primes as those not divisible by 2 or 3.  By raising the number of small
// primes which the potential prime is not divisible by, the set of potential
// primes more closely approximates the set of prime numbers.  And thus there
// are fewer potential primes to search, and fewer potential primes to divide
// against.


static size_t next_prime(size_t n) {
	const size_t L = 210;
	const size_t N = sizeof(small_primes) / sizeof(small_primes[0]);
	// If n is small enough, search in small_primes
	if (n <= small_primes[N-1])
		return small_primes[lower_bound(small_primes, N, n)];
	// Else n > largest small_primes
	// Start searching list of potential primes: L * k0 + indices[in]
	const size_t M = sizeof(indices) / sizeof(indices[0]);
	// Select first potential prime >= n
	//   Known a-priori n >= L
	size_t k0 = n / L;
	size_t in = (size_t)(lower_bound(indices, M, n - k0 * L));
	n = L * k0 + indices[in];
	while (true) {
		// Divide n by all primes or potential primes (i) until:
		//    1.  The division is even, so try next potential prime.
		//    2.  The i > sqrt(n), in which case n is prime.
		// It is known a-priori that n is not divisible by 2, 3, 5 or 7,
		//    so don't test those (j == 5 ->  divide by 11 first).  And the
		//    potential primes start with 211, so don't test against the last
		//    small prime.
		for (size_t j = 5; j < N - 1; ++j) {
			const size_t p = small_primes[j];
			const size_t q = n / p;
			if (q < p)
				return n;
			if (n == q * p)
				goto next;
		}
		// n wasn't divisible by small primes, try potential primes
		{
			size_t i = 211;
			while (true) {
				size_t q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 10;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 8;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 8;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 6;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 4;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 2;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				i += 10;
				q = n / i;
				if (q < i)
					return n;
				if (n == q * i)
					break;

				// This will loop i to the next "plane" of potential primes
				i += 2;
			}
		}
next:
		// n is not prime.  Increment n to next potential prime.
		if (++in == M) {
			++k0;
			in = 0;
		}
		n = L * k0 + indices[in];
	}
}

void initPrimeGen(PrimeGen *gen, size_t start) {
	gen->_start = next_prime(start) + 1;
}

size_t nextPrime(PrimeGen *gen) {
	// for our purposes, 1 IS prime
	size_t n = (gen->_start == 1) ? 1 : next_prime(gen->_start);
	gen->_start = n + 1;
	return n;
}
