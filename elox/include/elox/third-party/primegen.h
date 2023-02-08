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

#ifndef ELOX_PRIMEGEN_H
#define ELOX_PRIMEGEN_H

#include <stddef.h>

typedef struct {
	size_t _start;
} PrimeGen;


void initPrimeGen(PrimeGen *gen, size_t start);

size_t nextPrime(PrimeGen *gen) ;

#endif // ELOX_PRIMEGEN_H
