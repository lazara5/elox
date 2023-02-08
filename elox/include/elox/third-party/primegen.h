#ifndef ELOX_PRIMEGEN_H
#define ELOX_PRIMEGEN_H

#include <stddef.h>

typedef struct {
	size_t _start;
} PrimeGen;


void initPrimeGen(PrimeGen *gen, size_t start);

size_t nextPrime(PrimeGen *gen) ;

#endif // ELOX_PRIMEGEN_H
