#ifndef SOUL_CONFIG_H
#define SOUL_CONFIG_H

// Maximum number of local variables.
#ifndef SOUL_MAX_LOCAL_VARIABLES
	#define SOUL_MAX_LOCAL_VARIABLES 256
#endif // SOUL_MAX_LOCAL_VARIABLES

// Minimal capacity of the dynamic arrays.
#ifndef SOUL_ARRAY_MIN_CAPACITY
	#define SOUL_ARRAY_MIN_CAPACITY 8
#endif // SOUL_ARRAY_MIN_CAPACITY

// Factor by which the array's capacity will grow.
#ifndef SOUL_ARRAY_CAPACITY_GROWTH_RATE
	#define SOUL_ARRAY_CAPACITY_GROWTH_RATE 2
#endif // SOUL_ARRAY_CAPACITY_GROWTH_RATE

#endif // SOUL_CONFIG_H
