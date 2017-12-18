#include <gsl/gsl_rng.h>        	// GSL random number generators
#include <gsl/gsl_randist.h>    	// GSL random distributions
#include <time.h>

// Create a pointer to any rng
gsl_rng *rng_ptr;

// Creates a seed for the setrng_() function
unsigned long int random_seed (){
    unsigned long int seed = (long unsigned int)(time(0)*time(0));
    return (seed);
}

// Setting-up the pointer
void setrnd_( ){
    // Allocate an rng
    rng_ptr = gsl_rng_alloc(gsl_rng_taus);

    // Seed the rng
    gsl_rng_set(rng_ptr, random_seed());
}

// Get random number value from a sequence of uniformely distributed
// between 0 and 1
void getrnd_(double *rnd){
    *rnd = gsl_ran_flat(rng_ptr, 0., 1.);
}
