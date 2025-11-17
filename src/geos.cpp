// needed exactly once in your package or Rcpp script
// contains all the function pointers and the
// implementation of the function to initialize them
// (`libgeos_init_api()`)
#include "libgeos.c"

// this function needs to be called once before any GEOS functions
// are called (e.g., in .onLoad() for your package)
// [[Rcpp::export]]
void cpp_libgeos_init_api() {
  libgeos_init_api();
}
