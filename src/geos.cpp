// this is copied directly from the readme of libgeos as a required fn
#include "libgeos.c"

// [[Rcpp::export(rng = false)]]
void cpp_libgeos_init_api() {
  libgeos_init_api();
}
