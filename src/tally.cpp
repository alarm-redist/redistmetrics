#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
NumericMatrix tally_var(IntegerMatrix dm, NumericVector var, int nd) {
  int N = dm.ncol();
  int V = dm.nrow();
  NumericMatrix out(nd, N);

  for (int i = 0; i < N; i++) {
    for (int j = 0; j < V; j++) {
      out(dm(j, i) - 1, i) += var[j];
    }
  }

  return out;
}

