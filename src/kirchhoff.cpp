#include "kirchhoff.h"
#include "../inst/include/kirchhoff_inline.h"

/*
 * Compute the log number of spanning trees which could generate a given set of maps.
 * `districts` should have each column be a map
 */
// TESTED
NumericVector log_st_map(const Graph &g, const umat &districts,
                         const uvec &counties, int n_distr) {
  int N = districts.n_cols;
  int n_cty = max(counties);
  NumericVector log_st(N);
  for (int i = 0; i < N; i++) {
    double accuml = 0;
    for (int d = 1; d <= n_distr; d++) { // districts are 1-indexed
      for (int j = 1; j <= n_cty; j++) {
        accuml += log_st_distr(g, districts, counties, i, d, j);
      }
      accuml += log_st_contr(g, districts, counties, n_cty, i, d);
    }
    log_st(i) = accuml;
  }
  return log_st;
}


/*
 * Compute the number of edges removed
 */
// TESTED
NumericVector n_removed(const Graph &g, const umat &districts, int n_distr) {
  int V = g.size();
  int N = districts.n_cols;
  NumericVector n_rem(N);
  for (int n = 0; n < N; n++) {
    double removed = 0.0;
    for (int i = 0; i < V; i++) {
      int dist = districts(i, n);
      std::vector<int> nbors = g[i];
      int length = nbors.size();
      for (int j = 0; j < length; j++) {
        if (districts(nbors[j], n) != dist) removed += 1.0;
      }
    }
    n_rem[n] = removed;
  }

  return n_rem/2;
}
