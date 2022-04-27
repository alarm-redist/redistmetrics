#ifndef KIRCHHOFF_INLINE_H
#define KIRCHHOFF_INLINE_H

#include <RcppArmadillo.h>
#include "redistmetrics_types.h"

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


/*
 * Compute the log number of spanning trees for `district` intersect `county`
 */
// TESTED
inline double log_st_distr(const Graph &g, const umat &districts, const uvec &counties,
                           int idx, int district, int county) {
  int V = g.size();
  // number of precincts in this district
  int K = 0;
  std::vector<int> pos(V); // keep track of positions in subgraph
  int start = 0; // where to start loop below, to save time
  for (int i = 0; i < V; i++) {
    pos[i] = K - 1; // minus one because we're dropping 1st row and column
    if (districts(i, idx) == district && counties(i) == county) {
      K++;
      if (K == 2) start = i; // start 2nd vertex
    }
  }
  if (K <= 1) return 0;

  mat adj = zeros<mat>(K-1, K-1); // adjacency matrix (minus 1st row and column)
  for (int i = start; i < V; i++) {
    if (districts(i, idx) != district || counties(i) != county) continue;

    int prec = pos.at(i);
    if (prec < 0) continue;
    std::vector<int> nbors = g[i];
    int length = nbors.size();
    int degree = 0; // keep track of index within subgraph
    for (int j = 0; j < length; j++) {
      int nbor = nbors[j];
      if (districts(nbor, idx) != district || counties(nbor) != county) continue;
      degree++;
      if (pos.at(nbor) < 0) continue;
      adj(prec, pos[nbor]) = -1;
    }
    adj(prec, prec) = degree;
  }

  double lst, sign;
  log_det(lst, sign, adj);
  return lst;
}

/*
 * Compute the log number of spanning trees for the contracted graph
 */
// TESTED
inline double log_st_contr(const Graph &g, const umat &districts, const uvec &counties,
                           int n_cty, int idx, int district) {
  if (n_cty == 1) return 0;
  int V = g.size();
  // number of counties in this district
  int K = 0;
  std::vector<int> pos(V); // keep track of positions in subgraph
  std::vector<int> seen(n_cty, -2); // county lookup
  int start = 0;
  for (int i = 0; i < V; i++) {
    if (districts(i, idx) != district) continue;

    if (seen[counties(i)-1] < 0) {
      pos.at(i) = K - 1; // minus one because we're dropping 1st row and column
      seen[counties(i)-1] = K;
      K++;
      if (K == 2) start = i; // start 2nd vertex
    } else {
      pos.at(i) = seen.at(counties(i)-1) - 1;
    }
  }
  if (K <= 1) return 0;

  mat adj = zeros<mat>(K-1, K-1); // adjacency matrix (minus 1st row and column)
  for (int i = start; i < V; i++) {
    if (districts(i, idx) != district) continue;

    int cty = pos[i];
    if (cty < 0) continue; // skip 1st row, col
    std::vector<int> nbors = g[i];
    int length = nbors.size();
    for (int j = 0; j < length; j++) {
      int nbor = nbors.at(j);
      if (districts(nbor, idx) != district || pos.at(nbor) == cty) continue;
      adj(cty, cty)++;
      if (pos[nbor] < 0) continue; // ignore 1st row and column
      adj(cty, pos[nbor])--;
    }
  }

  double lst, sign;
  log_det(lst, sign, adj);
  return lst;
}


#endif
