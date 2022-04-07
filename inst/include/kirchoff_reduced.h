#ifndef KIRCHHOFF_H
#define KIRCHHOFF_H

#include <RcppArmadillo.h>
#include "redistmetrics_types.h"

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


/*
 * Compute the log number of spanning trees for `district` intersect `county`
 */
// TESTED
double log_st_distr(const Graph &g, const umat &districts, const uvec &counties,
                    int idx, int district, int county);

/*
 * Compute the log number of spanning trees for the contracted graph
 */
// TESTED
double log_st_contr(const Graph &g, const umat &districts, const uvec &counties,
                    int n_cty, int idx, int district);


#endif
