#ifndef KIRCHHOFF_H
#define KIRCHHOFF_H

#include <RcppArmadillo.h>
#include "../inst/include/redistmetrics_types.h"

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]
using namespace Rcpp;
using namespace arma;

/*
 * Compute the log number of spanning trees which could generate a given set of maps.
 * `districts` should have each column be a map
 */
// TESTED
// [[Rcpp::export(rng = false)]]
NumericVector log_st_map(const Graph &g, const arma::umat &districts,
                         const arma::uvec &counties, int n_distr);

/*
 * Compute the number of edges removed
 */
// TESTED
// [[Rcpp::export(rng = false)]]
NumericVector n_removed(const Graph &g, const arma::umat &districts, int n_distr);

#endif
