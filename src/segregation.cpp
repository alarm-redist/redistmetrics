#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export(rng = false)]]
NumericVector segregationcalc(NumericMatrix distmat,
                              NumericVector grouppop,
                              NumericVector fullpop) {
  // Vector to hold dissimilarity indices
  NumericVector diVec(distmat.ncol());

  // Population parameters
  int T = sum(fullpop);
  double P = sum(grouppop) / T;

  // Calculate denominators
  double d = 1.0 / (2.0 * T * P * (1 - P));

  int nd = max(distmat(_, 0));
  int V = distmat.nrow();
  int N = distmat.ncol();

  // Loop over possible plans
  for (int i = 0; i < N; i++) {
    double dissim = 0.0;
    vec cds = as<vec>(distmat(_, i));
    vec tpop = fill::zeros(nd);
    vec gpop = fill::zeros(nd);

    for (int j = 0; j < V; j++) {      
      tpop[cds[j]] += fullpop(j);
      gpop[cds[j]] += grouppop(j);
    }
    
    diVec(i) = sum(d * abs(gpop - P * tpop));
  }

  return diVec;
}


