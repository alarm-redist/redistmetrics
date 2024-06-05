#include <RcppArmadillo.h>
using namespace Rcpp;

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

  // Get the number of unique plans
  NumericVector cd1 = distmat(_, 0);
  arma::vec cdVec1 = as<arma::vec> (cd1);
  arma::vec cdLabs = arma::unique(cdVec1);

  // Range to look over for CDs
  int end = max(cd1) + 1;
  int start = min(cd1) == 1 ? 1 : 0;

  // Loop over possible plans
  for (int i = 0; i < distmat.ncol(); i++) {
    double dissim = 0.0;
    arma::vec cds = as<arma::vec> (distmat(_, i));

    for (int j = start; j < end; j++) {
      double tpop = 0.0;
      double gpop = 0.0;

      // Which precincts in the plan are in this cd?
      arma::uvec findCds = find(cds == j);

      // Loop over precincts
      for (int k = 0; k < findCds.size(); k++) {
        tpop += fullpop(findCds(k));
        gpop += grouppop(findCds(k));
      }

      if (tpop > 0.0) {
        dissim += d * tpop * std::abs(gpop / tpop - P);
      }
    }

    diVec(i) = dissim;
  }

  return diVec;
}


