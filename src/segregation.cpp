#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export(rng = false)]]
arma::vec segregationcalc(const arma::umat distmat, const arma::vec grouppop, 
        const arma::vec fullpop) {
  int nd = max(distmat.col(0));
  int V = distmat.n_rows;
  int N = distmat.n_cols;

  // Population parameters
  int T = sum(fullpop);
  double G = sum(grouppop) / T;
  double denom = 2.0 * T * G * (1 - G);

  vec out = zeros(N);
  for (int i = 0; i < N; i++) {
    vec tpop = zeros(nd);
    vec gpop = zeros(nd);

    for (int j = 0; j < V; j++) {      
      tpop[distmat(j, i) - 1] += fullpop(j);
      gpop[distmat(j, i) - 1] += grouppop(j);
    }
    
    out(i) = sum(abs(gpop - G * tpop)) / denom;
  }

  return out;
}


