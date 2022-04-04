#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
IntegerVector hamming(IntegerVector v, IntegerMatrix m) {
  int ham = 0, i = 0, c = 0;
  IntegerVector result(m.ncol());
  for(c = 0; c < m.ncol(); c++){
    ham = 0;
    for(i = 0; i < v.size(); i++){
      if(v[i] != m(i, c)){
        ham++;
      }
    }
    result[c] = ham;
  }
  return result;
}


// [[Rcpp::export(rng = false)]]
NumericVector minkowski(IntegerVector v, IntegerMatrix m, int p) {
  double mink = 0.0, diff;
  int i = 0, c = 0;
  NumericVector result(m.ncol());
  for(c = 0; c < m.ncol(); c++){
    mink = 0;
    for(i = 0; i < v.size(); i++){
      diff = pow((double)(abs(v[i] - m(i, c))), (double)p);
      mink+= diff;
    }
    mink = pow(mink, 1.0/p);
    result[c] = mink;
  }
  return result;
}
