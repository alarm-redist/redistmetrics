#include <Rcpp.h>
#include <iostream>
#include <unistd.h>
using namespace Rcpp;


// [[Rcpp::export(rng = false)]]
NumericMatrix polsbypopper(IntegerVector from,
                           IntegerVector to,
                           NumericVector area,
                           NumericVector perimeter,
                           IntegerMatrix dm,
                           int nd) {

  NumericMatrix ret(nd, dm.ncol());
  NumericVector dist_area(nd);
  NumericVector dist_peri(nd);
  NumericVector dist_peri2(nd);
  NumericVector zerovec(nd);
  int ne = from.size();
  double pi4 = 4.0*3.14159265;

  for(int c = 0; c < dm.ncol(); c++){
    // update holders:
    dist_area = clone(zerovec);
    dist_peri = clone(zerovec);

    // Get a vector of areas ~ just sum
    for(int r = 0; r < dm.nrow(); r++){
      dist_area(dm(r,c) - 1) += area(r);
    }
    // Get a vector of perims ~ sum by id'ing borders
    for(int e = 0; e < ne; e++){
      if(from(e) == -1){
        dist_peri(dm(to(e) - 1, c) - 1) += perimeter(e);
      } else {
        if(dm(from(e) - 1, c) != dm(to(e) - 1, c)){
          dist_peri(dm(to(e) - 1, c) - 1) += perimeter(e);
        }
      }
    }

    dist_peri2 = pow(dist_peri, 2.0);
    ret(_,c) = pi4*dist_area/dist_peri2;
  }

  return ret;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix schwartzberg(IntegerVector from,
                          IntegerVector to,
                          NumericVector area,
                          NumericVector perimeter,
                          IntegerMatrix dm,
                          int nd) {

  NumericMatrix ret(nd, dm.ncol());
  NumericVector dist_area(nd);
  NumericVector dist_peri(nd);
  NumericVector dist_peri2(nd);
  NumericVector zerovec(nd);
  int ne = from.size();
  double pi = 3.14159265;

  for(int c = 0; c < dm.ncol(); c++){
    // update holders:
    dist_area = clone(zerovec);
    dist_peri = clone(zerovec);

    // Get a vector of areas ~ just sum
    for(int r = 0; r < dm.nrow(); r++){
      dist_area(dm(r,c) - 1) += area(r);
    }
    // Get a vector of perims ~ sum by id'ing borders
    for(int e = 0; e < ne; e++){
      if(from(e) == -1){
        dist_peri(dm(to(e) - 1, c) - 1) += perimeter(e);
      } else {
        if(dm(from(e) - 1, c) != dm(to(e) - 1, c)){
          dist_peri(dm(to(e) - 1, c) - 1) += perimeter(e);
        }
      }
    }

    dist_peri2 = sqrt(dist_peri);
    ret(_,c) = 1 / (0.5 * dist_peri / sqrt(pi * dist_area));
  }

  return ret;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix bbox_reock(IntegerMatrix dm,
                         NumericVector areas,
                         NumericMatrix extents,
                         int nd) {

  NumericMatrix out(nd, dm.ncol());
  NumericVector dist_area(nd);
  // NumericVector min_xmin_dist(nd);
  // NumericVector max_xmax_dist(nd);
  // NumericVector min_ymin_dist(nd);
  // NumericVector max_ymax_dist(nd);
  NumericVector zerovec(nd);

  for (int c = 0; c < dm.ncol(); c++) {
    // Initialize aggregation vectors
    dist_area = clone(zerovec);
    NumericVector min_xmin_dist(nd, R_PosInf);
    NumericVector max_xmax_dist(nd, R_NegInf);
    NumericVector min_ymin_dist(nd, R_PosInf);
    NumericVector max_ymax_dist(nd, R_NegInf);

    // Aggregate by district
    for (int r = 0; r < dm.nrow(); r++) {
      int dist = dm(r, c) - 1; // Convert to 0-indexed

      dist_area(dist) += areas(r);

      if (extents(r, 0) < min_xmin_dist[dist]) min_xmin_dist[dist] = extents(r, 0);
      if (extents(r, 1) < min_ymin_dist[dist]) min_ymin_dist[dist] = extents(r, 1);
      if (extents(r, 2) > max_xmax_dist[dist]) max_xmax_dist[dist] = extents(r, 2);
      if (extents(r, 3) > max_ymax_dist[dist]) max_ymax_dist[dist] = extents(r, 3);
    }

    // Compute compactness for each district
    for (int d = 0; d < nd; d++) {
      double mbbox = (max_xmax_dist[d] - min_xmin_dist[d]) *
        (max_ymax_dist[d] - min_ymin_dist[d]);
      out(d, c) = dist_area[d] / mbbox;
    }
  }

  return out;
}
