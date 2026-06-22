#include <Rcpp.h>
#include <cmath>
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
                         const int nd) {
  const int n = dm.nrow();
  const int n_plans = dm.ncol();

  NumericMatrix out(nd, n_plans);
  std::vector<double> dist_area(nd);

  std::vector<double> min_xmin_dist(nd);
  std::vector<double> max_xmax_dist(nd);
  std::vector<double> min_ymin_dist(nd);
  std::vector<double> max_ymax_dist(nd);
  for (int c = 0; c < dm.ncol(); c++) {
    // Initialize aggregation vectors
    for (int d = 0; d < nd; d++) {
      dist_area[d] = 0.0;
      min_xmin_dist[d] = INFINITY;
      max_xmax_dist[d] = -INFINITY;
      min_ymin_dist[d] = INFINITY;
      max_ymax_dist[d] = -INFINITY;
    }

    // Aggregate by district
    for (int r = 0; r < n; r++) {
      int dist = dm(r, c) - 1; // Convert to 0-indexed

      dist_area[dist] += areas(r);
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

// [[Rcpp::export(rng = false)]]
NumericVector fryer_holden(IntegerMatrix dm,
                           NumericVector pop,
                           NumericMatrix coords,
                           const int nd) {
  const int n = dm.nrow();
  const int n_plans = dm.ncol();
  NumericVector out(n_plans);
  std::vector<double> weight(nd), weighted_r2(nd), weighted_x(nd), weighted_y(nd);

  for (int p = 0; p < n_plans; p++) {
    std::fill(weight.begin(), weight.end(), 0.0);
    std::fill(weighted_r2.begin(), weighted_r2.end(), 0.0);
    std::fill(weighted_x.begin(), weighted_x.end(), 0.0);
    std::fill(weighted_y.begin(), weighted_y.end(), 0.0);

    for (int i = 0; i < n; i++) {
      const int d = dm(i, p) - 1;
      const double w = pop[i];
      const double x = coords(i, 0);
      const double y = coords(i, 1);
      weight[d] += w;
      weighted_r2[d] += w * (x * x + y * y);
      weighted_x[d] += w * x;
      weighted_y[d] += w * y;
    }

    double total = 0.0;
    for (int d = 0; d < nd; d++) {
      total += 2.0 * (weight[d] * weighted_r2[d] -
        weighted_x[d] * weighted_x[d] - weighted_y[d] * weighted_y[d]);
    }
    out[p] = total;
  }

  return out;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix length_width(IntegerMatrix dm,
                           NumericMatrix extents,
                           const int nd) {
  const int n = dm.nrow();
  const int n_plans = dm.ncol();
  NumericMatrix out(nd, n_plans);
  std::vector<double> xmin(nd), ymin(nd), xmax(nd), ymax(nd);

  for (int p = 0; p < n_plans; p++) {
    std::fill(xmin.begin(), xmin.end(), INFINITY);
    std::fill(ymin.begin(), ymin.end(), INFINITY);
    std::fill(xmax.begin(), xmax.end(), -INFINITY);
    std::fill(ymax.begin(), ymax.end(), -INFINITY);
    for (int i = 0; i < n; i++) {
      const int d = dm(i, p) - 1;
      xmin[d] = std::min(xmin[d], extents(i, 0));
      ymin[d] = std::min(ymin[d], extents(i, 1));
      xmax[d] = std::max(xmax[d], extents(i, 2));
      ymax[d] = std::max(ymax[d], extents(i, 3));
    }
    for (int d = 0; d < nd; d++) {
      const double width = xmax[d] - xmin[d];
      const double height = ymax[d] - ymin[d];
      out(d, p) = std::min(width, height) / std::max(width, height);
    }
  }
  return out;
}
