#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export(rng = false)]]
IntegerVector splits(IntegerMatrix dm, IntegerVector community, int nd, int max_split) {
  IntegerVector ret(dm.ncol());
  IntegerVector com_name = sort_unique(community);
  int nc = com_name.size();
  IntegerMatrix com_found(nc, nd);
  IntegerVector mid;

  // by column (aka map)
  for(int c = 0; c < dm.ncol(); c++){
    com_found = IntegerMatrix(nc, nd);
    // by district
    for(int d = 0; d < nd; d++){
      // across all rows
      for(int r = 0; r < dm.nrow(); r++){
        if (dm(r,c) == d) {
          com_found(community(r), d) = 1;
        }
      }
    }

    mid = rowSums(com_found);
    for(int q = 0; q < mid.size(); q++){
      if (mid(q) > max_split) {
        ret(c)++;
      }
    }
  }
  return ret;
}

// [[Rcpp::export(rng = false)]]
IntegerMatrix distr_cty_splits(IntegerMatrix dm, IntegerVector community, int nd) {
  IntegerMatrix ret(nd, dm.ncol());
  IntegerVector com_name = sort_unique(community);
  IntegerVector com_found(com_name.size(), 0);

  // by column (aka map)
  for(int c = 0; c < dm.ncol(); c++){
    // by district
    for(int d = 0; d < nd; d++){
      com_found = IntegerVector(com_found.size(), 0);
      // across all rows
      for(int r = 0; r < dm.nrow(); r++){
        if (dm(r,c) == d) {
          com_found(community(r)) = 1;
        }
      }
      ret(d, c) = sum(com_found);
    }
  }
  return ret;
}

// [[Rcpp::export(rng = false)]]
IntegerMatrix admin_splits_count(IntegerMatrix dm, IntegerVector admin) {
  IntegerVector com_name = sort_unique(admin);
  int asize = com_name.size();
  IntegerMatrix ret(asize, dm.ncol());
  IntegerVector cur_col(dm.nrow());
  IntegerVector temp;

  // by map (column)
  for (int c = 0; c < dm.ncol(); c++) {
    cur_col = dm(_, c);

    // by admin :
    for (int a = 0; a < asize; a++) {
      temp = cur_col[admin == a];
      ret(a, c) = unique(temp).size();
    }
  }

  return ret;
}
