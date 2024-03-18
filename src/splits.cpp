#include <Rcpp.h>
#include <vector>
#include <set>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export(rng = false)]]
IntegerVector splits(const IntegerMatrix &dm, const IntegerVector &community,
                     int nd, int max_split, bool skip_last = false) {
  IntegerVector ret(dm.ncol());
  int nc = sort_unique(community).size();
  std::vector<std::vector<bool>> seen(nc);

  // by column (aka map)
  for(int c = 0; c < dm.ncol(); c++){
    for (int i = 0; i < nc; i++) {
      seen[i] = std::vector<bool>(nd, false);
    }
    for(int r = 0; r < dm.nrow(); r++){
      seen[community[r] - 1][dm(r, c) - 1] = true;
    }

    int splits = 0;
    int to = nc;
    if (skip_last) {
      to = nc - 1;
    }
    for (int i = 0; i < to; i++) {
      int tot_split = 0;
      for (int j = 0; j < nd; j++) {
        tot_split += seen[i][j];
        if (tot_split > max_split) {
          splits++;
          break;
        }
      }
    }
    ret[c] = splits;
  }
  return ret;
}

// [[Rcpp::export(rng = false)]]
IntegerMatrix distr_cty_splits(IntegerMatrix dm, IntegerVector community, int nd) {
  IntegerMatrix ret(nd, dm.ncol());
  int nc = sort_unique(community).size();
  IntegerVector com_found(nc, 0);

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
IntegerMatrix admin_splits_count(const IntegerMatrix &dm, const IntegerVector &admin,
                                 int nd, int nc) {
  IntegerMatrix ret(nc, dm.ncol());
  std::vector<std::vector<bool>> seen(nc);

  // by column (aka map)
  for(int c = 0; c < dm.ncol(); c++){
    for (int i = 0; i < nc; i++) {
      seen[i] = std::vector<bool>(nd, false);
    }
    for(int r = 0; r < dm.nrow(); r++){
      seen[admin[r] - 1][dm(r, c) - 1] = true;
    }

    for (int i = 0; i < nc; i++) {
      for (int j = 0; j < nd; j++) {
        ret(i, c) += seen[i][j];
      }
    }
  }

  return ret;
}
