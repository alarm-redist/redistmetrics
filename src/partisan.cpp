#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(rng = false)]]
NumericMatrix agg_p2d(IntegerMatrix dm, NumericVector vote, int nd) {
  NumericMatrix mat = NumericMatrix(nd, dm.ncol());
  for(int j = 0; j < dm.ncol(); j++){
    for(int i = 0; i < dm.nrow(); i++){
      mat(dm(i,j)-1,j) += vote[i];
    }
  }
  return mat;
}


// [[Rcpp::export(rng = false)]]
IntegerVector dseats(NumericMatrix dcounts, NumericMatrix rcounts){
  IntegerVector dseats(dcounts.ncol());

  for(int c = 0; c < dcounts.ncol(); c++){
    for(int r = 0; r < dcounts.nrow(); r++){
      if(dcounts(r,c) >= rcounts(r,c)){
        dseats(c) += 1;
      }
    }
  }

  return dseats;
}

// [[Rcpp::export(rng = false)]]
IntegerVector dseatsDVS(NumericMatrix dvs){
  IntegerVector dseats = IntegerVector(dvs.ncol());
  for(int c = 0; c < dvs.ncol(); c++){
    for(int r = 0; r < dvs.nrow(); r++){
      if(dvs(r,c) > .5){
        dseats[c] += 1;
      }
    }
  }
  return dseats;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix DVS(NumericMatrix dcounts, NumericMatrix rcounts){
  NumericMatrix mat = NumericMatrix(dcounts.nrow(), dcounts.ncol());

  for(int c = 0; c < mat.ncol(); c++){
    for(int r = 0; r < mat.nrow(); r++){
      mat(r,c) = (double)dcounts(r,c)/(dcounts(r,c)+rcounts(r,c));
    }
  }
  return mat;
}

inline NumericVector calc_seat_share(const IntegerVector& dseat_vec, int nd) {
  return (NumericVector)dseat_vec / (double)nd;
}

NumericMatrix shift_dvs(const NumericMatrix& dvs, const NumericVector& shift) {
  NumericMatrix dvs_shifted = clone(dvs);
  for (int c = 0; c < dvs.ncol(); c++) {
    for (int r = 0; r < dvs.nrow(); r++) {
      dvs_shifted(r, c) += shift(c);
    }
  }
  return dvs_shifted;
}

std::pair<NumericVector, NumericVector> calc_win_sums(const NumericMatrix& dvs) {
  NumericVector Dwin(dvs.ncol());
  NumericVector Rwin(dvs.ncol());

  for (int c = 0; c < dvs.ncol(); c++) {
    for (int r = 0; r < dvs.nrow(); r++) {
      if (dvs(r, c) >= .5) {
        Dwin(c) += dvs(r, c);
      } else {
        Rwin(c) += dvs(r, c);
      }
    }
  }

  return {Dwin, Rwin};
}

// [[Rcpp::export(rng = false)]]
NumericVector effgapEP(NumericMatrix dvs, IntegerVector dseat_vec, int nd){
  NumericVector V = colMeans(dvs);
  NumericVector S = calc_seat_share(dseat_vec, nd);
  NumericVector eg(dseat_vec.size());
  for(int s = 0; s < dseat_vec.size(); s++){
    eg(s) = -2*V[s] + S[s] + .5;
  }
  return -1.0 * eg;
}

// [[Rcpp::export(rng = false)]]
NumericVector effgap(NumericMatrix dcounts, NumericMatrix rcounts, int totvote){
  NumericVector eg(dcounts.ncol());

  NumericMatrix dwaste(dcounts.nrow(), dcounts.ncol());
  NumericMatrix rwaste(rcounts.nrow(), rcounts.ncol());
  int minwin;
  for(int c = 0; c < dcounts.ncol(); c++){
    for(int r = 0; r < dcounts.nrow(); r++){
      minwin = floor((dcounts(r,c) + rcounts(r,c))/2.0)+1;
      if(dcounts(r,c) > rcounts(r,c)){
        dwaste(r,c) += (dcounts(r,c) - minwin);
        rwaste(r,c) += rcounts(r,c);
      } else{
        dwaste(r,c) += dcounts(r,c);
        rwaste(r,c) += (rcounts(r,c) - minwin);
      }
    }
  }

  NumericVector netwaste(dcounts.ncol());
  netwaste = colSums(dwaste) - colSums(rwaste);

  for(int i = 0; i < netwaste.size(); i++){
    eg[i] = netwaste[i]/(double)totvote;
  }

  return eg;
}

// [[Rcpp::export(rng = false)]]
NumericVector taugap(double tau, NumericMatrix dvs, IntegerVector dseat_vec, int nd){
  NumericMatrix ai_mat = NumericMatrix(dvs.nrow(), dvs.ncol());
  IntegerMatrix ei_mat = IntegerMatrix(dvs.nrow(), dvs.ncol());
  NumericMatrix expr = NumericMatrix(ai_mat.nrow(), ai_mat.ncol());
  for(int c = 0; c < ai_mat.ncol(); c++){
    for(int r = 0; r < ai_mat.nrow(); r++){
      ai_mat(r,c) = 2*dvs(r,c) - 1;
      if(ai_mat(r,c) >= 0){
        ei_mat(r,c) = 1;
      } else{
        ei_mat(r,c) = -1;
      }
      expr(r,c) = ei_mat(r,c)*pow(ai_mat(r,c)*ei_mat(r,c), tau+1);
    }
  }

  NumericVector temp = colSums(expr)/2;
  NumericVector dseat_share = calc_seat_share(dseat_vec, nd);

  return 2*(temp +.5 - dseat_share);
}

// [[Rcpp::export(rng = false)]]
NumericVector meanmedian(NumericMatrix dvs){
  NumericVector mm = NumericVector(dvs.ncol());
  NumericVector med = NumericVector(dvs.ncol());
  NumericVector col = dvs(_, 0);
  for(int c = 0; c < dvs.ncol(); c++){
    col = dvs(_,c);
    med(c) = median(col);
  }
  mm = colMeans(dvs) - med;
  return mm;
}

std::pair<NumericVector,NumericVector> decl_components(NumericMatrix dvs, IntegerVector dseat_vec, int nd) {
  auto win_sums = calc_win_sums(dvs);
  NumericVector Dwin = win_sums.first;
  NumericVector Rwin = win_sums.second;

  for(int i = 0; i < Dwin.size(); i++){
    Dwin(i) = Dwin(i)/dseat_vec(i);
    Rwin(i) = Rwin(i)/(nd-dseat_vec(i));
  }

  NumericVector dseatshare = calc_seat_share(dseat_vec, nd);

  std::pair<NumericVector, NumericVector> out = {
    (Dwin - 0.5) / dseatshare,
    (0.5 - Rwin) / (1 - dseatshare)
  };

  return out;
}

// [[Rcpp::export(rng = false)]]
NumericVector declination_simple(NumericMatrix dvs, IntegerVector dseat_vec, int nd){
  std::pair<NumericVector, NumericVector> res = decl_components(dvs, dseat_vec, nd);
  return res.first - res.second;
}

// [[Rcpp::export(rng = false)]]
NumericVector declination_angle(NumericMatrix dvs, IntegerVector dseat_vec, int nd){
  std::pair<NumericVector, NumericVector> res = decl_components(dvs, dseat_vec, nd);
  return atan(res.first) - atan(res.second);
}

// [[Rcpp::export(rng = false)]]
NumericVector lopsidedwins(NumericMatrix dvs, IntegerVector dseat_vec, int nd){
  auto win_sums = calc_win_sums(dvs);
  NumericVector Dwin = win_sums.first;
  NumericVector Rwin = win_sums.second;

  for(int i =0; i < Dwin.size(); i++){
    Dwin(i) = Dwin(i)/dseat_vec(i);
    Rwin(i) = Rwin(i)/(nd-dseat_vec(i));
  }

  return Dwin + Rwin - 1.0;
}


// [[Rcpp::export(rng = false)]]
NumericVector responsiveness(NumericMatrix dvs, double v, int nd, double bandwidth = .01){
  NumericVector right_shift = (v + (bandwidth/2.0)) - colMeans(dvs);
  NumericVector left_shift = (v - (bandwidth/2.0)) - colMeans(dvs);

  NumericMatrix dvs_right = shift_dvs(dvs, right_shift);
  NumericMatrix dvs_left = shift_dvs(dvs, left_shift);

  NumericVector seat_right = (NumericVector)dseatsDVS(dvs_right)/(double)nd;
  NumericVector seat_left = (NumericVector)dseatsDVS(dvs_left)/(double)nd;

  return (seat_right - seat_left)/bandwidth;
}

// [[Rcpp::export(rng = false)]]
NumericVector biasatv(NumericMatrix dvs, double v, int nd){
  NumericVector dshift = (v) - colMeans(dvs);
  NumericVector rshift = (1-v) - colMeans(dvs);

  NumericMatrix dvs_dshift = shift_dvs(dvs, dshift);
  NumericMatrix dvs_rshift = shift_dvs(dvs, rshift);

  NumericVector seat_dshift = (NumericVector)dseatsDVS(dvs_dshift)/(double)nd;
  NumericVector seat_rshift = 1.0 - (NumericVector)dseatsDVS(dvs_rshift)/(double)nd;

  return (seat_rshift - seat_dshift)/2;
}

// [[Rcpp::export(rng = false)]]
NumericVector RankedMarginalDev(NumericMatrix dvs){
  NumericMatrix dvs_sort = 100.0 * dvs;
  NumericVector curr_col(dvs_sort.nrow());
  NumericVector out(dvs_sort.ncol());

  for(int c = 0; c < dvs_sort.ncol(); c++){
    curr_col = dvs_sort(_,c);
    curr_col = curr_col.sort();
    dvs_sort(_,c) = curr_col;
  }

  NumericVector rms = rowMeans(dvs_sort);

  for(int c = 0; c < dvs_sort.ncol(); c++){
    curr_col = dvs_sort(_,c);
    out(c) = sum(pow(curr_col - rms, 2.0));
  }


  return out;
}

// [[Rcpp::export(rng = false)]]
NumericVector smoothseat(NumericMatrix dvs, int nd) {
  NumericVector sscd(dvs.ncol());
  double mindem, maxrep, curr;
  for(int c = 0; c < dvs.ncol(); c++){
    mindem = 1.0;
    maxrep = 0.0;
    for(int r = 0; r < dvs.nrow(); r++){
      curr = (double) dvs(r,c);
      if(dvs(r,c) >= 0.5){
        mindem = std::min<double>(mindem, curr);
      } else{
        maxrep = std::max<double>(maxrep, curr);
      }
    }
    sscd(c) = (0.5 - (1.0 - mindem))/((1.0 - maxrep) - (1.0 - mindem));
  }
  return sscd;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix k_nearest_vote_sums(NumericMatrix distmat,
                                  NumericVector totpop,
                                  double target,
                                  NumericVector rvote,
                                  NumericVector dvote) {
  int m = distmat.nrow();
  NumericMatrix result(m, 2);
  colnames(result) = CharacterVector::create("rvote", "dvote");

  for (int i = 0; i < m; ++i) {
    std::vector<std::pair<double, int>> dist_idx;
    dist_idx.reserve(m - 1);
    for (int j = 0; j < m; ++j) {
      if (i != j) {
        dist_idx.emplace_back(distmat(i, j), j);
      }
    }

    std::sort(dist_idx.begin(), dist_idx.end());

    double cum_pop = 0.0;
    double cum_r = 0.0;
    double cum_d = 0.0;

    for (auto& p : dist_idx) {
      int idx = p.second;
      cum_pop += totpop[idx];
      cum_r += rvote[idx];
      cum_d += dvote[idx];
      if (cum_pop >= target)
        break;
    }

    result(i, 0) = cum_r;
    result(i, 1) = cum_d;
  }

  return result;
}
