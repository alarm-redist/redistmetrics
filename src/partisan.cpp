#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(rng = false)]]
IntegerMatrix agg_p2d(IntegerMatrix dm, IntegerVector vote, int nd) {
  IntegerMatrix mat = IntegerMatrix(nd, dm.ncol());
  for(int j = 0; j < dm.ncol(); j++){
    for(int i = 0; i < dm.nrow(); i++){
      mat(dm(i,j)-1,j) += vote[i];
    }
  }
  return mat;
}


// [[Rcpp::export(rng = false)]]
IntegerVector dseats(IntegerMatrix dm, IntegerMatrix dcounts, IntegerMatrix rcounts, int nd){
  IntegerVector dseats(dm.ncol());

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
NumericMatrix DVS(IntegerMatrix dcounts, IntegerMatrix rcounts){
  NumericMatrix mat = NumericMatrix(dcounts.nrow(), dcounts.ncol());

  for(int c = 0; c < mat.ncol(); c++){
    for(int r = 0; r < mat.nrow(); r++){
      mat(r,c) = (double)dcounts(r,c)/(dcounts(r,c)+rcounts(r,c));
    }
  }
  return mat;
}

// [[Rcpp::export(rng = false)]]
NumericVector effgapEP(NumericMatrix dvs, IntegerVector dseat_vec, int nd){
  NumericVector V = colMeans(dvs);
  NumericVector S(dseat_vec.size());
  for(int s = 0; s < dseat_vec.size(); s++){
    S[s] = dseat_vec[s]/(double) nd;
  }
  NumericVector eg(dseat_vec.size());
  for(int s = 0; s < dseat_vec.size(); s++){
    eg(s) = -2*V[s] + S[s] + .5;
  }
  return -1.0 * eg;
}

// [[Rcpp::export(rng = false)]]
NumericVector effgap(IntegerMatrix dcounts, IntegerMatrix rcounts, int totvote){
  NumericVector eg(dcounts.ncol());

  IntegerMatrix dwaste(dcounts.nrow(), dcounts.ncol());
  IntegerMatrix rwaste(rcounts.nrow(), rcounts.ncol());
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

  IntegerVector netwaste(dcounts.ncol());
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
  NumericVector dseat_share = NumericVector(dseat_vec.size());
  for(int i =0; i < dseat_share.size(); i++){
    dseat_share(i) = dseat_vec(i)/(double) nd;
  }

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
  NumericVector Dwin = NumericVector(dvs.ncol());
  NumericVector Rwin = NumericVector(dvs.ncol());

  for(int c = 0; c < dvs.ncol(); c++){
    for(int r = 0; r < dvs.nrow(); r++){
      if(dvs(r,c) >= .5){
        Dwin(c) += dvs(r,c);
      } else{
        Rwin(c) += dvs(r,c);
      }
    }
  }
  for(int i = 0; i < Dwin.size(); i++){
    Dwin(i) = Dwin(i)/dseat_vec(i);
    Rwin(i) = Rwin(i)/(nd-dseat_vec(i));
  }

  NumericVector dseatshare = (NumericVector)dseat_vec/(double)nd;

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
  NumericVector Dwin = NumericVector(dvs.ncol());
  NumericVector Rwin = NumericVector(dvs.ncol());

  for(int c = 0; c < dvs.ncol(); c++){
    for(int r = 0; r < dvs.nrow(); r++){
      if(dvs(r,c) >= .5){
        Dwin(c) += dvs(r,c);
      } else{
        Rwin(c) += dvs(r,c);
      }
    }
  }
  for(int i =0; i < Dwin.size(); i++){
    Dwin(i) = Dwin(i)/dseat_vec(i);
    Rwin(i) = Rwin(i)/(nd-dseat_vec(i));
  }

  return Dwin + Rwin - 1.0;
}


// [[Rcpp::export(rng = false)]]
NumericVector responsiveness(NumericMatrix dvs, double v, int nd, double bandwidth = .01){
  NumericVector right = (v + (bandwidth/2.0)) - colMeans(dvs);
  NumericVector left = (v - (bandwidth/2.0)) - colMeans(dvs);
  NumericMatrix dvs_right = clone(dvs);
  NumericMatrix dvs_left = clone(dvs);

  for(int c = 0; c < dvs.ncol(); c++){
    for(int r = 0; r < dvs.nrow(); r++){
      dvs_right(r,c) += right(c);
      dvs_left(r,c) += left(c);
    }
  }

  NumericVector seat_right = (NumericVector)dseatsDVS(dvs_right)/(double)nd;
  NumericVector seat_left = (NumericVector)dseatsDVS(dvs_left)/(double)nd;

  return (seat_right - seat_left)/bandwidth;
}

// [[Rcpp::export(rng = false)]]
NumericVector biasatv(NumericMatrix dvs, double v, int nd){
  NumericVector dshift = (v) - colMeans(dvs);
  NumericVector rshift = (1-v) - colMeans(dvs);
  NumericMatrix dvs_dshift = clone(dvs);
  NumericMatrix dvs_rshift = clone(dvs);

  for(int c = 0; c < dvs.ncol(); c++){
    for(int r = 0; r < dvs.nrow(); r++){
      dvs_dshift(r,c) += dshift(c);
      dvs_rshift(r,c) += rshift(c);
    }
  }

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
