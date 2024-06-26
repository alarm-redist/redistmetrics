// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/redistmetrics.h"
#include "../inst/include/redistmetrics_types.h"
#include <RcppArmadillo.h>
#include <RcppThread.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// polsbypopper
NumericMatrix polsbypopper(IntegerVector from, IntegerVector to, NumericVector area, NumericVector perimeter, IntegerMatrix dm, int nd);
RcppExport SEXP _redistmetrics_polsbypopper(SEXP fromSEXP, SEXP toSEXP, SEXP areaSEXP, SEXP perimeterSEXP, SEXP dmSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type from(fromSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type to(toSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type area(areaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type perimeter(perimeterSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type dm(dmSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(polsbypopper(from, to, area, perimeter, dm, nd));
    return rcpp_result_gen;
END_RCPP
}
// schwartzberg
NumericMatrix schwartzberg(IntegerVector from, IntegerVector to, NumericVector area, NumericVector perimeter, IntegerMatrix dm, int nd);
RcppExport SEXP _redistmetrics_schwartzberg(SEXP fromSEXP, SEXP toSEXP, SEXP areaSEXP, SEXP perimeterSEXP, SEXP dmSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type from(fromSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type to(toSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type area(areaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type perimeter(perimeterSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type dm(dmSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(schwartzberg(from, to, area, perimeter, dm, nd));
    return rcpp_result_gen;
END_RCPP
}
// talisman
NumericVector talisman(NumericMatrix dvs, double nd, double alpha, double beta);
RcppExport SEXP _redistmetrics_talisman(SEXP dvsSEXP, SEXP ndSEXP, SEXP alphaSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    Rcpp::traits::input_parameter< double >::type nd(ndSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(talisman(dvs, nd, alpha, beta));
    return rcpp_result_gen;
END_RCPP
}
// contiguity
IntegerVector contiguity(List adj, IntegerVector group);
RcppExport SEXP _redistmetrics_contiguity(SEXP adjSEXP, SEXP groupSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< List >::type adj(adjSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type group(groupSEXP);
    rcpp_result_gen = Rcpp::wrap(contiguity(adj, group));
    return rcpp_result_gen;
END_RCPP
}
// hamming
IntegerVector hamming(IntegerVector v, IntegerMatrix m);
RcppExport SEXP _redistmetrics_hamming(SEXP vSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(hamming(v, m));
    return rcpp_result_gen;
END_RCPP
}
// minkowski
NumericVector minkowski(IntegerVector v, IntegerMatrix m, int p);
RcppExport SEXP _redistmetrics_minkowski(SEXP vSEXP, SEXP mSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(minkowski(v, m, p));
    return rcpp_result_gen;
END_RCPP
}
// log_st_map
NumericVector log_st_map(const Graph& g, const arma::umat& districts, const arma::uvec& counties, int n_distr);
static SEXP _redistmetrics_log_st_map_try(SEXP gSEXP, SEXP districtsSEXP, SEXP countiesSEXP, SEXP n_distrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Graph& >::type g(gSEXP);
    Rcpp::traits::input_parameter< const arma::umat& >::type districts(districtsSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type counties(countiesSEXP);
    Rcpp::traits::input_parameter< int >::type n_distr(n_distrSEXP);
    rcpp_result_gen = Rcpp::wrap(log_st_map(g, districts, counties, n_distr));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _redistmetrics_log_st_map(SEXP gSEXP, SEXP districtsSEXP, SEXP countiesSEXP, SEXP n_distrSEXP) {
    SEXP rcpp_result_gen;
    {
        rcpp_result_gen = PROTECT(_redistmetrics_log_st_map_try(gSEXP, districtsSEXP, countiesSEXP, n_distrSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// n_removed
NumericVector n_removed(const Graph& g, const arma::umat& districts, int n_distr);
static SEXP _redistmetrics_n_removed_try(SEXP gSEXP, SEXP districtsSEXP, SEXP n_distrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Graph& >::type g(gSEXP);
    Rcpp::traits::input_parameter< const arma::umat& >::type districts(districtsSEXP);
    Rcpp::traits::input_parameter< int >::type n_distr(n_distrSEXP);
    rcpp_result_gen = Rcpp::wrap(n_removed(g, districts, n_distr));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _redistmetrics_n_removed(SEXP gSEXP, SEXP districtsSEXP, SEXP n_distrSEXP) {
    SEXP rcpp_result_gen;
    {
        rcpp_result_gen = PROTECT(_redistmetrics_n_removed_try(gSEXP, districtsSEXP, n_distrSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// agg_p2d
NumericMatrix agg_p2d(IntegerMatrix dm, NumericVector vote, int nd);
RcppExport SEXP _redistmetrics_agg_p2d(SEXP dmSEXP, SEXP voteSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type dm(dmSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vote(voteSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(agg_p2d(dm, vote, nd));
    return rcpp_result_gen;
END_RCPP
}
// dseats
IntegerVector dseats(NumericMatrix dcounts, NumericMatrix rcounts);
RcppExport SEXP _redistmetrics_dseats(SEXP dcountsSEXP, SEXP rcountsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dcounts(dcountsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type rcounts(rcountsSEXP);
    rcpp_result_gen = Rcpp::wrap(dseats(dcounts, rcounts));
    return rcpp_result_gen;
END_RCPP
}
// dseatsDVS
IntegerVector dseatsDVS(NumericMatrix dvs);
RcppExport SEXP _redistmetrics_dseatsDVS(SEXP dvsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    rcpp_result_gen = Rcpp::wrap(dseatsDVS(dvs));
    return rcpp_result_gen;
END_RCPP
}
// DVS
NumericMatrix DVS(NumericMatrix dcounts, NumericMatrix rcounts);
RcppExport SEXP _redistmetrics_DVS(SEXP dcountsSEXP, SEXP rcountsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dcounts(dcountsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type rcounts(rcountsSEXP);
    rcpp_result_gen = Rcpp::wrap(DVS(dcounts, rcounts));
    return rcpp_result_gen;
END_RCPP
}
// effgapEP
NumericVector effgapEP(NumericMatrix dvs, IntegerVector dseat_vec, int nd);
RcppExport SEXP _redistmetrics_effgapEP(SEXP dvsSEXP, SEXP dseat_vecSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dseat_vec(dseat_vecSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(effgapEP(dvs, dseat_vec, nd));
    return rcpp_result_gen;
END_RCPP
}
// effgap
NumericVector effgap(NumericMatrix dcounts, NumericMatrix rcounts, int totvote);
RcppExport SEXP _redistmetrics_effgap(SEXP dcountsSEXP, SEXP rcountsSEXP, SEXP totvoteSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dcounts(dcountsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type rcounts(rcountsSEXP);
    Rcpp::traits::input_parameter< int >::type totvote(totvoteSEXP);
    rcpp_result_gen = Rcpp::wrap(effgap(dcounts, rcounts, totvote));
    return rcpp_result_gen;
END_RCPP
}
// taugap
NumericVector taugap(double tau, NumericMatrix dvs, IntegerVector dseat_vec, int nd);
RcppExport SEXP _redistmetrics_taugap(SEXP tauSEXP, SEXP dvsSEXP, SEXP dseat_vecSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dseat_vec(dseat_vecSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(taugap(tau, dvs, dseat_vec, nd));
    return rcpp_result_gen;
END_RCPP
}
// meanmedian
NumericVector meanmedian(NumericMatrix dvs);
RcppExport SEXP _redistmetrics_meanmedian(SEXP dvsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    rcpp_result_gen = Rcpp::wrap(meanmedian(dvs));
    return rcpp_result_gen;
END_RCPP
}
// declination_simple
NumericVector declination_simple(NumericMatrix dvs, IntegerVector dseat_vec, int nd);
RcppExport SEXP _redistmetrics_declination_simple(SEXP dvsSEXP, SEXP dseat_vecSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dseat_vec(dseat_vecSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(declination_simple(dvs, dseat_vec, nd));
    return rcpp_result_gen;
END_RCPP
}
// declination_angle
NumericVector declination_angle(NumericMatrix dvs, IntegerVector dseat_vec, int nd);
RcppExport SEXP _redistmetrics_declination_angle(SEXP dvsSEXP, SEXP dseat_vecSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dseat_vec(dseat_vecSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(declination_angle(dvs, dseat_vec, nd));
    return rcpp_result_gen;
END_RCPP
}
// lopsidedwins
NumericVector lopsidedwins(NumericMatrix dvs, IntegerVector dseat_vec, int nd);
RcppExport SEXP _redistmetrics_lopsidedwins(SEXP dvsSEXP, SEXP dseat_vecSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dseat_vec(dseat_vecSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(lopsidedwins(dvs, dseat_vec, nd));
    return rcpp_result_gen;
END_RCPP
}
// responsiveness
NumericVector responsiveness(NumericMatrix dvs, double v, int nd, double bandwidth);
RcppExport SEXP _redistmetrics_responsiveness(SEXP dvsSEXP, SEXP vSEXP, SEXP ndSEXP, SEXP bandwidthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    Rcpp::traits::input_parameter< double >::type v(vSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    Rcpp::traits::input_parameter< double >::type bandwidth(bandwidthSEXP);
    rcpp_result_gen = Rcpp::wrap(responsiveness(dvs, v, nd, bandwidth));
    return rcpp_result_gen;
END_RCPP
}
// biasatv
NumericVector biasatv(NumericMatrix dvs, double v, int nd);
RcppExport SEXP _redistmetrics_biasatv(SEXP dvsSEXP, SEXP vSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    Rcpp::traits::input_parameter< double >::type v(vSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(biasatv(dvs, v, nd));
    return rcpp_result_gen;
END_RCPP
}
// RankedMarginalDev
NumericVector RankedMarginalDev(NumericMatrix dvs);
RcppExport SEXP _redistmetrics_RankedMarginalDev(SEXP dvsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    rcpp_result_gen = Rcpp::wrap(RankedMarginalDev(dvs));
    return rcpp_result_gen;
END_RCPP
}
// smoothseat
NumericVector smoothseat(NumericMatrix dvs, int nd);
RcppExport SEXP _redistmetrics_smoothseat(SEXP dvsSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dvs(dvsSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(smoothseat(dvs, nd));
    return rcpp_result_gen;
END_RCPP
}
// reindex
IntegerMatrix reindex(IntegerMatrix dm, int nd);
RcppExport SEXP _redistmetrics_reindex(SEXP dmSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type dm(dmSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(reindex(dm, nd));
    return rcpp_result_gen;
END_RCPP
}
// segregationcalc
arma::vec segregationcalc(const arma::umat distmat, const arma::vec grouppop, const arma::vec fullpop);
RcppExport SEXP _redistmetrics_segregationcalc(SEXP distmatSEXP, SEXP grouppopSEXP, SEXP fullpopSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::umat >::type distmat(distmatSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type grouppop(grouppopSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type fullpop(fullpopSEXP);
    rcpp_result_gen = Rcpp::wrap(segregationcalc(distmat, grouppop, fullpop));
    return rcpp_result_gen;
END_RCPP
}
// splits
IntegerVector splits(const IntegerMatrix& dm, const IntegerVector& community, int nd, int max_split, bool skip_last);
static SEXP _redistmetrics_splits_try(SEXP dmSEXP, SEXP communitySEXP, SEXP ndSEXP, SEXP max_splitSEXP, SEXP skip_lastSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix& >::type dm(dmSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type community(communitySEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    Rcpp::traits::input_parameter< int >::type max_split(max_splitSEXP);
    Rcpp::traits::input_parameter< bool >::type skip_last(skip_lastSEXP);
    rcpp_result_gen = Rcpp::wrap(splits(dm, community, nd, max_split, skip_last));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _redistmetrics_splits(SEXP dmSEXP, SEXP communitySEXP, SEXP ndSEXP, SEXP max_splitSEXP, SEXP skip_lastSEXP) {
    SEXP rcpp_result_gen;
    {
        rcpp_result_gen = PROTECT(_redistmetrics_splits_try(dmSEXP, communitySEXP, ndSEXP, max_splitSEXP, skip_lastSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// distr_cty_splits
IntegerMatrix distr_cty_splits(IntegerMatrix dm, IntegerVector community, int nd);
static SEXP _redistmetrics_distr_cty_splits_try(SEXP dmSEXP, SEXP communitySEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type dm(dmSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type community(communitySEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(distr_cty_splits(dm, community, nd));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _redistmetrics_distr_cty_splits(SEXP dmSEXP, SEXP communitySEXP, SEXP ndSEXP) {
    SEXP rcpp_result_gen;
    {
        rcpp_result_gen = PROTECT(_redistmetrics_distr_cty_splits_try(dmSEXP, communitySEXP, ndSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// admin_splits_count
IntegerMatrix admin_splits_count(const IntegerMatrix& dm, const IntegerVector& admin, int nd, int nc);
static SEXP _redistmetrics_admin_splits_count_try(SEXP dmSEXP, SEXP adminSEXP, SEXP ndSEXP, SEXP ncSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix& >::type dm(dmSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type admin(adminSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    Rcpp::traits::input_parameter< int >::type nc(ncSEXP);
    rcpp_result_gen = Rcpp::wrap(admin_splits_count(dm, admin, nd, nc));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _redistmetrics_admin_splits_count(SEXP dmSEXP, SEXP adminSEXP, SEXP ndSEXP, SEXP ncSEXP) {
    SEXP rcpp_result_gen;
    {
        rcpp_result_gen = PROTECT(_redistmetrics_admin_splits_count_try(dmSEXP, adminSEXP, ndSEXP, ncSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// tally_var
NumericMatrix tally_var(IntegerMatrix dm, NumericVector var, int nd);
RcppExport SEXP _redistmetrics_tally_var(SEXP dmSEXP, SEXP varSEXP, SEXP ndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type dm(dmSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type var(varSEXP);
    Rcpp::traits::input_parameter< int >::type nd(ndSEXP);
    rcpp_result_gen = Rcpp::wrap(tally_var(dm, var, nd));
    return rcpp_result_gen;
END_RCPP
}
// var_info_mat
arma::mat var_info_mat(const arma::umat m, const arma::vec pop, int ndists, int ncores);
static SEXP _redistmetrics_var_info_mat_try(SEXP mSEXP, SEXP popSEXP, SEXP ndistsSEXP, SEXP ncoresSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::umat >::type m(mSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type pop(popSEXP);
    Rcpp::traits::input_parameter< int >::type ndists(ndistsSEXP);
    Rcpp::traits::input_parameter< int >::type ncores(ncoresSEXP);
    rcpp_result_gen = Rcpp::wrap(var_info_mat(m, pop, ndists, ncores));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _redistmetrics_var_info_mat(SEXP mSEXP, SEXP popSEXP, SEXP ndistsSEXP, SEXP ncoresSEXP) {
    SEXP rcpp_result_gen;
    {
        rcpp_result_gen = PROTECT(_redistmetrics_var_info_mat_try(mSEXP, popSEXP, ndistsSEXP, ncoresSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _redistmetrics_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("NumericVector(*log_st_map)(const Graph&,const arma::umat&,const arma::uvec&,int)");
        signatures.insert("NumericVector(*n_removed)(const Graph&,const arma::umat&,int)");
        signatures.insert("IntegerVector(*splits)(const IntegerMatrix&,const IntegerVector&,int,int,bool)");
        signatures.insert("IntegerMatrix(*distr_cty_splits)(IntegerMatrix,IntegerVector,int)");
        signatures.insert("IntegerMatrix(*admin_splits_count)(const IntegerMatrix&,const IntegerVector&,int,int)");
        signatures.insert("arma::mat(*var_info_mat)(const arma::umat,const arma::vec,int,int)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _redistmetrics_RcppExport_registerCCallable() { 
    R_RegisterCCallable("redistmetrics", "_redistmetrics_log_st_map", (DL_FUNC)_redistmetrics_log_st_map_try);
    R_RegisterCCallable("redistmetrics", "_redistmetrics_n_removed", (DL_FUNC)_redistmetrics_n_removed_try);
    R_RegisterCCallable("redistmetrics", "_redistmetrics_splits", (DL_FUNC)_redistmetrics_splits_try);
    R_RegisterCCallable("redistmetrics", "_redistmetrics_distr_cty_splits", (DL_FUNC)_redistmetrics_distr_cty_splits_try);
    R_RegisterCCallable("redistmetrics", "_redistmetrics_admin_splits_count", (DL_FUNC)_redistmetrics_admin_splits_count_try);
    R_RegisterCCallable("redistmetrics", "_redistmetrics_var_info_mat", (DL_FUNC)_redistmetrics_var_info_mat_try);
    R_RegisterCCallable("redistmetrics", "_redistmetrics_RcppExport_validate", (DL_FUNC)_redistmetrics_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_redistmetrics_polsbypopper", (DL_FUNC) &_redistmetrics_polsbypopper, 6},
    {"_redistmetrics_schwartzberg", (DL_FUNC) &_redistmetrics_schwartzberg, 6},
    {"_redistmetrics_talisman", (DL_FUNC) &_redistmetrics_talisman, 4},
    {"_redistmetrics_contiguity", (DL_FUNC) &_redistmetrics_contiguity, 2},
    {"_redistmetrics_hamming", (DL_FUNC) &_redistmetrics_hamming, 2},
    {"_redistmetrics_minkowski", (DL_FUNC) &_redistmetrics_minkowski, 3},
    {"_redistmetrics_log_st_map", (DL_FUNC) &_redistmetrics_log_st_map, 4},
    {"_redistmetrics_n_removed", (DL_FUNC) &_redistmetrics_n_removed, 3},
    {"_redistmetrics_agg_p2d", (DL_FUNC) &_redistmetrics_agg_p2d, 3},
    {"_redistmetrics_dseats", (DL_FUNC) &_redistmetrics_dseats, 2},
    {"_redistmetrics_dseatsDVS", (DL_FUNC) &_redistmetrics_dseatsDVS, 1},
    {"_redistmetrics_DVS", (DL_FUNC) &_redistmetrics_DVS, 2},
    {"_redistmetrics_effgapEP", (DL_FUNC) &_redistmetrics_effgapEP, 3},
    {"_redistmetrics_effgap", (DL_FUNC) &_redistmetrics_effgap, 3},
    {"_redistmetrics_taugap", (DL_FUNC) &_redistmetrics_taugap, 4},
    {"_redistmetrics_meanmedian", (DL_FUNC) &_redistmetrics_meanmedian, 1},
    {"_redistmetrics_declination_simple", (DL_FUNC) &_redistmetrics_declination_simple, 3},
    {"_redistmetrics_declination_angle", (DL_FUNC) &_redistmetrics_declination_angle, 3},
    {"_redistmetrics_lopsidedwins", (DL_FUNC) &_redistmetrics_lopsidedwins, 3},
    {"_redistmetrics_responsiveness", (DL_FUNC) &_redistmetrics_responsiveness, 4},
    {"_redistmetrics_biasatv", (DL_FUNC) &_redistmetrics_biasatv, 3},
    {"_redistmetrics_RankedMarginalDev", (DL_FUNC) &_redistmetrics_RankedMarginalDev, 1},
    {"_redistmetrics_smoothseat", (DL_FUNC) &_redistmetrics_smoothseat, 2},
    {"_redistmetrics_reindex", (DL_FUNC) &_redistmetrics_reindex, 2},
    {"_redistmetrics_segregationcalc", (DL_FUNC) &_redistmetrics_segregationcalc, 3},
    {"_redistmetrics_splits", (DL_FUNC) &_redistmetrics_splits, 5},
    {"_redistmetrics_distr_cty_splits", (DL_FUNC) &_redistmetrics_distr_cty_splits, 3},
    {"_redistmetrics_admin_splits_count", (DL_FUNC) &_redistmetrics_admin_splits_count, 4},
    {"_redistmetrics_tally_var", (DL_FUNC) &_redistmetrics_tally_var, 3},
    {"_redistmetrics_var_info_mat", (DL_FUNC) &_redistmetrics_var_info_mat, 4},
    {"_redistmetrics_RcppExport_registerCCallable", (DL_FUNC) &_redistmetrics_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_redistmetrics(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
