// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_redistmetrics_RCPPEXPORTS_H_GEN_
#define RCPP_redistmetrics_RCPPEXPORTS_H_GEN_

#include "redistmetrics_types.h"
#include <RcppArmadillo.h>
#include <RcppThread.h>
#include <Rcpp.h>

namespace redistmetrics {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("redistmetrics", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("redistmetrics", "_redistmetrics_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in redistmetrics");
            }
        }
    }

    inline NumericVector log_st_map(const Graph& g, const arma::umat& districts, const arma::uvec& counties, int n_distr) {
        typedef SEXP(*Ptr_log_st_map)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_log_st_map p_log_st_map = NULL;
        if (p_log_st_map == NULL) {
            validateSignature("NumericVector(*log_st_map)(const Graph&,const arma::umat&,const arma::uvec&,int)");
            p_log_st_map = (Ptr_log_st_map)R_GetCCallable("redistmetrics", "_redistmetrics_log_st_map");
        }
        RObject rcpp_result_gen;
        {
            rcpp_result_gen = p_log_st_map(Shield<SEXP>(Rcpp::wrap(g)), Shield<SEXP>(Rcpp::wrap(districts)), Shield<SEXP>(Rcpp::wrap(counties)), Shield<SEXP>(Rcpp::wrap(n_distr)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

    inline NumericVector n_removed(const Graph& g, const arma::umat& districts, int n_distr) {
        typedef SEXP(*Ptr_n_removed)(SEXP,SEXP,SEXP);
        static Ptr_n_removed p_n_removed = NULL;
        if (p_n_removed == NULL) {
            validateSignature("NumericVector(*n_removed)(const Graph&,const arma::umat&,int)");
            p_n_removed = (Ptr_n_removed)R_GetCCallable("redistmetrics", "_redistmetrics_n_removed");
        }
        RObject rcpp_result_gen;
        {
            rcpp_result_gen = p_n_removed(Shield<SEXP>(Rcpp::wrap(g)), Shield<SEXP>(Rcpp::wrap(districts)), Shield<SEXP>(Rcpp::wrap(n_distr)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

    inline IntegerVector splits(const IntegerMatrix& dm, const IntegerVector& community, int nd, int max_split, bool skip_last = false) {
        typedef SEXP(*Ptr_splits)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_splits p_splits = NULL;
        if (p_splits == NULL) {
            validateSignature("IntegerVector(*splits)(const IntegerMatrix&,const IntegerVector&,int,int,bool)");
            p_splits = (Ptr_splits)R_GetCCallable("redistmetrics", "_redistmetrics_splits");
        }
        RObject rcpp_result_gen;
        {
            rcpp_result_gen = p_splits(Shield<SEXP>(Rcpp::wrap(dm)), Shield<SEXP>(Rcpp::wrap(community)), Shield<SEXP>(Rcpp::wrap(nd)), Shield<SEXP>(Rcpp::wrap(max_split)), Shield<SEXP>(Rcpp::wrap(skip_last)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<IntegerVector >(rcpp_result_gen);
    }

    inline IntegerMatrix distr_cty_splits(IntegerMatrix dm, IntegerVector community, int nd) {
        typedef SEXP(*Ptr_distr_cty_splits)(SEXP,SEXP,SEXP);
        static Ptr_distr_cty_splits p_distr_cty_splits = NULL;
        if (p_distr_cty_splits == NULL) {
            validateSignature("IntegerMatrix(*distr_cty_splits)(IntegerMatrix,IntegerVector,int)");
            p_distr_cty_splits = (Ptr_distr_cty_splits)R_GetCCallable("redistmetrics", "_redistmetrics_distr_cty_splits");
        }
        RObject rcpp_result_gen;
        {
            rcpp_result_gen = p_distr_cty_splits(Shield<SEXP>(Rcpp::wrap(dm)), Shield<SEXP>(Rcpp::wrap(community)), Shield<SEXP>(Rcpp::wrap(nd)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<IntegerMatrix >(rcpp_result_gen);
    }

    inline IntegerMatrix admin_splits_count(const IntegerMatrix& dm, const IntegerVector& admin, int nd, int nc) {
        typedef SEXP(*Ptr_admin_splits_count)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_admin_splits_count p_admin_splits_count = NULL;
        if (p_admin_splits_count == NULL) {
            validateSignature("IntegerMatrix(*admin_splits_count)(const IntegerMatrix&,const IntegerVector&,int,int)");
            p_admin_splits_count = (Ptr_admin_splits_count)R_GetCCallable("redistmetrics", "_redistmetrics_admin_splits_count");
        }
        RObject rcpp_result_gen;
        {
            rcpp_result_gen = p_admin_splits_count(Shield<SEXP>(Rcpp::wrap(dm)), Shield<SEXP>(Rcpp::wrap(admin)), Shield<SEXP>(Rcpp::wrap(nd)), Shield<SEXP>(Rcpp::wrap(nc)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<IntegerMatrix >(rcpp_result_gen);
    }

    inline arma::mat var_info_mat(const arma::umat m, const arma::vec pop, int ndists, int ncores) {
        typedef SEXP(*Ptr_var_info_mat)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_var_info_mat p_var_info_mat = NULL;
        if (p_var_info_mat == NULL) {
            validateSignature("arma::mat(*var_info_mat)(const arma::umat,const arma::vec,int,int)");
            p_var_info_mat = (Ptr_var_info_mat)R_GetCCallable("redistmetrics", "_redistmetrics_var_info_mat");
        }
        RObject rcpp_result_gen;
        {
            rcpp_result_gen = p_var_info_mat(Shield<SEXP>(Rcpp::wrap(m)), Shield<SEXP>(Rcpp::wrap(pop)), Shield<SEXP>(Rcpp::wrap(ndists)), Shield<SEXP>(Rcpp::wrap(ncores)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::mat >(rcpp_result_gen);
    }

}

#endif // RCPP_redistmetrics_RCPPEXPORTS_H_GEN_
