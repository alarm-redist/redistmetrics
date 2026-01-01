#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Calculate matrix of phase commute scores
// [[Rcpp::export(rng = false)]]
NumericMatrix phasecommute(const IntegerMatrix& plans,
                           const IntegerVector& current,
                           const NumericVector& pop,
                           const NumericMatrix& commute_times,  // n_units x n_schools
                           const int ndists) {
    const int n_units   = plans.nrow();
    const int n_plans   = plans.ncol();
    
    // create empty output matrix
    NumericMatrix out(ndists, n_plans);
    
    for (int p = 0; p < n_plans; ++p) {
        std::vector<double> reassigned_extra(ndists, 0.0);
        std::vector<double> distr_pop(ndists, 0.0);
        
        for (int k = 0; k < n_units; ++k) {
            int distr_new = plans(k, p) - 1;

            // denominator: total population in each new district
            if (distr_new >= 0 && distr_new < ndists && std::isfinite(pop[k])) {
                distr_pop[distr_new] += pop[k];
            }

            // numerator: add commute where the zoned school changes
            if (current[k] - 1 == distr_new) continue;
            
            double commute_old = commute_times(k, current[k] - 1);
            double commute_new = commute_times(k, distr_new);
            
            if (std::isfinite(commute_old) && std::isfinite(commute_new) && commute_old < commute_new && std::isfinite(pop[k]) && pop[k] > 0.0) {
                reassigned_extra[distr_new] += pop[k] * (commute_new - commute_old);
            }
        }
        
        // per-district score: log1p(average extra commute per person)
        for (int d = 0; d < ndists; d++) {
            double denom = distr_pop[d];
            double avg_extra = (denom > 0.0) ? (reassigned_extra[d] / denom) : 0.0;
            out(d, p) = std::log1p(avg_extra);
        }
    }
    
    return out;
}

// Calculate matrix of max commute scores
// [[Rcpp::export(rng = false)]]
NumericMatrix maxcommute(const IntegerMatrix& plans,
                         NumericMatrix& commute_times,  // n_units x n_schools
                         const int ndists) {
    const int n_units   = plans.nrow();
    const int n_plans   = plans.ncol();
    
    // create empty output matrix
    NumericMatrix out(ndists, n_plans);
    
    for (int p = 0; p < n_plans; ++p) {
        std::vector<double> max_time(ndists, 0.0);
        
        // find the maximum commute for any individual in each new district
        for (int k = 0; k < n_units; ++k) {
            int distr_new = plans(k, p) - 1;
            double commute_new = commute_times(k, distr_new);
            if (std::isfinite(commute_new) && commute_new > max_time[distr_new]) {
                max_time[distr_new] = commute_new;
            }
        }
        
        // per-district score: log1p(max commute for a person in the district)
        for (int d = 0; d < ndists; d++) {
            out(d, p) = std::log1p(max_time[d]);
        }
    }
    
    return out;
}
