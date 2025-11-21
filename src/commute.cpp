#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Calculate matrix of phase commute scores
// [[Rcpp::export(rng = false)]]
NumericMatrix phasecommute(const IntegerMatrix& plans,
                           const IntegerVector& current,
                           const NumericVector& pop,
                           const IntegerVector& schools_idx, // 0-based
                           const NumericMatrix& commute_times,  // n_units x n_schools
                           const int ndists) {
    const int n_units   = plans.nrow();
    const int n_plans   = plans.ncol();
    const int n_schools = schools_idx.size();
    
    // schools' districts under current plan
    IntegerVector schools_current_distr(n_schools);
    for (int j = 0; j < n_schools; j++) {
        int school_row = schools_idx[j];  // jth school's row number in map
        schools_current_distr[j] = current[school_row];  // current district ID of jth school
    }
    
    // create empty output matrix
    NumericMatrix out(ndists, n_plans);
    
    for (int p = 0; p < n_plans; ++p) {
        // schools' districts under proposed plan p
        IntegerVector schools_new_distr(n_schools);
        for (int j = 0; j < n_schools; j++) {
            int school_row = schools_idx[j];
            schools_new_distr[j] = plans(school_row, p);
        }
        
        std::vector<double> reassigned_extra(ndists, 0.0);
        std::vector<double> distr_pop(ndists, 0.0);
        
        for (int k = 0; k < n_units; ++k) {
            int distr_new = plans(k, p);

            // denominator: total population in each new district
            if (distr_new >= 1 && distr_new <= ndists && std::isfinite(pop[k])) {
                distr_pop[distr_new - 1] += pop[k];
            }

            // numerator: add commute where the zoned school changes
            if (current[k] == distr_new) continue;
            
            double commute_old = commute_times(k, current[k] - 1);
            double commute_new = commute_times(k, distr_new - 1);
            
            if (std::isfinite(commute_old) && std::isfinite(commute_new) && commute_old < commute_new && std::isfinite(pop[k]) && pop[k] > 0.0) {
                reassigned_extra[distr_new - 1] += pop[k] * (commute_new - commute_old);
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
                         const IntegerVector& schools_idx, // 0-based
                         NumericMatrix& commute_times,  // n_units x n_schools
                         const int ndists) {
    const int n_units   = plans.nrow();
    const int n_plans   = plans.ncol();
    const int n_schools = schools_idx.size();
    
    // create empty output matrix
    NumericMatrix out(ndists, n_plans);
    
    for (int p = 0; p < n_plans; ++p) {
        // schools' districts under proposed plan p
        IntegerVector schools_new_distr(n_schools);
        for (int j = 0; j < n_schools; j++) {
            int school_row = schools_idx[j];
            schools_new_distr[j] = plans(school_row, p);
        }
        
        std::vector<double> max_time(ndists, 0.0);
        
        // find the maximum commute for any individual in each new district
        for (int k = 0; k < n_units; ++k) {
            int distr_new = plans(k, p);
            double commute_new = commute_times(k, distr_new - 1);
            if (std::isfinite(commute_new) && commute_new > max_time[distr_new - 1]) {
                max_time[distr_new - 1] = commute_new;
            }
        }
        
        // per-district score: log1p(max extra commute for a person in the district)
        for (int d = 0; d < ndists; d++) {
            out(d, p) = std::log1p(max_time[d]);
        }
    }
    
    return out;
}
