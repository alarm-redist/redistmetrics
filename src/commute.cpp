#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Return first index j with school_distr[j] == target; -1 if none
inline int first_match(const IntegerVector& school_distr, int n_schools, int target) {
    for (int j = 0; j < n_schools; j++) {
        if (school_distr[j] == target) return j;
    }
    return -1;
}

// Calculate matrix of phase commute scores
// [[Rcpp::export(rng = false)]]
NumericMatrix phasecommute(const IntegerMatrix& plans,
                                const IntegerVector& current,
                                const NumericVector& pop,
                                const IntegerVector& schools, // 1-based
                                const NumericMatrix& commute_times,  // n_units x n_schools
                                const int ndists) {
    const int n_units   = plans.nrow();
    const int n_plans   = plans.ncol();
    const int n_schools = schools.size();
    
    // 1-based -> 0-based copy
    IntegerVector schools_idx(n_schools);
    for (int j = 0; j < n_schools; j++) {
        int idx = schools[j];
        if (idx < 1 || idx > n_units)
            stop("schools_idx out of range.");
        schools_idx[j] = idx - 1;
    }
    
    // schools' districts under current plan
    IntegerVector schools_current_distr(n_schools);
    for (int j = 0; j < n_schools; j++) {
        schools_current_distr[j] = current[schools_idx[j]];
    }
    
    // create empty output matrix
    NumericMatrix out(ndists, n_plans);
    
    for (int p = 0; p < n_plans; ++p) {
        // schools' districts under proposed plan p
        IntegerVector schools_new_distr(n_schools);
        for (int j = 0; j < n_schools; j++) {
            schools_new_distr[j] = plans(schools_idx[j], p);
        }
        
        std::vector<double> reassigned_extra(ndists, 0.0);
        std::vector<double> distr_pop(ndists, 0.0);
        
        // total population per district (denominator)
        for (int k = 0; k < n_units; ++k) {
            int distr_new = plans(k, p);
            // check that new district is valid and population is finite
            if (distr_new >= 1 && distr_new <= ndists && std::isfinite(pop[k])) {
                distr_pop[distr_new - 1] += pop[k];
            }
        }
        
        // numerator: add commute where the zoned school changes AND commute increases
        for (int k = 0; k < n_units; ++k) {
            int distr_new = plans(k, p);
            if (distr_new < 1 || distr_new > ndists) continue;
            
            // old/new zoned school indices (columns into commute_times)
            int j_old = first_match(schools_current_distr, n_schools, current[k]);
            if (j_old < 0 || j_old >= ndists) continue;
            
            int j_new = first_match(schools_new_distr, n_schools, distr_new);
            if (j_new < 0 || j_new >= ndists) continue;
            
            // skip if zoned school doesn't change
            if (j_old == j_new) continue;
            
            double c_old = commute_times(k, j_old);
            double c_new = commute_times(k, j_new);
            
            if (std::isfinite(c_old) && std::isfinite(c_new) && c_old < c_new && std::isfinite(pop[k]) && pop[k] > 0.0) {
                reassigned_extra[distr_new - 1] += pop[k] * (c_new - c_old);
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
                                const IntegerVector& schools, // 1-based
                                const NumericMatrix& commute_times,  // n_units x n_schools
                                const int ndists) {
    const int n_units   = plans.nrow();
    const int n_plans   = plans.ncol();
    const int n_schools = schools.size();
    
    // 1-based -> 0-based copy
    IntegerVector schools_idx(n_schools);
    for (int j = 0; j < n_schools; j++) {
        int idx = schools[j];
        if (idx < 1 || idx > n_units)
            stop("schools_idx out of range.");
        schools_idx[j] = idx - 1;
    }
    
    // create empty output matrix
    NumericMatrix out(ndists, n_plans);
    
    for (int p = 0; p < n_plans; ++p) {
        // schools' districts under proposed plan p
        IntegerVector schools_new_distr(n_schools);
        for (int j = 0; j < n_schools; j++) {
            schools_new_distr[j] = plans(schools_idx[j], p);
        }
        
        std::vector<double> max_time(ndists, 0.0);
        
        // find the maximum commute for any individual in each district
        for (int k = 0; k < n_units; ++k) {
            int distr_new = plans(k, p);
            if (distr_new < 1 || distr_new > ndists) continue;
            
            // new zoned school indices (columns into commute_times)
            int j_new = first_match(schools_new_distr, n_schools, distr_new);
            if (j_new < 0 || j_new >= ndists) continue;
            
            double c_new = commute_times(k, j_new);
            
            if (std::isfinite(c_new) && c_new > max_time[distr_new - 1]) {
                max_time[distr_new - 1] = c_new;
            }
        }
        
        // per-district score: log1p(max extra commute for a person in the district)
        for (int d = 0; d < ndists; d++) {
            out(d, p) = std::log1p(max_time[d]);
        }
    }
    
    return out;
}
