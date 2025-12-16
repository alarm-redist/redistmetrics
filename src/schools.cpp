#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Calculate matrix of split feeder scores
// [[Rcpp::export(rng = false)]]
NumericMatrix splitfeeders(const IntegerMatrix& plans,
                           const IntegerVector& lower,
                           const IntegerVector& pop,
                           const int ndists) {
    const int n_units   = plans.nrow();
    const int n_plans   = plans.ncol();
    const int n_lower   = unique(lower).size();
    
    // create empty output matrix
    NumericMatrix out(ndists, n_plans);
    
    for (int p = 0; p < n_plans; ++p) {
        // create empty lower to upper district matrix
        NumericMatrix lower_to_upper(n_lower, ndists);

        // For each lower level district and upper level district, store how many students went from lower to upper
        for (int v = 0; v < n_units; ++v) {
            int lower_distr = lower[v];
            int upper_distr = plans(v, p);
            lower_to_upper(lower_distr, upper_distr) += pop[v];
        }

        // Count how many lower level districts are sending less than 25% of their students to each upper level district
        for (int d = 0; d < n_lower; ++d) {
            for (int u = 0; u < ndists; ++u) {
                double frac = lower_to_upper(d, u) / sum(lower_to_upper(d, _));
                if (frac < 0.25) {
                    out(u, p) += 1;
                }
            }
        }
    }
    
    return out;
}

// Calculate matrix of capacity utilization percentages
// [[Rcpp::export(rng = false)]]
NumericMatrix capacityutil(const IntegerMatrix& plans,
                       const IntegerVector& schools_capacity, // same order as schools_idx
                       const IntegerVector& pop,
                       const int ndists) {
    const int n_units   = plans.nrow();
    const int n_plans   = plans.ncol();
    
    // create empty output matrix
    NumericMatrix out(ndists, n_plans);
    
    for (int p = 0; p < n_plans; ++p) {
        // Store how many people are assigned to each district
        std::vector<double> distr_pop(ndists, 0.0);
        for (int k = 0; k < n_units; ++k) {
            int distr_assigned = plans(k, p);
            distr_pop[distr_assigned] += pop[k];
        }

        // Compute capacity utilization percentage for each district
        for (int d = 0; d < ndists; d++) {
            // estimate population of school aged children assigned to district d
            double normalized_pop = distr_pop[d] / sum(pop) * sum(schools_capacity);
            double ratio = normalized_pop / schools_capacity[d];
            out(d, p) = ratio;
        }
    }
    
    return out;
}

// Calculate matrix of school outside zone counts
// [[Rcpp::export(rng = false)]]
NumericMatrix schooloutsidezone(const IntegerMatrix& plans,
                       const IntegerVector& schools_idx, // 0-based
                       const int ndists) {
    const int n_plans   = plans.ncol();
    
    // create empty output matrix
    NumericMatrix out(ndists, n_plans);
    
    for (int p = 0; p < n_plans; ++p) {
        int count = 0;
        for (int d = 0; d < ndists; d++) {
            // Ensure that the school assigned to district d is actually in district d
            // TODO: verify that district i always corresponds to school i
            // Otherwise, just count how many districts have more than 1 school within its boundaries
            if (plans(schools_idx[d], p) != d) {
                count++;
            }
            out(d, p) = count;
        }
    }
    
    return out;
}
