#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Calculate matrix of split feeder scores
// [[Rcpp::export(rng = false)]]
NumericMatrix splitfeeders(const IntegerMatrix& plans,
                           const IntegerVector& lower,
                           const IntegerVector& schools_idx, // 0-based
                           const IntegerVector& pop,
                           const int ndists) {
    const int n_units   = plans.nrow();
    const int n_plans   = plans.ncol();
    const int n_schools = schools_idx.size();
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

// Calculate matrix of capacity utilization scores
// [[Rcpp::export(rng = false)]]
NumericMatrix capacityutil(const IntegerMatrix& plans,
                       const IntegerVector& schools_idx, // 0-based
                       const IntegerVector& schools_capacity, // same order as schools_idx
                       const IntegerVector& pop,
                       const int ndists) {
    const int n_units   = plans.nrow();
    const int n_plans   = plans.ncol();
    const int n_schools = schools_idx.size();
    
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
            double ratio = distr_pop[d] / schools_capacity[d];
            if (ratio < 0.85 || ratio > 1.15) {
                out(d, p) = 20;
            }
            else if ((0.85 <= ratio && ratio <= 0.94) || 1.05 <= ratio <= 1.14) {
                out(d, p) = 10;
            }
            else if (0.95 <= ratio && ratio <= 1.04) {
                out(d, p) = 0;
            }
        }
    }
    
    return out;
}
