#include <Rcpp.h>
#include <cmath>
#include <queue>
#include <set>
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
            int lower_distr = lower[v] - 1;
            int upper_distr = plans(v, p) - 1;
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
            int distr_assigned = plans(k, p) - 1;
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

// Calculate matrix of school outside zone counts for each plan
// [[Rcpp::export(rng = false)]]
NumericMatrix schooloutsidezone(const IntegerMatrix& plans,
                       const IntegerVector& schools_idx, // 0-based
                       const int ndists) {
    const int n_plans   = plans.ncol();
    const int n_schools = schools_idx.size();
    
    // create empty output matrix
    NumericMatrix out(ndists, n_plans);
    
    for (int p = 0; p < n_plans; ++p) {
        // calculate the number of districts with no school within their zone
        IntegerVector col = plans(_, p);
        IntegerVector vals = col[schools_idx];
        int count = n_schools - unique(vals).size();
        // int count = 0;
        // for (int d = 0; d < ndists; d++) {
        //     // Count how many schools are assigned outside their own district
        //     if (plans(schools_idx[d], p) != d + 1) {
        //         count++;
        //     }
        // }
        for (int d = 0; d < ndists; d++) {
            out(d, p) = count;
        }
    }
    
    return out;
}

// Calculate matrix of connected components in each plan
// [[Rcpp::export(rng = false)]]
NumericMatrix attendanceisland(const IntegerMatrix& plans,
                               const Rcpp::List& adjacency,
                               const int ndists) {
    const int n_units = plans.nrow();
    const int n_plans = plans.ncol();
    
    // create empty output matrix
    NumericMatrix out(ndists, n_plans);
    
    for (int p = 0; p < n_plans; ++p) {
        int island_count = 0;

        // For each district, count connected components
        for (int d = 0; d < ndists; ++d) {
            // Get all units in district d (1-indexed in plans)
            std::vector<int> units_in_district;
            for (int k = 0; k < n_units; ++k) {
                if (plans(k, p) == d + 1) {  // d+1 because districts are 1-indexed
                    units_in_district.push_back(k);
                }
            }
            
            // Track visited units
            std::set<int> units_set(units_in_district.begin(), units_in_district.end());
            std::set<int> visited;
            int components = 0;
            
            // BFS to count connected components
            for (int start : units_in_district) {
                if (visited.find(start) != visited.end()) continue;
                
                components++;
                std::queue<int> q;
                q.push(start);
                visited.insert(start);
                
                while (!q.empty()) {
                    int u = q.front();
                    q.pop();
                    
                    // Get neighbors of u
                    IntegerVector neighbors = adjacency[u];
                    for (int i = 0; i < neighbors.length(); ++i) {
                        int v = neighbors[i];
                        
                        // If v is in same district and not visited, explore it
                        if (units_set.find(v) != units_set.end() && 
                            visited.find(v) == visited.end()) {
                            visited.insert(v);
                            q.push(v);
                        }
                    }
                }
            }
            
            island_count += components - 1; // add number of islands
        }

        for (int d = 0; d < ndists; d++) {
            out(d, p) = island_count;
        }
    }
    
    return out;
}
