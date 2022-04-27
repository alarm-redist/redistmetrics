// [[Rcpp::interfaces(r, cpp)]]
#include <RcppArmadillo.h>
#include <RcppThread.h>
using namespace Rcpp;
using namespace arma;

double var_info(const uvec m1, const uvec m2, const vec pop, int k) {
    int V = m1.n_elem;
    mat joint(k, k);
    vec p1(k);
    vec p2(k);

    double total_pop = 0;
    for (int i = 0; i < V; i++) {
        joint(m1[i]-1, m2[i]-1) += pop[i];
        p1[m1[i]-1] += pop[i];
        p2[m2[i]-1] += pop[i];
        total_pop += pop[i];
    }

    double varinf = 0;
    for (int i = 0; i < k; i++) {
        for (int j = 0; j < k; j++) {
            double jo = joint(i, j);
            if (jo < 1) continue;
            varinf -= (jo / total_pop) * (2.0*std::log(jo) - std::log(p1[i]) - std::log(p2[j]));
        }
    }

    if (std::fabs(varinf) <= 1e-9)
        varinf = 0;
    return varinf;
}

/*
 * `m` has rows = precincts, cols = plans
 * `i` is the index of the plan we want to compute distances to
 * `pop` is population of precincts
 *
 * Only computes pairs (i, j) with j < i
 */
// [[Rcpp::export(rng = false)]]
arma::mat var_info_mat(const arma::umat m, const arma::vec pop, int ndists, int ncores) {
    int N = m.n_cols;

    mat out(N, N);
    RcppThread::parallelFor(1, N, [&] (int i) {
        auto plan_i = m.col(i);
        for (int j = 0; j < i; j++) {
            double vi = var_info(plan_i, m.col(j), pop, ndists);
            out(i, j) = vi;
            out(j, i) = vi;
        }
    }, ncores);

    return out;
}
