#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// Per-cell rebalancing kernel for base cells.
//
// Inputs are sorted by strID; group_starts holds the 0-based start index of
// each cell (same convention as check_sensitivity_cpp). For each cell the
// records are ordered by descending |orig * mult| (stable, so ties keep the
// input order) and processed by the greedy running-noise pass. Cells are
// independent of each other and are processed in parallel.
//
// Returns the rebalanced direction for every record in input order.
// [[Rcpp::export]]
NumericVector rebalance_cells_cpp(NumericVector orig,
                                  NumericVector mult,
                                  NumericVector dirs,
                                  LogicalVector is_sens_by_cell,
                                  IntegerVector group_starts,
                                  int n_threads) {
  const int n_total = orig.size();
  const int n_groups = group_starts.size();
  NumericVector dir_out(n_total);

#ifdef _OPENMP
  if (n_threads > 0) {
    omp_set_num_threads(n_threads);
  }
#endif

#pragma omp parallel for schedule(dynamic)
  for (int g = 0; g < n_groups; g++) {
    const int start = group_starts[g];
    const int end = (g == n_groups - 1) ? n_total : group_starts[g + 1];
    const int n_obs = end - start;
    if (n_obs <= 0) {
      continue;
    }
    const bool is_sensitive = is_sens_by_cell[g];

    std::vector<int> idx(n_obs);
    for (int i = 0; i < n_obs; i++) {
      idx[i] = i;
    }
    std::stable_sort(idx.begin(), idx.end(), [&](int a, int b) {
      const double ia = std::abs(orig[start + a] * mult[start + a]);
      const double ib = std::abs(orig[start + b] * mult[start + b]);
      return ia > ib;
    });

    double running_noise = 0.0;
    for (int k = 0; k < n_obs; k++) {
      const int i = start + idx[k];
      double d;
      if (k == 0) {
        // First record: apply perturbation with its fixed direction
        d = dirs[i];
      } else if (is_sensitive) {
        d = dirs[i];
      } else {
        const double delta = orig[i] * mult[i];
        d = (std::abs(running_noise + delta) <
             std::abs(running_noise - delta)) ? 1.0 : -1.0;
      }
      dir_out[i] = d;
      const double pert = orig[i] * (1.0 + d * mult[i]);
      running_noise += pert - orig[i];
    }
  }

  return dir_out;
}
