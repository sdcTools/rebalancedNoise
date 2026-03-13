#include <Rcpp.h>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector check_sensitivity_cpp(NumericVector vals,
                                             CharacterVector ids,
                                             IntegerVector group_starts,
                                             int n_threshold,
                                             double p_rule,
                                             int nk_n,
                                             double nk_k,
                                             int n_threads) {
  int n_groups = group_starts.size();
  int n_total = vals.size();
  LogicalVector is_sens(n_total);

  // Setze die Anzahl der Threads, falls OpenMP verfügbar ist
#ifdef _OPENMP
  if (n_threads > 0) {
    omp_set_num_threads(n_threads);
  }
#endif

#pragma omp parallel for schedule(dynamic)
  for (int g = 0; g < n_groups; g++) {
    int start = group_starts[g];
    int end = (g == n_groups - 1) ? n_total : group_starts[g + 1];

    double total = 0;
    for (int i = start; i < end; i++) {
      total += vals[i];
    }

    int n_obs = end - start;
    bool cell_is_sens = false;

    if (n_obs <= n_threshold) {
      cell_is_sens = true;
    }

    if (!cell_is_sens) {
      double x1 = vals[start];
      double x2 = (n_obs > 1) ? vals[start + 1] : 0.0;

      if (p_rule > 0 && (total - x1 - x2) < (p_rule / 100.0 * x1)) {
        cell_is_sens = true;
      }

      if (!cell_is_sens && nk_n > 0) {
        double top_n_sum = 0;
        int max_n = (n_obs < nk_n) ? n_obs : nk_n;
        for (int i = 0; i < max_n; i++) {
          top_n_sum += vals[start + i];
        }
        if ((top_n_sum / total) * 100.0 > nk_k) {
          cell_is_sens = true;
        }
      }
    }

    for (int i = start; i < end; i++) {
      is_sens[i] = cell_is_sens;
    }
  }

  return is_sens;
}
