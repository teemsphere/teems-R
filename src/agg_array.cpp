#include <cpp11.hpp>
#include <vector>
#include <cstring>
#include <cstdint>

// Scatter-add aggregation of a dense column-major array.
//
// x:         numeric array data (double or integer SEXP, read in place —
//            no coercion copy), column-major
// codes:     list of integer vectors, one per source dimension;
//            codes[[k]][j] = 0-based aggregated bin of level j on dim k
// out_sizes: number of aggregated bins per dimension
//
// Returns the dense aggregated array (column-major, prod(out_sizes) cells).
// Each source cell adds into out[g] where g is assembled from the per-dim
// bin offsets, tracked incrementally with a mixed-radix counter so the
// pass is a single sweep with no divisions.
[[cpp11::register]]
cpp11::doubles agg_array_sum(cpp11::sexp x, cpp11::list codes,
                             cpp11::integers out_sizes) {
  if (TYPEOF(x) != REALSXP && TYPEOF(x) != INTSXP) {
    cpp11::stop("agg_array_sum: numeric input required");
  }
  const int ndim = codes.size();
  std::vector<cpp11::integers> cd;
  cd.reserve(ndim);
  std::vector<R_xlen_t> dsize(ndim);
  for (int k = 0; k < ndim; ++k) {
    cd.push_back(cpp11::integers(codes[k]));
    dsize[k] = cd[k].size();
  }

  std::vector<R_xlen_t> ostride(ndim);
  R_xlen_t total = 1;
  for (int k = 0; k < ndim; ++k) {
    ostride[k] = total;
    total *= out_sizes[k];
  }

  // off[k][j] = codes[k][j] * ostride[k]
  std::vector<std::vector<R_xlen_t>> off(ndim);
  for (int k = 0; k < ndim; ++k) {
    off[k].resize(dsize[k]);
    for (R_xlen_t j = 0; j < dsize[k]; ++j) {
      off[k][j] = static_cast<R_xlen_t>(cd[k][j]) * ostride[k];
    }
  }

  cpp11::writable::doubles out(total);
  double* po = REAL(out);
  std::memset(po, 0, sizeof(double) * total);

  const R_xlen_t n = Rf_xlength(x);

  std::vector<R_xlen_t> cnt(ndim, 0);
  R_xlen_t g = 0;
  for (int k = 0; k < ndim; ++k) g += off[k][0];

  const auto step = [&](R_xlen_t& gg) {
    int k = 0;
    while (k < ndim) {
      gg -= off[k][cnt[k]];
      if (++cnt[k] < dsize[k]) {
        gg += off[k][cnt[k]];
        break;
      }
      cnt[k] = 0;
      gg += off[k][0];
      ++k;
    }
  };

  if (TYPEOF(x) == REALSXP) {
    const double* px = REAL(x);
    for (R_xlen_t i = 0; i < n; ++i) {
      po[g] += px[i];
      step(g);
    }
  } else {
    const int* px = INTEGER(x);
    for (R_xlen_t i = 0; i < n; ++i) {
      po[g] += (px[i] == NA_INTEGER) ? NA_REAL : static_cast<double>(px[i]);
      step(g);
    }
  }
  return out;
}
