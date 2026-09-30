#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

// isomorphism class of the triad (p0, p1, p2), looked up by its 6-bit arc code
// and the attributes of the three positions
static inline int triad_class(const std::vector<std::vector<int> >& out,
                              const IntegerVector& attr,
                              const IntegerVector& lookup,
                              int k, int p0, int p1, int p2) {
  auto arc = [&](int a, int b) {
    return std::binary_search(out[a].begin(), out[a].end(), b) ? 1 : 0;
  };
  int code = arc(p0, p1) + 2 * arc(p0, p2) + 4 * arc(p1, p0) +
             8 * arc(p1, p2) + 16 * arc(p2, p0) + 32 * arc(p2, p1);
  return lookup[((code * k + attr[p0]) * k + attr[p1]) * k + attr[p2]];
}

// Colored triad census following Batagelj & Mrvar (2001): connected triads are
// enumerated from each connected pair, dyadic triads are counted per color of
// the isolated vertex. Empty (003) triads are derived in R from the totals.
// outList: sorted out-neighbors (0-based), nbList: sorted undirected neighbors,
// attr: 0-based colors, lookup: class id for (code, attr_p0, attr_p1, attr_p2)
// [[Rcpp::export]]
NumericVector triadCensusCol(List outList, List nbList, IntegerVector attr,
                             int k, IntegerVector lookup, int nclass) {
  int n = attr.size();
  std::vector<std::vector<int> > out(n), nb(n);
  for (int i = 0; i < n; ++i) {
    out[i] = as<std::vector<int> >(outList[i]);
    nb[i] = as<std::vector<int> >(nbList[i]);
  }
  std::vector<double> ns(k, 0.0);
  for (int i = 0; i < n; ++i) ns[attr[i]] += 1;

  NumericVector counts(nclass);
  std::vector<int> mark(n, -1);
  std::vector<int> S;
  std::vector<double> scol(k);
  int pair_id = 0;

  for (int v = 0; v < n; ++v) {
    Rcpp::checkUserInterrupt();
    for (int u : nb[v]) {
      if (u <= v) continue;
      // S = N(u) | N(v) \ {u, v}
      S.clear();
      mark[u] = pair_id;
      mark[v] = pair_id;
      for (int w : nb[u]) if (mark[w] != pair_id) { mark[w] = pair_id; S.push_back(w); }
      for (int w : nb[v]) if (mark[w] != pair_id) { mark[w] = pair_id; S.push_back(w); }

      // dyadic triads: w adjacent to neither u nor v
      std::fill(scol.begin(), scol.end(), 0.0);
      scol[attr[u]] += 1;
      scol[attr[v]] += 1;
      for (int w : S) scol[attr[w]] += 1;
      for (int c = 0; c < k; ++c) {
        double cnt = ns[c] - scol[c];
        if (cnt > 0) {
          int code = (std::binary_search(out[v].begin(), out[v].end(), u) ? 1 : 0) +
                     (std::binary_search(out[u].begin(), out[u].end(), v) ? 4 : 0);
          counts[lookup[((code * k + attr[v]) * k + attr[u]) * k + c]] += cnt;
        }
      }

      // connected triads, each counted exactly once
      for (int w : S) {
        if (u < w || (v < w && w < u && !std::binary_search(nb[v].begin(), nb[v].end(), w))) {
          counts[triad_class(out, attr, lookup, k, v, u, w)] += 1;
        }
      }
      ++pair_id;
    }
  }
  return counts;
}
