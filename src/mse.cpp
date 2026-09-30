#include <Rcpp.h>
using namespace Rcpp;

// Neighborhood inclusion: for every non-isolated vertex v, find all w != v with
// N(v) subset of N[w] (closed neighborhood). Returns the pairs (v, w) as a
// two-column matrix (0-based). Isolated vertices are handled in R.
// [[Rcpp::export]]
IntegerMatrix mse(List adjList, IntegerVector deg) {
  int n = deg.size();
  std::vector<int> marked(n, -1);
  std::vector<int> t(n, 0);
  std::vector<int> from, to;
  for (int v = 0; v < n; ++v) {
    Rcpp::checkUserInterrupt();
    std::vector<int> Nv = as<std::vector<int> >(adjList[v]);
    for (std::vector<int>::size_type j = 0; j != Nv.size(); ++j) {
      int u = Nv[j];
      std::vector<int> Nu = as<std::vector<int> >(adjList[u]);
      Nu.push_back(u);
      for (std::vector<int>::size_type i = 0; i != Nu.size(); ++i) {
        int w = Nu[i];
        if (w != v) {
          if (marked[w] != v) {
            marked[w] = v;
            t[w] = 0;
          }
          t[w] += 1;
          if (t[w] == deg[v]) {
            from.push_back(v);
            to.push_back(w);
          }
        }
      }
    }
  }
  IntegerMatrix pairs(from.size(), 2);
  for (std::size_t i = 0; i < from.size(); ++i) {
    pairs(i, 0) = from[i];
    pairs(i, 1) = to[i];
  }
  return pairs;
}
