#' @title Maximal Structural Equivalence
#' @description Calculates structural equivalence for an undirected graph
#' @param g An igraph object
#' @details Two nodes u and v are structurally equivalent if they have exactly the same neighbors. The equivalence classes produced with this function are either cliques or empty graphs.
#' @return vector of equivalence classes
#' @author David Schoch
#' @export
structural_equivalence <- function(g) {
  if(igraph::is.directed(g)){
    stop("g must be undirected")
  }
  adj <- lapply(igraph::neighborhood(g,mindist = 1), function(x) x - 1)
  deg <- igraph::degree(g)
  P <- mse(adj, deg)
  MSE <- which((P + t(P)) == 2, arr.ind = T)
  if (length(MSE) >= 1) {
    MSE <- t(apply(MSE, 1, sort))
    MSE <- MSE[!duplicated(MSE), ]
    g <- igraph::graph.empty()
    g <- igraph::add.vertices(g, nrow(P))
    g <- igraph::add.edges(g, c(t(MSE)))
    g <- igraph::as.undirected(g)
    MSE <- igraph::components(g,"weak")$membership
  } else {
    MSE <- 1:nrow(P)
  }
  return(MSE)
}
