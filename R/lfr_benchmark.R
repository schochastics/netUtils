#' LFR benchmark graphs
#' @description Generates benchmark networks for clustering tasks with a priori known communities. The algorithm accounts for the heterogeneity in the distributions of node degrees and of community sizes.
#' @param n Number of nodes in the created graph.
#' @param tau1 Power law exponent for the degree distribution of the created graph. This value must be strictly greater than one
#' @param tau2 Power law exponent for the community size distribution in the created graph. This value must be strictly greater than one
#' @param mu Fraction of inter-community edges incident to each node. This value must be in the interval 0 to 1.
#' @param average_degree Desired average degree of nodes in the created graph. This value must be in the interval 0 to n. Exactly one of this and `min_degree` must be specified, otherwise an error is raised
#' @param max_degree Maximum degree of nodes in the created graph. If not specified, this is set to n-1.
#' @param min_community Minimum size of communities in the graph. If not specified, this is set to `min_degree`
#' @param max_community Maximum size of communities in the graph. If not specified, this is set to n, the total number of nodes in the graph.
#' @param on number of overlapping nodes
#' @param om number of memberships of the overlapping nodes
#' @return an igraph object
#' @references A. Lancichinetti, S. Fortunato, and F. Radicchi.(2008) Benchmark graphs for testing community detection algorithms. Physical Review E, 78. arXiv:0805.4770
#' @details code adapted from <https://github.com/synwalk/synwalk-analysis/tree/master/lfr_generator>
#' @examples 
#' # Simple Girven-Newman benchmark graphs
#' g <- sample_lfr(n = 128,average_degree = 16,max_degree = 16,mu = 0.1,min_community = 32,max_community = 32)
#' @export
sample_lfr <- function(n,
                       tau1,
                       tau2,
                       mu,
                       average_degree = NULL,
                       max_degree = NULL,
                       min_community = NULL,
                       max_community = NULL,
                       on = 0,
                       om = 0) {

  if(missing(tau1)){
    tau1 <- 2
  }

  if(missing(tau2)){
    tau2 <- 1
  }
  if(missing(mu)){
    mu <- 0.1
  }

  if(missing(average_degree)){
    stop("average_degree must be specified")
  }

  if(average_degree>n || average_degree<=0) {
     stop("average_degree must be in the interval (0, n]")
  }

  if(missing(max_degree)){
    stop("max_degree must be specified")
  }
  if(max_degree>n || max_degree<=0) {
     stop("max_degree must be in the interval (0, n]")
  }

  if (!(tau1 >= 1)) {
    stop("tau1 must be greater than one")
  }
  if (!(tau2 >= 1)) {
    stop("tau2 must be greater than one")
  }
  if (mu < 0 || mu > 1) {
    stop("mu must be in the interval [0, 1]")
  }
  
  if(!is.null(min_community) && !is.null(max_community)){
    crange <- TRUE
  }

  if(is.null(min_community) && is.null(max_community)){
    min_community <- 0
    max_community <- 0
    crange <- FALSE
  } else if(xor(is.null(min_community),is.null(max_community))){
    stop("both min_community and max_community need to either be specified or not")
  }
  
  res <- benchmark(FALSE, FALSE,
    num_nodes = n,
    average_k = average_degree,
    max_degree = max_degree,
    tau = tau1,
    tau2 = tau2,
    mixing_parameter = mu,
    overlapping_nodes = on,
    overlap_membership = om,
    nmin = min_community,
    nmax = max_community,
    fixed_range = crange)
  
  g <- igraph::graph_from_adj_list(lapply(res[["edgelist"]],function(x) x+1),mode = "all")
  igraph::V(g)$membership <- unlist(res$membership) + 1
  g
}