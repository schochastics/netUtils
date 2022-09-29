#' LFR benchmark graphs
#' @description Generates benchmark networks for clustering tasks with a priori known communities. The algorithm accounts for the heterogeneity in the distributions of node degrees and of community sizes.
#' @param n Number of nodes in the created graph.
#' @param tau1 Power law exponent for the degree distribution of the created graph. This value must be strictly greater than one
#' @param tau2 Power law exponent for the community size distribution in the created graph. This value must be strictly greater than one
#' @param mu Fraction of inter-community edges incident to each node. This value must be in the interval 0 to 1.
#' @param average_degree Desired average degree of nodes in the created graph. This value must be in the interval 0 to n. Exactly one of this and `min_degree` must be specified, otherwise an error is raised
#' @param min_degree Minimum degree of nodes in the created graph. This value must be in the interval 0 to n. Exactly one of this and `average_degree` must be specified, otherwise an error is raised.
#' @param max_degree Maximum degree of nodes in the created graph. If not specified, this is set to n-1.
#' @param min_community Minimum size of communities in the graph. If not specified, this is set to `min_degree`
#' @param max_community Maximum size of communities in the graph. If not specified, this is set to n, the total number of nodes in the graph.
#' @param tol Tolerance when comparing floats, specifically when comparing average degree values.
#' @param max_iters Maximum number of iterations to try to create the community sizes, degree distribution, and community affiliations.
#' @return an igraph object
#' @references A. Lancichinetti, S. Fortunato, and F. Radicchi.(2008) Benchmark graphs for testing community detection algorithms. Physical Review E, 78. arXiv:0805.4770
#' @details code adapted from the implementation in NetworkX: https://networkx.org/documentation/stable/_modules/networkx/generators/community.html#LFR_benchmark_graph
#' @author David Schoch
#' @export
sample_lfr <- function(n,
                       tau1,
                       tau2,
                       mu,
                       average_degree=NULL,
                       min_degree=NULL,
                       max_degree=NULL,
                       min_community=NULL,
                       max_community=NULL,
                       tol=1.0e-7,
                       max_iters=500){
  if(!requireNamespace("VGAM", quietly = TRUE)){
    stop("The package 'VGAM' is needed for this function")
  }
  if(!(tau1 > 1)){
    stop("tau1 must be greater than one")
  }
  if(!(tau2 > 1)){
    stop("tau2 must be greater than one")
  }
  if(mu<0 | mu>1){
    stop("mu must be in the interval [0, 1]")
  }
  if(is.null(max_degree)){
    max_degree <- (n-1)/2 #TODO: fix
  } else if(max_degree>n | max_degree<=0){
    stop("max_degree must be in the interval (0, n]")
  }
  if(is.null(min_degree)){
    min_degree  <- generate_min_degree(tau1, average_degree, max_degree, tol, max_iters)
    # print("done min_degree")
  }

  # Generate a degree sequence with a power law distribution.
  low  <- min_degree
  high <- max_degree
  # condition <- function(seq){
  #   sum(seq) %% 2 == 0
  # }
  # length1 <- function(seq){
  #   return(length(seq) >= n)
  # }


  # deg_seq <- powerlaw_sequence(n,tau1, low, high,condition,length1, max_iters)
  deg_seq <- rPLB(n, b = -tau1, low, high)
  # print("done deg_seq")
  # Validate parameters for generating the community size sequence.
  if(is.null(min_community)){
    min_community <- min(deg_seq)
  }
  if(is.null(max_community)){
    max_community <- max(deg_seq)
  }
  low  <- min_community
  high <- max(c(max_community,low+1))

  # condition <- function(seq){
  #   return(sum(seq) == n)
  # }
  #
  # length1 <- function(seq){
  #   return(sum(seq) >= n)
  # }

  # comms <- powerlaw_sequence(n,tau2, low, high, condition, length1, max_iters)

  for(i in 1:max_iters){
    seqi <- rPLB(n, b = -tau2, low, high)
    if(any(cumsum(seqi)==n)){
      comms <-  seqi[1:which(cumsum(seqi)==n)]
      break()
    }
    if(i==max_iters){
      stop("Could not create power law sequence for communities")
    }
  }
  max_iters <- max_iters * n

  communities  <- generate_communities(deg_seq, comms, mu, max_iters)

  g <- igraph::graph.empty(n = n,directed = FALSE)
  igraph::V(g)$cluster <- 0
  for(i in seq_along(communities)){
    com <- communities[[i]]
    for(u in com){
      while(igraph::degree(g)[u]<round(deg_seq[u] * (1 - mu))){
        v <- sample(com,1)
        g <- igraph::add.edges(g,c(u,v))
      }
      while(igraph::degree(g)[u]<deg_seq[u]){
        v <- sample(seq_len(n),1)
        g <- igraph::add.edges(g,c(u,v))
      }
      igraph::V(g)$cluster[u] <- i
    }
  }
  # g <- igraph::graph.empty(n = n,directed = FALSE)
  # igraph::V(g)$cluster <- 0
  # delta <- deg_seq
  # for(i in seq_along(communities)){
  #   com <- communities[[i]]
  #   el <- do.call("rbind",lapply(com,function(u) cbind(u,sample(com,deg_seq[u]*(1-mu),replace = TRUE))))
  #   g <- igraph::add.edges(g,c(t(el)))
  #   igraph::V(g)$cluster[com] <- i
  # }
  # delta <- deg_seq-igraph::degree(g)
  # for(i in 1:n){
  #   if(delta[i]>0){
  #     el <- cbind(i,sample(1:n,delta[i]))
  #     g <- igraph::add.edges(g,c(t(el)))
  #     delta[el[,2]] <- delta[el[,2]] - 1
  #     delta[i] <- 0
  #   }
  # }
  igraph::simplify(g)
}


# helpers

generate_min_degree <- function(gamma, average_degree, max_degree, tolerance, max_iters){
  min_deg_top <- max_degree
  min_deg_bot <- 1
  min_deg_mid <- (min_deg_top - min_deg_bot) / 2 + min_deg_bot
  itrs <- 0
  mid_avg_deg <- 0
  while(abs(mid_avg_deg - average_degree) > tolerance){
    if(itrs > max_iters){
      stop("Could not match average_degree" )
    }
    x <- seq(floor(min_deg_mid), max_degree)
    mid_avg_deg <- sum((x^(-gamma + 1)) / VGAM::zeta(gamma, shift = min_deg_mid))
    if(mid_avg_deg > average_degree){
      min_deg_top <- min_deg_mid
      min_deg_mid <- (min_deg_top - min_deg_bot) / 2 + min_deg_bot
    } else{
      min_deg_bot <- min_deg_mid
      min_deg_mid <- (min_deg_top - min_deg_bot) / 2 + min_deg_bot
    }
    itrs <- itrs + 1
  }
  return(round(min_deg_mid))
}

powerlaw_sequence <- function(n,gamma, low, high, condition,length1, max_iters){
  # bool_len <- function(seq){
  #   return(length(seq) >= n)
  # }
  for(i in seq_len(max_iters)){
    seqi <- c()
    while(!length1(seqi)){
      seqi <- c(seqi,zipf_rv_below(gamma, low, high))
    }
    if(condition(seqi)){
      return(seqi)
    }
  }
  stop("Could not create power law sequence")
}


zipf_rv_below <- function(gamma, xmin, threshold){
  result <- zipf_rv(gamma, xmin)
  while(result > threshold){
    result <- zipf_rv(gamma, xmin)
  }
  return(result)
}

zipf_rv <- function(alpha, xmin = 1){
  if(xmin < 1){
    stop("xmin must be greater 1")
  }
  if( alpha <= 1){
    stop("alpha must be greater 1.0")
  }
  a1<- alpha - 1.0
  b = 2^a1
  while(TRUE){
    u <- 1.0 - stats::runif(1)  # u in (0,1]
    v <- stats::runif(1)  # v in [0,1)
    x <- floor(xmin * u^(-1.0 / a1))
    t <- (1.0 + (1.0 / x))^a1
    if ((v * x * (t - 1.0) / (b - 1.0)) <= (t / b)){
      break()
    }
  }
  return(x)
}

generate_communities <- function(degree_seq, community_sizes, mu, max_iters){
  result = vector("list",length = length(community_sizes))
  n <- length(degree_seq)
  free <- seq_len(n)
  for(i in seq_len(max_iters)){
    v <- free[1]
    free <- free[-1]
    com <- sample(seq_along(community_sizes),1)
    s_in <- round(degree_seq[v] * (1 - mu))
    if(s_in > max(community_sizes)){
      s_in <- max(community_sizes[com]) * (1-mu)
    }
    if(s_in < community_sizes[com]){
      result[[com]] <- c(result[[com]],v)
    } else{
      free <- c(free,v)
    }
    if( length(result[[com]]) > community_sizes[com]){
      free <- c(free,result[[com]][1])
      result[[com]] <- result[[com]][-1]
    }
    if(length(free)==0){
      return(result)
    }
  }
  stop("Could not assign communities; try increasing min_community")
}

rPLB <- function(n = 1, b = -2, xmin = 1, xmax = 100)
{
  if(xmin <= 0 | xmin >= xmax) stop("Parameters out of bounds in rPLB")
  u <- stats::runif(n)
  if(b != -1)
  { y <- ( u*xmax^(b+1) +  (1-u) * xmin^(b+1) ) ^ (1/(b+1))
  } else
  { y <- xmax^u * xmin^(1-u)
  }
  return(round(y))
}
