#' Generate random graphs with a given coreness sequence
#' @description Similar to \link[igraph]{sample_degseq} just with \link[igraph]{coreness}
#' @param cores coreness sequence
#' @details The code is an adaption of the python code from here: adopted from https://github.com/ktvank/Random-Graphs-with-Prescribed-K-Core-Sequences/
#' @return igraph object of graph with the same coreness sequence as the input
#' @references
#'Van Koevering, Katherine, Austin R. Benson, and Jon Kleinberg. 2021. ‘Random Graphs with Prescribed K-Core Sequences: A New Null Model for Network Analysis’. ArXiv:2102.12604. https://doi.org/10.1145/3442381.3450001.
#' @author David Schoch
#' @export
sample_coreseq <- function(cores){
  cores <- sort(cores,decreasing = TRUE)

  if(!is_kcoreseq(cores)){
    stop("sequence is not a valid kcore sequence")
  }
  K <- cores[1]

  indices <- lapply(0:K,function(x) which(cores==x))

  if(K == 0){
    n0 <- length(indices[[1]])
    g <- igraph::graph.empty(n = n0, directed = FALSE)
    return(g)

  } else if(K == 1){
    n0 <- length(indices[[1]])
    n1 <- length(indices[[2]])
    g <- igraph::graph.empty(n = n1,directed = FALSE)
    g <- igraph::add.edges(g,edges = c(t(cbind(1:(n1-1),2:n1))))
    g <- igraph::add.vertices(g,nv = n0)
    return(g)
  } else if(K == 2){
    n0 <- length(indices[[1]])
    n1 <- length(indices[[2]])
    n2 <- length(indices[[3]])
    g <- igraph::graph.empty(n = n2,directed = FALSE)
    g <- igraph::add.edges(g,edges = c(t(cbind(1:(n2-1),2:n2))))
    g <- igraph::add.edges(g,edges = c(1,n2))
    g <- add_lower_nodes(cores,indices,g,1)
    return(g)
  } else{
    g <- igraph::graph.empty(directed = FALSE)
    g <- generate_k_graph(K,sum(cores==K),g)
    g <- add_lower_nodes(cores,indices,g,K-1)
    return(g)
  }
}

is_kcoreseq <- function(cores){
  cores <- sort(cores,decreasing = TRUE)
  num_max_core_val <- sum(cores==max(cores))
  return(num_max_core_val>=cores[1])
}

add_lower_nodes <- function(cores,indices,g,k){
  K <- cores[1]
  higher_nodes <- sort(unname(unlist(indices[(k+2):(K+1)])))
  g <- igraph::add.vertices(g,nv = length(indices[[k+1]]))
  if(k==0){
    return(g)
  }
  for(v in indices[[k+1]]){
    randv <- sample(higher_nodes,k,replace = FALSE)
    g <- igraph::add_edges(g,c(t(cbind(randv,v))))
  }
  return(add_lower_nodes(cores,indices,g,k-1))
}

generate_k_graph <- function(C,N,g){
  if(!((C%%2) == (N%%2))){
    g <- igraph::add_vertices(g,nv = N)
    g <- igraph::add.edges(g,edges = c(t(cbind(1:(N-1),2:N))))
    g <- igraph::add.edges(g,edges = c(1,N))
    z <- ceiling((N-C+1)/2)
    for(i in 0:(N-1)){
      start <- (i+z+1)%%(N) #BUG POTENTIAL!!!
      stop   <- (i-z)%%(N)  #BUG POTENTIAL!!!

      if(stop>=start){
        listv <- start:(stop-1) #BUG POTENTIAL!!!
        edges <- c(t(cbind(listv,i)))
      } else{
        listv <- c((0:(N-1))[0:(stop)],(0:(N-1))[(start+1):N])
        edges <- c(t(cbind(listv,i)))
      }
      g <- igraph::add.edges(g,edges+1)
      g <- igraph::simplify(g)
    }
  } else{
    g <- generate_k_graph(C,N-1,g)
    g <- igraph::add_vertices(g,1)
    randv <- sample(1:(N-1),C)
    g <- igraph::add_edges(g,c(t(cbind(randv,N))))
  }
  return(g)
}
