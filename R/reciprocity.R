#' Reciprocity correlation coefficient
#'
#' @param g igraph object. should be a directed graph
#' @return Reciprocity as a correlation
#' @details
#' The usual definition of reciprocity has some defects. It cannot tell the relative difference of reciprocity compared with purely random network with the same number of vertices and edges.
#' The useful information from reciprocity is not the value itself, but whether mutual links occur more or less often than expected by chance.
#'
#' To overcome this issue, reciprocity can be defined as the correlation coefficient between the entries of the adjacency matrix of a directed graph:
#' \deqn{
#'  \frac{\sum_{i\neq j} (a_{ij} - a')((a_{ji} - a')}{\sum_{i\neq j} (a_{ij} - a')^2}
#' }
#' where a' is the density of g.
#'
#' This definition gives an absolute quantity which directly allows one to distinguish between reciprocal (>0) and antireciprocal (< 0) networks, with mutual links occurring more and less often than random respectively.
#' @references Diego Garlaschelli; Loffredo, Maria I. (2004). "Patterns of Link Reciprocity in Directed Networks". Physical Review Letters. American Physical Society. 93 (26): 268701
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- sample_gnp(20,p = 0.3,directed = TRUE)
#' reciprocity(g)
#' reciprocity_cor(g)
#' @export
reciprocity_cor <- function(g){
  if(!igraph::is.igraph(g)){
    stop("g must be an igraph object")
  }
  if(!igraph::is.directed(g)){
    stop("g must be directed")
  }
  r <- igraph::reciprocity(g)
  d <- igraph::graph.density(g)
  if(d==1){
    return(0)
  } else{
    (r-d)/(1-d)
  }
}
