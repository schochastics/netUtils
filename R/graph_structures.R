#' @title Adjacency list
#' @description Create adjacency lists from a graph, either for adjacent edges or for neighboring vertices. This version is faster than the version of igraph but less general.
#' @param g An igraph object
#' @details The function does not have a mode parameter and only returns the adjacency list comparable to as_adj_list(g,mode="all)
#' @return A list of numeric vectors.
#' @author David Schoch
#' @export
as_adj_list1 <- function(g){
  n <- igraph::vcount(g)
  lapply(1:n,function(i){
    x <- g[[i]][[1]]
    attr(x,"env") <- NULL
    attr(x,"graph") <- NULL
    class(x) <- NULL
    x
  })
}

#' @title weighted dense adjacency matrix
#' @description returns the weighted adjacency matrix in dense format
#' @param g An igraph object
#' @param attr Either NULL or a character string giving an edge attribute name. If NULL a traditional adjacency matrix is returned. If not NULL then the values of the given edge attribute are included in the adjacency matrix.
#' @details This method is faster than as_adj from igraph if you need the weighted adjacency matrix in dense format
#' @return Numeric matrix
#' @author David Schoch
#' @export
as_adj_weighted <- function(g,attr = NULL){
  as.matrix(igraph::as_adj(g,attr=attr,type = "both",sparse=T))
}


#' @title Clique Vertex Matrix
#' @description Creates the clique vertex matrix with entries (i,j) equal to one if node j is in clique i
#' @param g An igraph object
#' @return Numeric matrix
#' @author David Schoch
#' @export
clique_vertex_mat <- function(g){
  if(!igraph::is.igraph(g)){
    stop("g must be an igraph object")
  }
  if(igraph::is.directed(g)){
    warning("g is directed. Underlying undirected graph is used")
    g <- igraph::as.undirected(g)
  }
  mcl <- igraph::max_cliques(g)
  M <- matrix(0,length(mcl),igraph::vcount(g))
  for(i in 1:length(mcl)){
    M[i,mcl[[i]]] <- 1
  }
  M
}


#' @title Convert a list of graphs to an adjacency matrices
#' @description Convenience function that turns a list of igraph objects into adjacency matrices.
#' @param g_lst A list of igraph object
#' @param attr Either NULL or a character string giving an edge attribute name. If NULL a binary adjacency matrix is returned.
#' @param sparse Logical scalar, whether to create a sparse matrix. The 'Matrix' package must be installed for creating sparse matrices.
#' @return List of numeric matrices
#' @author David Schoch
#' @export
as_multi_adj <- function(g_lst,attr=NULL,sparse = FALSE){
  if(!all(unlist(lapply(g_lst,igraph::is.igraph)))){
    stop("all entries of g_lst must be igraph objects")
  }
  lapply(g_lst,function(x) igraph::as_adj(x,"both",attr=attr,sparse=sparse))
}
