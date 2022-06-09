#' Graph correlation
#'
#' This function computes the correlation between networks. Implemented methods expect
#' the graph to be an adjacency matrix, an igraph, or a network object.
#'
#' @param object1  igraph object or adjacency matrix
#' @param object2  igraph object or adjacency matrix over the same vertex set as object1
#' @param ... additional arguments
#'
#' @return correlation between graphs
#'
#' @export
graph_cor <- function(object1,object2) UseMethod("graph_cor")

#' @rdname graph_cor
#' @method graph_cor default
#' @export
graph_cor.default <- function(object1,object2) {
  stop("don't know how to handle class ", dQuote(data.class(object1)), " and ", dQuote(data.class(object2)))
}

#' @rdname graph_cor
#' @method graph_cor igraph
#' @export
graph_cor.igraph <- function(object1,object2,...){
  A1 <- igraph::as_adj(object1,type = "both",sparse = FALSE,...)
  A2 <- igraph::as_adj(object2,type = "both",sparse = FALSE,...)
  graph_cor.matrix(A1,A2)
}

#' @rdname graph_cor
#' @method graph_cor matrix
#' @export
graph_cor.matrix <- function(object1,object2){
  stats::cor(c(object1),c(object2),use = "complete.obs")
}

#' @rdname graph_cor
#' @method graph_cor array
#' @export
graph_cor.array <- function(object1,object2){
  stats::cor(c(object1),c(object2),use = "complete.obs")
}
