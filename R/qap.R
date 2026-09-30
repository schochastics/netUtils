#' Graph correlation
#'
#' This function computes the correlation between networks. Implemented methods expect
#' the graph to be an adjacency matrix or an igraph object.
#'
#' @param object1  igraph object or adjacency matrix
#' @param object2  igraph object or adjacency matrix over the same vertex set as object1
#' @param diag logical. Should the diagonal of the adjacency matrices be included?
#'   Defaults to `FALSE`, the standard definition of graph correlation.
#' @param attr Either NULL or a character string giving an edge attribute name used as edge weights for igraph objects. If NULL, the unweighted adjacency matrices are used.
#' @param ... additional arguments (currently unused)
#'
#' @return correlation between graphs
#'
#' @export
graph_cor <- function(object1, object2, ...) UseMethod("graph_cor")

#' @rdname graph_cor
#' @method graph_cor default
#' @export
graph_cor.default <- function(object1, object2, ...) {
    stop(
        "don't know how to handle class ",
        dQuote(data.class(object1)),
        " and ",
        dQuote(data.class(object2))
    )
}

#' @rdname graph_cor
#' @method graph_cor igraph
#' @export
graph_cor.igraph <- function(object1, object2, diag = FALSE, attr = NULL, ...) {
    A1 <- adjacency_matrix(object1, attr = attr, sparse = FALSE)
    A2 <- adjacency_matrix(object2, attr = attr, sparse = FALSE)
    graph_cor.matrix(A1, A2, diag = diag)
}

#' @rdname graph_cor
#' @method graph_cor matrix
#' @export
graph_cor.matrix <- function(object1, object2, diag = FALSE, ...) {
    if (!identical(dim(object1), dim(object2))) {
        stop("object1 and object2 must have the same dimensions")
    }
    if (!diag && length(dim(object1)) == 2 && nrow(object1) == ncol(object1)) {
        base::diag(object1) <- NA
        base::diag(object2) <- NA
    }
    stats::cor(c(object1), c(object2), use = "complete.obs")
}

#' @rdname graph_cor
#' @method graph_cor array
#' @export
graph_cor.array <- graph_cor.matrix
