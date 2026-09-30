#' @title Adjacency list
#' @description Create adjacency lists from a graph, either for adjacent edges or for neighboring vertices. This version is faster than the version of igraph but less general.
#' @param g An igraph object
#' @details The function does not have a mode parameter and returns the same neighbors as `as_adj_list(g, mode = "all")`, as plain integer vectors. For directed graphs, both in- and out-neighbors are returned.
#' @return A list of numeric vectors.
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- make_ring(10)
#' as_adj_list1(g)
#' @export
as_adj_list1 <- function(g) {
    lapply(unname(igraph::as_adj_list(g, mode = "all")), as.integer)
}

#' @title weighted dense adjacency matrix
#' @description returns the weighted adjacency matrix in dense format
#' @param g An igraph object
#' @param attr Either NULL or a character string giving an edge attribute name. If NULL a traditional adjacency matrix is returned. If not NULL then the values of the given edge attribute are included in the adjacency matrix.
#' @details This method is faster than as_adj from igraph if you need the weighted adjacency matrix in dense format
#' @return Numeric matrix
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- sample_gnp(10, 0.2)
#' E(g)$weight <- runif(ecount(g))
#' as_adj_weighted(g, attr = "weight")
#' @export
as_adj_weighted <- function(g, attr = NULL) {
    as.matrix(adjacency_matrix(g, attr = attr, sparse = TRUE))
}

# as_adjacency_matrix() with an edge attribute as weights, or unweighted if
# attr is NULL. igraph >= 3.0.0 replaced the `attr` argument by `weights` and
# uses the "weight" edge attribute by default.
adjacency_matrix <- function(g, attr = NULL, sparse = FALSE) {
    has_weights <- "weights" %in% names(formals(igraph::as_adjacency_matrix))
    if (is.null(attr)) {
        if (has_weights) {
            return(igraph::as_adjacency_matrix(g, type = "both", weights = NA, sparse = sparse))
        }
        return(igraph::as_adjacency_matrix(g, type = "both", sparse = sparse))
    }
    if (!attr %in% igraph::edge_attr_names(g)) {
        stop("there is no edge attribute called ", attr, call. = FALSE)
    }
    if (has_weights) {
        igraph::as_adjacency_matrix(g, type = "both", weights = igraph::edge_attr(g, attr), sparse = sparse)
    } else {
        igraph::as_adjacency_matrix(g, type = "both", attr = attr, sparse = sparse)
    }
}


#' @title Clique Vertex Matrix
#' @description Creates the clique vertex matrix with entries (i,j) equal to one if node j is in clique i
#' @param g An igraph object
#' @return Numeric matrix
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- sample_gnp(10, 0.2)
#' clique_vertex_mat(g)
#' @export
clique_vertex_mat <- function(g) {
    if (!igraph::is_igraph(g)) {
        stop("g must be an igraph object")
    }
    if (igraph::is_directed(g)) {
        warning("g is directed. Underlying undirected graph is used")
        g <- igraph::as_undirected(g)
    }
    mcl <- igraph::max_cliques(g)
    M <- matrix(0, length(mcl), igraph::vcount(g))
    for (i in seq_along(mcl)) {
        M[i, mcl[[i]]] <- 1
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
as_multi_adj <- function(g_lst, attr = NULL, sparse = FALSE) {
    if (!all(vapply(g_lst, igraph::is_igraph, logical(1)))) {
        stop("all entries of g_lst must be igraph objects")
    }
    lapply(g_lst, adjacency_matrix, attr = attr, sparse = sparse)
}
