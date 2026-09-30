#' @title helper function
#' @description small functions to deal with typical network problems
#'
#' @param g igraph object
#' @name helpers
#' @return igraph object
#' @author David Schoch
NULL

#' @rdname helpers
#' @export
biggest_component <- function(g) {
    comps <- igraph::components(g, mode = "weak")
    igraph::induced_subgraph(g, which(comps$membership == which.max(comps$csize)))
}

#' @rdname helpers
#' @export
delete_isolates <- function(g) {
    igraph::delete_vertices(g, which(igraph::degree(g) == 0))
}

# validate that `g` is a directed graph carrying a vertex attribute `vattr`
# with positive integer values, and return that attribute as an integer
# vector. Used by the *_census_attr functions.
validate_vattr <- function(g, vattr) {
    if (!igraph::is_igraph(g)) {
        stop("g must be an igraph object", call. = FALSE)
    }
    if (!igraph::is_directed(g)) {
        stop("g must be a directed graph", call. = FALSE)
    }
    if (!vattr %in% igraph::vertex_attr_names(g)) {
        stop("there is no vertex attribute called ", vattr, call. = FALSE)
    }
    attr <- igraph::vertex_attr(g, vattr)
    if (!is.numeric(attr)) {
        stop("vertex attribute must be numeric", call. = FALSE)
    }
    if (anyNA(attr) || any(attr != round(attr)) || any(attr < 1)) {
        stop("vertex attribute must contain positive integers without missing values", call. = FALSE)
    }
    as.integer(attr)
}
