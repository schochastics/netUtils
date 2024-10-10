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
