#' @title Cartesian product of two graphs
#' @description Compute the Cartesian product of two graphs
#' @param g An igraph object
#' @param h An igraph object
#' @details See https://en.wikipedia.org/wiki/Cartesian_product_of_graphs
#' @return Cartesian product as igraph object
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- make_ring(4)
#' h <- make_full_graph(2)
#' graph_cartesian(g, h)
#' @export
graph_cartesian <- function(g, h) {
    elg <- igraph::get.edgelist(g)
    elh <- igraph::get.edgelist(h)
    vg <- 1:igraph::vcount(g)
    vh <- 1:igraph::vcount(h)
    el <- matrix(0, 0, 2)
    for (i in seq_len(nrow(elg))) {
        el_tmp <- matrix(apply(expand.grid(elg[i, ], vh), 1, function(x) paste(x, collapse = "-")), ncol = 2, byrow = TRUE)
        el <- rbind(el, el_tmp)
    }
    for (i in seq_len(nrow(elh))) {
        el_tmp <- matrix(apply(expand.grid(elh[i, ], vg), 1, function(x) paste(rev(x), collapse = "-")), ncol = 2, byrow = TRUE)
        el <- rbind(el, el_tmp)
    }
    igraph::graph_from_edgelist(el, F)
}

# direct graph product ----
#' @title Direct product of two graphs
#' @description Compute the direct product of two graphs
#' @param g An igraph object
#' @param h An igraph object
#' @details See https://en.wikipedia.org/wiki/Tensor_product_of_graphs
#' @return Direct product as igraph object
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- make_ring(4)
#' h <- make_full_graph(2)
#' graph_direct(g, h)
#' @export
graph_direct <- function(g, h) {
    elg <- igraph::get.edgelist(g)
    elh <- igraph::get.edgelist(h)
    el <- matrix(0, 0, 2)
    for (i in seq_len(nrow(elg))) {
        for (j in seq_len(nrow(elh))) {
            edg <- c(paste0(elg[i, 1], "-", elh[j, 1]), paste0(elg[i, 2], "-", elh[j, 2]))
            el <- rbind(el, edg)
            edg <- c(paste0(elg[i, 2], "-", elh[j, 1]), paste0(elg[i, 1], "-", elh[j, 2]))
            el <- rbind(el, edg)
        }
    }
    igraph::graph_from_edgelist(el, directed = FALSE)
}
