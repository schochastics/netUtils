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
    elg <- igraph::as_edgelist(g)
    elh <- igraph::as_edgelist(h)
    vg <- 1:igraph::vcount(g)
    vh <- 1:igraph::vcount(h)
    el_list <- vector("list", nrow(elg) + nrow(elh))
    for (i in seq_len(nrow(elg))) {
        el_list[[i]] <- matrix(apply(expand.grid(elg[i, ], vh), 1, function(x) paste(x, collapse = "-")), ncol = 2, byrow = TRUE)
    }
    for (i in seq_len(nrow(elh))) {
        el_list[[nrow(elg) + i]] <- matrix(apply(expand.grid(elh[i, ], vg), 1, function(x) paste(rev(x), collapse = "-")), ncol = 2, byrow = TRUE)
    }
    el <- do.call(rbind, el_list)
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
    elg <- igraph::as_edgelist(g)
    elh <- igraph::as_edgelist(h)
    el_list <- vector("list", nrow(elg) * nrow(elh) * 2)
    idx <- 1L
    for (i in seq_len(nrow(elg))) {
        for (j in seq_len(nrow(elh))) {
            el_list[[idx]] <- c(paste0(elg[i, 1], "-", elh[j, 1]), paste0(elg[i, 2], "-", elh[j, 2]))
            idx <- idx + 1L
            el_list[[idx]] <- c(paste0(elg[i, 2], "-", elh[j, 1]), paste0(elg[i, 1], "-", elh[j, 2]))
            idx <- idx + 1L
        }
    }
    el <- do.call(rbind, el_list)
    igraph::graph_from_edgelist(el, directed = FALSE)
}
