#' @title Maximal Structural Equivalence
#' @description Calculates structural equivalence for an undirected graph
#' @param g An igraph object
#' @details Two nodes u and v are structurally equivalent if they have exactly the same neighbors. The equivalence classes produced with this function are either cliques or empty graphs.
#' @return vector of equivalence classes
#' @author David Schoch
#' @export
structural_equivalence <- function(g) {
    if (igraph::is_directed(g)) {
        stop("g must be undirected")
    }
    n <- igraph::vcount(g)
    adj <- lapply(igraph::neighborhood(g, mindist = 1), function(x) as.integer(x) - 1L)
    # number of distinct neighbors (degree would count multiple edges and loops)
    deg <- lengths(adj)
    dom <- mse(adj, deg) + 1L
    # u and v are equivalent if each dominates the other
    key <- (dom[, 1] - 1) * n + dom[, 2]
    rev_key <- (dom[, 2] - 1) * n + dom[, 1]
    equiv <- dom[key %in% rev_key & dom[, 1] < dom[, 2], , drop = FALSE]
    # all isolates are equivalent
    iso <- which(deg == 0)
    if (length(iso) > 1) {
        equiv <- rbind(equiv, cbind(iso[-length(iso)], iso[-1]))
    }
    if (nrow(equiv) == 0) {
        return(seq_len(n))
    }
    h <- igraph::make_empty_graph(n, directed = FALSE)
    h <- igraph::add_edges(h, c(t(equiv)))
    igraph::components(h)$membership
}
