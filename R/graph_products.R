#' @title Cartesian product of two graphs
#' @description Compute the Cartesian product of two graphs
#' @param g An igraph object
#' @param h An igraph object
#' @details See https://en.wikipedia.org/wiki/Cartesian_product_of_graphs.
#' The result is undirected and its vertices are named "u-v", where u and v are
#' the names (or ids) of the vertices in `g` and `h`.
#' @return Cartesian product as igraph object
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- make_ring(4)
#' h <- make_full_graph(2)
#' graph_cartesian(g, h)
#' @export
graph_cartesian <- function(g, h) {
    prod <- product_setup(g, h)
    elg <- prod$elg
    elh <- prod$elh
    ng <- prod$ng
    nh <- prod$nh
    id <- prod$id

    # (a, j) -- (b, j) for every edge a -- b of g and vertex j of h
    j <- rep(seq_len(nh), times = nrow(elg))
    e1 <- rbind(id(rep(elg[, 1], each = nh), j), id(rep(elg[, 2], each = nh), j))
    # (i, c) -- (i, d) for every vertex i of g and edge c -- d of h
    i <- rep(seq_len(ng), each = nrow(elh))
    e2 <- rbind(id(i, rep(elh[, 1], times = ng)), id(i, rep(elh[, 2], times = ng)))

    product_graph(prod, c(e1, e2))
}

# direct graph product ----
#' @title Direct product of two graphs
#' @description Compute the direct product of two graphs
#' @param g An igraph object
#' @param h An igraph object
#' @details See https://en.wikipedia.org/wiki/Tensor_product_of_graphs.
#' The result is undirected and its vertices are named "u-v", where u and v are
#' the names (or ids) of the vertices in `g` and `h`.
#' @return Direct product as igraph object
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- make_ring(4)
#' h <- make_full_graph(2)
#' graph_direct(g, h)
#' @export
graph_direct <- function(g, h) {
    prod <- product_setup(g, h)
    elg <- prod$elg
    elh <- prod$elh
    id <- prod$id

    # for edges a -- b of g and c -- d of h: (a, c) -- (b, d) and (b, c) -- (a, d)
    eg <- rep(seq_len(nrow(elg)), times = nrow(elh))
    eh <- rep(seq_len(nrow(elh)), each = nrow(elg))
    ga <- elg[eg, 1]
    gb <- elg[eg, 2]
    hc <- elh[eh, 1]
    hd <- elh[eh, 2]
    edges <- rbind(id(ga, hc), id(gb, hd), id(gb, hc), id(ga, hd))

    product_graph(prod, c(edges))
}

# shared helpers for graph products: vertex (i, j) has id (i - 1) * nh + j
product_setup <- function(g, h) {
    ng <- igraph::vcount(g)
    nh <- igraph::vcount(h)
    list(
        elg = igraph::as_edgelist(g, names = FALSE),
        elh = igraph::as_edgelist(h, names = FALSE),
        ng = ng,
        nh = nh,
        id = function(i, j) (i - 1) * nh + j,
        names = paste(
            rep(vertex_labels(g), each = nh),
            rep(vertex_labels(h), times = ng),
            sep = "-"
        )
    )
}

product_graph <- function(prod, edges) {
    res <- igraph::make_empty_graph(prod$ng * prod$nh, directed = FALSE)
    res <- igraph::add_edges(res, edges)
    igraph::V(res)$name <- prod$names
    res
}

vertex_labels <- function(g) {
    if ("name" %in% igraph::vertex_attr_names(g)) {
        igraph::V(g)$name
    } else {
        seq_len(igraph::vcount(g))
    }
}
