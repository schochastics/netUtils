#' Homophilic random graph using BA preferential attachment model
#' @description A graph of n nodes is grown by attaching new nodes each with m
#' edges that are preferentially attached to existing nodes with high
#' degree, depending on the homophily parameters.
#' @param n number of nodes
#' @param m number of edges a new node is connected to
#' @param minority_fraction fraction of nodes that belong to the minority group
#' @param h_ab probability to connect a node from group a with groub b
#' @param h_ba probability to connect a node from group b with groub a. If NULL, h_ab is used.
#' @param directed should a directed network be created
#' @details The code is an adaption of the python code from https://github.com/gesiscss/HomophilicNtwMinorities/
#' @return igraph object
#' @references
#' Karimi, F., Génois, M., Wagner, C., Singer, P., & Strohmaier, M. (2018). Homophily influences ranking of minorities in social networks. Scientific reports, 8(1), 1-12. (https://www.nature.com/articles/s41598-018-29405-7)
#'
#' Espín-Noboa, L., Wagner, C., Strohmaier, M., & Karimi, F. (2022). Inequality and inequity in network-based ranking and recommendation algorithms. Scientific reports, 12(1), 1-14. (https://www.nature.com/articles/s41598-022-05434-1)
#' @author David Schoch
#' #maximally heterophilic network
#' sample_pa_homophilic(n = 50, m = 2,minority_fraction = 0.2,h_ab = 1)
#' #maximally homophilic network
#' sample_pa_homophilic(n = 50, m = 2,minority_fraction = 0.2,h_ab = 0)
#' @export
sample_pa_homophilic <- function(n, m, minority_fraction, h_ab, h_ba = NULL, directed = FALSE) {
    if (is.null(h_ba)) {
        h_ba <- h_ab
    }
    h_aa <- 1 - h_ab
    h_bb <- 1 - h_ba
    minority_attr <- sample(
        c(
            rep(TRUE, floor(minority_fraction * n)),
            rep(FALSE, n - floor(minority_fraction * n))
        )
    )

    g <- igraph::graph.empty(n = 0, directed = directed)
    g <- igraph::add.vertices(g, n, attr = list(minority = minority_attr))

    dist <- matrix(NA, n, n)
    dist[outer(minority_attr, minority_attr, "&")] <- h_aa # within minority
    dist[outer(!minority_attr, !minority_attr, "&")] <- h_bb # within majority
    dist[outer(minority_attr, !minority_attr, "&")] <- h_ab # min->maj
    dist[outer(!minority_attr, minority_attr, "&")] <- h_ba # maj->min


    target_list <- seq_len(m)
    source <- m + 1
    while (source <= n) {
        deg <- igraph::degree(g)
        targets <- pick_targets(deg, source, target_list, dist, m)
        if (length(targets != 0)) {
            el <- rbind(source, targets)
            g <- igraph::add.edges(g, c(el))
        }
        target_list <- c(target_list, source)
        source <- source + 1
    }
    return(g)
}

pick_targets <- function(deg, source, target_list, dist, m) {
    target_prob <- dist[source, target_list] * (deg[target_list] + 1e-5)

    if (sum(target_prob > 0) < m) {
        return(c())
    } else {
        targets <- sample(target_list, m, prob = target_prob)
        return(targets)
    }
}
