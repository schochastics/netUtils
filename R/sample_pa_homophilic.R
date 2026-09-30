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
#' @examples
#' # maximally heterophilic network
#' sample_pa_homophilic(n = 50, m = 2, minority_fraction = 0.2, h_ab = 1)
#' # maximally homophilic network
#' sample_pa_homophilic(n = 50, m = 2, minority_fraction = 0.2, h_ab = 0)
#' @export
sample_pa_homophilic <- function(n, m, minority_fraction, h_ab, h_ba = NULL, directed = FALSE) {
    if (is.null(h_ba)) {
        h_ba <- h_ab
    }
    if (length(n) != 1 || length(m) != 1 || n != round(n) || m != round(m) || m < 1 || n <= m) {
        stop("n and m must be positive integers with m < n")
    }
    if (minority_fraction < 0 || minority_fraction > 1) {
        stop("minority_fraction must be in the interval [0, 1]")
    }
    if (h_ab < 0 || h_ab > 1 || h_ba < 0 || h_ba > 1) {
        stop("h_ab and h_ba must be in the interval [0, 1]")
    }
    minority_attr <- sample(
        c(
            rep(TRUE, floor(minority_fraction * n)),
            rep(FALSE, n - floor(minority_fraction * n))
        )
    )

    # attachment weight by group of source (rows) and target (columns),
    # index 1 = majority, 2 = minority
    homophily <- matrix(c(1 - h_ba, h_ab, h_ba, 1 - h_ab), 2, 2)
    grp <- minority_attr + 1

    deg <- numeric(n)
    edges <- vector("list", n)
    target_list <- seq_len(m)
    for (source in seq(m + 1, n)) {
        targets <- pick_targets(deg, source, target_list, homophily, grp, m)
        if (length(targets) != 0) {
            edges[[source]] <- rbind(source, targets)
            deg[source] <- deg[source] + length(targets)
            deg[targets] <- deg[targets] + 1
        }
        target_list <- c(target_list, source)
    }

    g <- igraph::make_empty_graph(n = 0, directed = directed)
    g <- igraph::add_vertices(g, n, attr = list(minority = minority_attr))
    igraph::add_edges(g, unlist(edges))
}

pick_targets <- function(deg, source, target_list, homophily, grp, m) {
    target_prob <- homophily[grp[source], grp[target_list]] * (deg[target_list] + 1e-5)

    if (sum(target_prob > 0) < m) {
        return(c())
    }
    sample(target_list, m, prob = target_prob)
}
