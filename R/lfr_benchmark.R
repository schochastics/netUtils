#' LFR benchmark graphs
#' @description Generates benchmark networks for clustering tasks with a priori known communities. The algorithm accounts for the heterogeneity in the distributions of node degrees and of community sizes.
#' @param n Number of nodes in the created graph.
#' @param tau1 Power law exponent for the degree distribution of the created graph. This value must be at least one.
#' @param tau2 Power law exponent for the community size distribution in the created graph. This value must be at least one.
#' @param mu Fraction of inter-community edges incident to each node. This value must be in the interval 0 to 1.
#' @param average_degree Desired average degree of nodes in the created graph. This value must be in the interval (0, n] and is required.
#' @param max_degree Maximum degree of nodes in the created graph. This value must be in the interval (0, n] and is required.
#' @param min_community Minimum size of communities in the graph. Either both or none of `min_community` and `max_community` must be specified.
#' If none are specified, the community size range is set automatically to `[max(k_min, 3), max_degree]`,
#' where `k_min` is the minimum degree implied by `average_degree`, `max_degree` and `tau1`.
#' @param max_community Maximum size of communities in the graph. Must be at least `min_community`.
#' @param on number of overlapping nodes (a non-negative integer not larger than `n`).
#' @param om number of memberships of the overlapping nodes. Must be at least 2 if `on > 0`.
#' @param verbose logical. Should progress messages of the generator be printed?
#' @return an igraph object with two vertex attributes: `membership`, an integer vector
#' holding the (first) community of each vertex, and `memberships`, a list holding
#' all communities of each vertex (only overlapping vertices have more than one).
#' @references A. Lancichinetti, S. Fortunato, and F. Radicchi.(2008) Benchmark graphs for testing community detection algorithms. Physical Review E, 78. arXiv:0805.4770
#' @details code adapted from <https://github.com/synwalk/synwalk-analysis/tree/master/lfr_generator>.
#' Random numbers are drawn from R's random number generator, so results can be reproduced with [set.seed()].
#' @examples
#' # Simple Girven-Newman benchmark graphs
#' g <- sample_lfr(
#'     n = 128, average_degree = 16,
#'     max_degree = 16, mu = 0.1,
#'     min_community = 32, max_community = 32
#' )
#' @export
sample_lfr <- function(n,
                       tau1 = 2,
                       tau2 = 1,
                       mu = 0.1,
                       average_degree,
                       max_degree,
                       min_community = NULL,
                       max_community = NULL,
                       on = 0,
                       om = 0,
                       verbose = FALSE) {
    if (missing(average_degree)) {
        stop("average_degree must be specified")
    }

    if (average_degree > n || average_degree <= 0) {
        stop("average_degree must be in the interval (0, n]")
    }

    if (missing(max_degree)) {
        stop("max_degree must be specified")
    }
    if (max_degree > n || max_degree <= 0) {
        stop("max_degree must be in the interval (0, n]")
    }

    if (!(tau1 >= 1)) {
        stop("tau1 must be at least one")
    }
    if (!(tau2 >= 1)) {
        stop("tau2 must be at least one")
    }
    if (mu < 0 || mu > 1) {
        stop("mu must be in the interval [0, 1]")
    }

    if (!is_count(on)) {
        stop("on must be a non-negative integer")
    }
    if (!is_count(om)) {
        stop("om must be a non-negative integer")
    }
    if (on > n) {
        stop("on must not be larger than n")
    }
    if (on > 0 && om < 2) {
        stop("om must be at least 2 if on > 0")
    }

    if (xor(is.null(min_community), is.null(max_community))) {
        stop("both min_community and max_community need to either be specified or not")
    }
    crange <- !is.null(min_community)
    if (crange) {
        if (!is_count(min_community) || min_community < 1) {
            stop("min_community must be a positive integer")
        }
        if (!is_count(max_community)) {
            stop("max_community must be a positive integer")
        }
        if (min_community > max_community) {
            stop("min_community must not be larger than max_community")
        }
    } else {
        min_community <- 0
        max_community <- 0
    }

    res <- benchmark(FALSE, FALSE,
        num_nodes = n,
        average_k = average_degree,
        max_degree = max_degree,
        tau = tau1,
        tau2 = tau2,
        mixing_parameter = mu,
        overlapping_nodes = on,
        overlap_membership = om,
        nmin = min_community,
        nmax = max_community,
        fixed_range = crange,
        verbose = verbose
    )

    if (res[["links_removed"]] > 0) {
        warning(
            "the degree sequence was changed slightly: ", res[["links_removed"]],
            " multiple link(s) could not be rewired and were removed",
            call. = FALSE
        )
    }

    g <- igraph::graph_from_adj_list(lapply(res[["edgelist"]], function(x) x + 1), mode = "all")
    memberships <- lapply(res[["membership"]], function(x) as.integer(x) + 1L)
    igraph::V(g)$membership <- vapply(memberships, `[`, integer(1), 1L)
    igraph::V(g)$memberships <- memberships
    g
}

is_count <- function(x) {
    is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0 && x == round(x)
}
