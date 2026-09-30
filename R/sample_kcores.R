#' Generate random graphs with a given coreness sequence
#' @description Similar to \link[igraph]{sample_degseq} just with \link[igraph]{coreness}
#' @param cores coreness sequence
#' @details The code is an adaption of the python code from https://github.com/ktvank/Random-Graphs-with-Prescribed-K-Core-Sequences/
#' @return igraph object of graph with the same coreness sequence as the input
#' @references
#' Van Koevering, Katherine, Austin R. Benson, and Jon Kleinberg. 2021. ‘Random Graphs with Prescribed K-Core Sequences: A New Null Model for Network Analysis’. ArXiv:2102.12604. https://doi.org/10.1145/3442381.3450001.
#' @author David Schoch
#' @examples
#' library(igraph)
#' g1 <- make_graph("Zachary")
#' kcores1 <- coreness(g1)
#' g2 <- sample_coreseq(kcores1)
#' kcores2 <- coreness(g2)
#'
#' # the sorted arrays are the same
#' all(sort(kcores1) == sort(kcores2))
#'
#' @export
sample_coreseq <- function(cores) {
    if (!is.numeric(cores) || length(cores) == 0 || anyNA(cores) ||
        any(cores < 0) || any(cores != round(cores))) {
        stop("cores must be a non-empty vector of non-negative integers")
    }
    cores <- sort(cores, decreasing = TRUE)

    if (!is_kcoreseq(cores)) {
        stop("sequence is not a valid kcore sequence")
    }
    K <- cores[1]

    indices <- lapply(0:K, function(x) which(cores == x))

    if (K == 0) {
        n0 <- length(indices[[1]])
        g <- igraph::make_empty_graph(n = n0, directed = FALSE)
        return(g)
    } else if (K == 1) {
        n0 <- length(indices[[1]])
        n1 <- length(indices[[2]])
        g <- igraph::make_empty_graph(n = n1, directed = FALSE)
        g <- igraph::add_edges(g, edges = c(t(cbind(1:(n1 - 1), 2:n1))))
        g <- igraph::add_vertices(g, nv = n0)
        return(g)
    } else if (K == 2) {
        n0 <- length(indices[[1]])
        n1 <- length(indices[[2]])
        n2 <- length(indices[[3]])
        g <- igraph::make_empty_graph(n = n2, directed = FALSE)
        g <- igraph::add_edges(g, edges = c(t(cbind(1:(n2 - 1), 2:n2))))
        g <- igraph::add_edges(g, edges = c(1, n2))
        g <- add_lower_nodes(cores, indices, g, 1)
        return(g)
    } else {
        g <- igraph::make_empty_graph(directed = FALSE)
        g <- generate_k_graph(K, sum(cores == K), g)
        g <- add_lower_nodes(cores, indices, g, K - 1)
        return(g)
    }
}

# a k-core with k > 0 needs at least k + 1 vertices
is_kcoreseq <- function(cores) {
    K <- max(cores)
    K == 0 || sum(cores == K) >= K + 1
}

add_lower_nodes <- function(cores, indices, g, k) {
    K <- cores[1]
    higher_nodes <- sort(unname(unlist(indices[(k + 2):(K + 1)])))
    g <- igraph::add_vertices(g, nv = length(indices[[k + 1]]))
    if (k == 0) {
        return(g)
    }
    all_edges <- lapply(indices[[k + 1]], function(v) {
        randv <- sample(higher_nodes, k, replace = FALSE)
        c(t(cbind(randv, v)))
    })
    g <- igraph::add_edges(g, unlist(all_edges))
    return(add_lower_nodes(cores, indices, g, k - 1))
}

generate_k_graph <- function(C, N, g) {
    if (!((C %% 2) == (N %% 2))) {
        g <- igraph::add_vertices(g, nv = N)
        g <- igraph::add_edges(g, edges = c(t(cbind(1:(N - 1), 2:N))))
        g <- igraph::add_edges(g, edges = c(1, N))
        z <- ceiling((N - C + 1) / 2)
        all_edges <- lapply(0:(N - 1), function(i) {
            start <- (i + z + 1) %% N
            stop <- (i - z) %% N
            if (stop >= start) {
                listv <- start:(stop - 1)
            } else {
                listv <- c((0:(N - 1))[0:(stop)], (0:(N - 1))[(start + 1):N])
            }
            c(t(cbind(listv, i))) + 1
        })
        g <- igraph::add_edges(g, unlist(all_edges))
        g <- igraph::simplify(g)
    } else {
        g <- generate_k_graph(C, N - 1, g)
        g <- igraph::add_vertices(g, 1)
        randv <- sample(1:(N - 1), C)
        g <- igraph::add_edges(g, c(t(cbind(randv, N))))
    }
    return(g)
}
