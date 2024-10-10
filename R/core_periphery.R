#' Discrete core-periphery model
#' @description Fits a discrete core-periphery model to a given network
#' @param graph igraph object
#' @param method algorithm to use (see details)
#' @param iter number of iterations if `method=GA`
#' @param ... other parameters for GA
#' @details The function fits the data to an optimal pattern matrix with a genetic algorithm (method="GA") or a rank 1 approximation, either with degree centrality (method="rk1_dc") or eigenvector centrality (method="rk1_ec") . The rank 1 approximation is computationally far cheaper but also more experimental. Best is to compare the results from both models.
#' @return list with numeric vector with entries (k1,k2,...ki...) where ki assigns vertex i to either the core (ki=1) or periphery (ki=0), and the maximal correlation with an optimal pattern matrix
#' @references
#' Borgatti, Stephen P., and Martin G. Everett. "Models of core/periphery structures." Social networks 21.4 (2000): 375-395.
#' @author David Schoch
#' @examples
#' set.seed(121)
#' # split graphs have a perfect core-periphery structure
#' sg <- split_graph(n = 20, p = 0.3, core = 0.5)
#' core_periphery(sg)
#' @export
core_periphery <- function(graph, method = "rk1_dc", iter = 500, ...) {
    A <- igraph::as_adj(graph, type = "both", sparse = FALSE)
    if (method == "SA") {
        warning("method='SA' is deprecated, using 'GA' instead")
        method <- "GA"
    } else if (method == "GA") {
        if (!requireNamespace("GA", quietly = TRUE)) {
            stop("The package 'GA' is needed for method='GA'")
        }

        n <- nrow(A)

        res <- GA::ga("real-valued",
            fitness = cp_fct1__0, A = A,
            lower = rep(0, n), upper = rep(1, n),
            monitor = FALSE, maxiter = iter, maxFitness = 1, ...
        )
        return(list(
            vec = unname(round(res@solution[nrow(res@solution), ])),
            corr = res@fitnessValue
        ))
    } else if (method == "rk1_dc") {
        ev <- igraph::degree(graph, mode = "all", loops = FALSE)

        thresh <- unique(ev)
        optcorr <- -2

        for (tr in thresh) {
            evabs <- (ev >= tr) + 0
            E <- outer(evabs, evabs, "+")
            E[E == 1] <- NA
            E[E == 2] <- 1
            diag(E) <- NA
            if (sum(E, na.rm = TRUE) == 0) {
                next()
            }
            tmp <- suppressWarnings(graph_cor(E, A))
            if (is.na(tmp)) {
                next()
            }
            if (tmp > optcorr) {
                optperm <- evabs
                optcorr <- tmp
            }
        }
        return(list(vec = optperm, corr = optcorr))
    } else if (method == "rk1_ec") {
        ev <- round(igraph::evcent(graph)$vector, 8)

        thresh <- unique(ev)
        optcorr <- -2

        for (tr in thresh) {
            evabs <- (ev >= tr) + 0
            E <- outer(evabs, evabs, "+")
            E[E == 1] <- NA
            E[E == 2] <- 1
            diag(E) <- NA
            if (sum(E, na.rm = TRUE) == 0) {
                next()
            }
            tmp <- suppressWarnings(graph_cor(E, A))
            if (is.na(tmp)) {
                next()
            }
            if (tmp > optcorr) {
                optperm <- evabs
                optcorr <- tmp
            }
        }
        return(list(vec = optperm, corr = optcorr))
    } else {
        stop("method must be one of 'SA', 'rk1_dc', or 'rk1_ec'")
    }
}


# helper functions ----
cp_fct1__0 <- function(A, cvec) { # core=1 periphery=0
    cvec <- round(cvec)
    delta <- outer(cvec, cvec, "+")
    delta[delta == 1] <- NA
    delta[delta == 2] <- 1
    diag(delta) <- NA
    suppressWarnings(graph_cor(delta, A))
}
