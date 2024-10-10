#' triad census with node attributes
#'
#' @param g igraph object. should be a directed graph
#' @param vattr name of vertex attribute to be used
#' @return triad census with node attributes
#' @details The node attribute should be integers from 1 to max(attr).
#' The output is a named vector where the names are of the form Txxx-abc, where xxx corresponds to the standard triad census notation and "abc" are the attributes of the involved nodes.
#'
#' The implemented algorithm is comparable to the algorithm in Lienert et al.
#' @references Lienert, J., Koehly, L., Reed-Tsochas, F., & Marcum, C. S. (2019). An efficient counting method for the colored triad census. Social Networks, 58, 136-142.
#' @author David Schoch
#' @examples
#' library(igraph)
#' set.seed(112)
#' g <- sample_gnp(20, p = 0.3, directed = TRUE)
#' # add a vertex attribute
#' V(g)$type <- rep(1:2, each = 10)
#' triad_census_attr(g, "type")
#' @export
triad_census_attr <- function(g, vattr) {
    if (!igraph::is_directed(g)) {
        stop("g must be a directed graph")
    }
    if (!vattr %in% igraph::vertex_attr_names(g)) {
        stop(paste0("there is no vertex attribute called ", vattr))
    }
    attr <- igraph::vertex_attr(g, vattr)
    if (!all(is.numeric(attr))) {
        stop("vertex attribute must be numeric ")
    }
    A <- igraph::as_adj(g)

    orbit_classes <- matrix(c(
        0, 0, 0, 2, 3, 1, 2, 1, 3, 11, 12, 12, 3, 2, 1, 5, 5, 4, 6, 7, 8, 14, 15, 13,
        1, 2, 3, 7, 6, 8, 10, 10, 9, 23, 24, 22, 12, 11, 12, 15, 14, 13, 24, 23, 22,
        26, 26, 25, 3, 1, 2, 6, 8, 7, 5, 4, 5, 14, 13, 15, 9, 10, 10, 17, 18, 16,
        17, 16, 18, 20, 19, 19, 8, 7, 6, 21, 21, 21, 18, 16, 17, 30, 29, 31,
        22, 23, 24, 31, 30, 29, 28, 27, 28, 33, 32, 34, 1, 3, 2, 10, 9, 10, 7, 8, 6,
        23, 22, 24, 8, 6, 7, 18, 17, 16, 21, 21, 21, 30, 31, 29, 4, 5, 5, 16, 17, 18,
        16, 18, 17, 27, 28, 28, 13, 14, 15, 19, 20, 19, 29, 30, 31, 32, 33, 34,
        12, 12, 11, 24, 22, 23, 15, 13, 14, 26, 25, 26, 22, 24, 23, 28, 28, 27,
        31, 29, 30, 33, 34, 32, 13, 15, 14, 29, 31, 30, 19, 19, 20, 32, 34, 33,
        25, 26, 26, 34, 33, 32, 34, 32, 33, 35, 35, 35
    ), ncol = 3, byrow = T)
    tri_names <- c(
        "003", "012", "012", "021D", "012", "102", "021C", "111U", "012", "021C", "021U", "030T",
        "021D", "111U", "030T", "120U", "012", "021C", "102", "111U", "021U", "111D", "111D", "201",
        "021C", "030C", "111D", "120C", "030T", "120C", "120D", "210", "012", "021U", "021C", "030T",
        "021C", "111D", "030C", "120C", "102", "111D", "111D", "120D", "111U", "201", "120C", "210",
        "021D", "030T", "111U", "120U", "030T", "120D", "120C", "210", "111U", "120C", "201", "210",
        "120U", "210", "210", "300"
    )
    tri_names <- paste0("T", tri_names)
    attrcomb <- as.matrix(expand.grid(1:max(attr), 1:max(attr), 1:max(attr)))

    code_sort <- function(vorb, attr, i) {
        idx <- order(vorb, attr)
        vorbs <- vorb[idx]
        attrs <- attr[idx]
        if (all(vorbs == 21) && length(unique(attr)) == 3) {
            if (i == 39) {
                attrs <- rev(attrs)
            }
        }
        return(paste0(paste0(vorbs, collapse = ""), "-", paste0(attrs, collapse = "")))
    }

    triad_names <- c()
    triad_names1 <- c()
    for (i in seq_len(nrow(orbit_classes))) {
        for (j in seq_len(nrow(attrcomb))) {
            new <- code_sort(orbit_classes[i, ], attrcomb[j, ], i)
            triad_names <- unique(c(triad_names, new))
            triad_names1 <- unique(c(triad_names1, paste0(tri_names[i], "-", gsub(".*-", "", new))))
        }
    }
    triads <- rep(0L, length(triad_names))
    names(triads) <- triad_names
    triads <- triadCensusCol(A, attr, orbit_classes, triads)
    names(triads) <- triad_names1
    triads
}
