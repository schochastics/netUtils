#' triad census with node attributes
#'
#' @param g igraph object. should be a directed graph
#' @param vattr name of vertex attribute to be used
#' @return triad census with node attributes
#' @details The node attribute should be integers from 1 to max(attr).
#' The output is a named vector where the names are of the form Txxx-abc, where xxx corresponds to the standard triad census notation and "abc" are the attributes of the involved nodes. If there are more than nine attribute values, the attributes are separated by dots (Txxx-a.b.c).
#' For cyclic triads (030C) with three distinct attributes, the two orientations are counted separately: "abc" if the edges point from the smallest to the middle attribute value, "cba" otherwise.
#' Multiple edges and loops are ignored.
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
    attr <- validate_vattr(g, vattr)
    k <- max(attr)
    g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
    classes <- triad_classes(k)

    strip <- function(lst) lapply(lst, function(x) sort(unique(as.integer(x) - 1L)))
    out_list <- strip(igraph::as_adj_list(g, mode = "out"))
    nb_list <- strip(igraph::as_adj_list(g, mode = "all"))
    counts <- triadCensusCol(
        out_list, nb_list, attr - 1L, k,
        classes$lookup, length(classes$names)
    )

    # empty triads: all triads with a given color multiset minus the non-empty ones
    ns <- tabulate(attr, nbins = k)
    is_empty <- classes$type == "003"
    for (i in which(is_empty)) {
        cols <- classes$colors[[i]]
        tab <- table(cols)
        total <- prod(choose(ns[as.integer(names(tab))], as.vector(tab)))
        same <- vapply(classes$colors, identical, logical(1), cols)
        counts[i] <- total - sum(counts[same & !is_empty])
    }
    stats::setNames(counts, classes$names)
}

# Enumerate the isomorphism classes of triads with vertex colors 1..k.
# A triad is given by its 6-bit arc code over positions (p0, p1, p2), with bits
# p0->p1, p0->p2, p1->p0, p1->p2, p2->p0, p2->p1, and the colors of the positions.
# Returns the class names (in the historical order), the triad type and sorted
# colors of each class, and a lookup vector giving the 0-based class id for
# index ((code * k + c0) * k + c1) * k + c2 with 0-based colors.
triad_classes <- function(k) {
    tri_names <- c(
        "003", "012", "012", "021D", "012", "102", "021C", "111U",
        "012", "021C", "021U", "030T", "021D", "111U", "030T", "120U",
        "012", "021C", "102", "111U", "021U", "111D", "111D", "201",
        "021C", "030C", "111D", "120C", "030T", "120C", "120D", "210",
        "012", "021U", "021C", "030T", "021C", "111D", "030C", "120C",
        "102", "111D", "111D", "120D", "111U", "201", "120C", "210",
        "021D", "030T", "111U", "120U", "030T", "120D", "120C", "210",
        "111U", "120C", "201", "210", "120U", "210", "210", "300"
    )
    # arcs (from, to) in bit order
    arcs <- rbind(c(1, 2), c(1, 3), c(2, 1), c(2, 3), c(3, 1), c(3, 2))
    perms <- rbind(c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1), c(3, 1, 2), c(3, 2, 1))
    bit_of <- function(from, to) which(arcs[, 1] == from & arcs[, 2] == to)

    # all (code, c0, c1, c2) combinations, code slowest to match lookup indexing
    grid <- expand.grid(c2 = seq_len(k), c1 = seq_len(k), c0 = seq_len(k), code = 0:63)
    grid <- grid[, c("code", "c0", "c1", "c2")]
    bits <- vapply(1:6, function(b) (grid$code %/% 2^(b - 1)) %% 2, numeric(nrow(grid)))
    col <- as.matrix(grid[, c("c0", "c1", "c2")])

    # canonical form: minimum over all position permutations of (code, colors)
    canon <- rep(Inf, nrow(grid))
    for (p in seq_len(nrow(perms))) {
        pm <- perms[p, ]
        # new position i holds old position pm[i]
        new_code <- 0
        for (b in 1:6) {
            old_bit <- bit_of(pm[arcs[b, 1]], pm[arcs[b, 2]])
            new_code <- new_code + bits[, old_bit] * 2^(b - 1)
        }
        key <- ((new_code * k + col[, pm[1]] - 1) * k + col[, pm[2]] - 1) * k + col[, pm[3]] - 1
        canon <- pmin(canon, key)
    }

    # order classes as the historical implementation: by arc code, then by colors
    # with the first color varying fastest. Cyclic triads with three distinct
    # colors are ordered by orientation (ascending under code 25, else code 38).
    ord_code <- grid$code
    cyc <- which(grid$code %in% c(25, 38) & col[, 1] != col[, 2] & col[, 1] != col[, 3] & col[, 2] != col[, 3])
    ascending <- vapply(cyc, function(i) cycle_ascending(grid$code[i], col[i, ]), logical(1))
    ord_code[cyc] <- ifelse(ascending, 25, 38)
    ord <- order(ord_code, grid$c2, grid$c1, grid$c0)
    first <- ord[!duplicated(canon[ord])]
    class_id <- match(canon, canon[first])

    sep <- if (k > 9) "." else ""
    labels <- vapply(first, function(i) {
        triad_label(grid$code[i], col[i, ], sep)
    }, character(1))
    list(
        names = paste0("T", tri_names[grid$code[first] + 1], "-", labels),
        type = tri_names[grid$code[first] + 1],
        colors = lapply(first, function(i) sort(unname(col[i, ]))),
        lookup = as.integer(class_id - 1L)
    )
}

# label a colored triad: colors ordered by the automorphism orbit of their
# position, ties by color. For cyclic triads (030C) with three distinct colors
# the orientation matters: "abc" if the arcs run from the smallest to the middle
# color, "cba" otherwise.
triad_label <- function(code, cols, sep) {
    orbit_classes <- matrix(
        c(
             0,  0,  0,   2,  3,  1,   2,  1,  3,  11, 12, 12,
             3,  2,  1,   5,  5,  4,   6,  7,  8,  14, 15, 13,
             1,  2,  3,   7,  6,  8,  10, 10,  9,  23, 24, 22,
            12, 11, 12,  15, 14, 13,  24, 23, 22,  26, 26, 25,
             3,  1,  2,   6,  8,  7,   5,  4,  5,  14, 13, 15,
             9, 10, 10,  17, 18, 16,  17, 16, 18,  20, 19, 19,
             8,  7,  6,  21, 21, 21,  18, 16, 17,  30, 29, 31,
            22, 23, 24,  31, 30, 29,  28, 27, 28,  33, 32, 34,
             1,  3,  2,  10,  9, 10,   7,  8,  6,  23, 22, 24,
             8,  6,  7,  18, 17, 16,  21, 21, 21,  30, 31, 29,
             4,  5,  5,  16, 17, 18,  16, 18, 17,  27, 28, 28,
            13, 14, 15,  19, 20, 19,  29, 30, 31,  32, 33, 34,
            12, 12, 11,  24, 22, 23,  15, 13, 14,  26, 25, 26,
            22, 24, 23,  28, 28, 27,  31, 29, 30,  33, 34, 32,
            13, 15, 14,  29, 31, 30,  19, 19, 20,  32, 34, 33,
            25, 26, 26,  34, 33, 32,  34, 32, 33,  35, 35, 35
        ),
        ncol = 3,
        byrow = TRUE
    )
    orbits <- orbit_classes[code + 1, ]
    lab <- cols[order(orbits, cols)]
    if (code %in% c(25, 38) && length(unique(cols)) == 3 && !cycle_ascending(code, cols)) {
        lab <- rev(lab)
    }
    paste0(lab, collapse = sep)
}

# for a cyclic triad (code 25 or 38) with three distinct colors: does the arc
# leaving the smallest color point to the middle color?
cycle_ascending <- function(code, cols) {
    # successor of each position along the cycle
    succ <- if (code == 25) c(2, 3, 1) else c(3, 1, 2)
    cols[succ[which.min(cols)]] == stats::median(cols)
}
