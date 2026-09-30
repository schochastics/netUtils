#' dyad census with node attributes
#'
#' @param g igraph object. should be a directed graph.
#' @param vattr name of vertex attribute to be used.
#' @return dyad census as a data.frame with one row per unordered pair of
#'   attribute values `from_attr <= to_attr` and columns
#'   \describe{
#'     \item{asym_ab}{asymmetric dyads with the edge pointing from `from_attr` to `to_attr`.
#'       For within-group rows (`from_attr == to_attr`) this is the total number of asymmetric dyads.}
#'     \item{asym_ba}{asymmetric dyads with the edge pointing from `to_attr` to `from_attr`.
#'       `NA` for within-group rows, where the direction is not defined.}
#'     \item{sym}{mutual dyads}
#'     \item{null}{empty dyads}
#'   }
#' @details The node attribute should be integers from 1 to max(attr).
#' Multiple edges and loops are ignored.
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- sample_gnp(10, 0.4, directed = TRUE)
#' V(g)$attr <- c(rep(1, 5), rep(2, 5))
#' dyad_census_attr(g, "attr")
#' @export
dyad_census_attr <- function(g, vattr) {
    attr <- validate_vattr(g, vattr)
    k <- max(attr)
    g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
    ns <- tabulate(attr, nbins = k)

    el <- igraph::as_edgelist(g, names = FALSE)
    mutual <- igraph::which_mutual(g)
    cross_tab <- function(idx) {
        unclass(table(
            factor(attr[el[idx, 1]], levels = seq_len(k)),
            factor(attr[el[idx, 2]], levels = seq_len(k))
        ))
    }
    # asym[a, b]: asymmetric edges a -> b; mut[a, b]: edges a -> b that are reciprocated
    asym <- cross_tab(!mutual)
    mut <- cross_tab(mutual)

    dc_df <- expand.grid(from_attr = seq_len(k), to_attr = seq_len(k))
    dc_df <- dc_df[dc_df[["from_attr"]] <= dc_df[["to_attr"]], ]
    dc_df <- dc_df[order(dc_df[["from_attr"]], dc_df[["to_attr"]]), ]
    rownames(dc_df) <- NULL
    ab <- cbind(dc_df[["from_attr"]], dc_df[["to_attr"]])
    ba <- ab[, 2:1, drop = FALSE]
    diag_row <- ab[, 1] == ab[, 2]

    dc_df[["asym_ab"]] <- asym[ab]
    dc_df[["asym_ba"]] <- ifelse(diag_row, NA_real_, asym[ba])
    # a mutual dyad contributes one edge in each direction
    dc_df[["sym"]] <- ifelse(diag_row, mut[ab] / 2, mut[ab])
    total <- ifelse(diag_row, choose(ns[ab[, 1]], 2), ns[ab[, 1]] * ns[ab[, 2]])
    dc_df[["null"]] <- total - dc_df[["asym_ab"]] - ifelse(diag_row, 0, dc_df[["asym_ba"]]) - dc_df[["sym"]]
    dc_df
}
