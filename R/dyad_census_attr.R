#' dyad census with node attributes
#'
#' @param g igraph object. should be a directed graph.
#' @param vattr name of vertex attribute to be used.
#' @return dyad census as a data.frame.
#' @details The node attribute should be integers from 1 to max(attr)
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- sample_gnp(10, 0.4, directed = TRUE)
#' V(g)$attr <- c(rep(1, 5), rep(2, 5))
#' dyad_census_attr(g, "attr")
#' @export
dyad_census_attr <- function(g, vattr) {
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
    ns <- table(attr)

    df <- igraph::as_data_frame(g, "both")
    nodes <- df$vertices
    edges <- df$edges

    edges["from_attr"] <- nodes[[vattr]][edges[["from"]]]
    edges["to_attr"] <- nodes[[vattr]][edges[["to"]]]
    edges[["reci"]] <- igraph::which_mutual(g)
    edges[["count"]] <- 1

    asym <- tryCatch(stats::aggregate(count ~ from_attr + to_attr, data = edges[!edges[["reci"]], ], FUN = sum),
        error = function(e) cbind(expand.grid(from_attr = 1:max(attr), to_attr = 1:max(attr)), count = 0)
    )
    sym <- tryCatch(stats::aggregate(count ~ from_attr + to_attr, data = edges[edges[["reci"]], ], FUN = sum),
        error = function(e) cbind(expand.grid(from_attr = 1:max(attr), to_attr = 1:max(attr)), count = 0)
    )

    idxx <- sym[["from_attr"]] == sym[["to_attr"]]
    sym[["count"]][idxx] <- sym[["count"]][idxx] / 2
    sym <- sym[sym[["from_attr"]] <= sym[["to_attr"]], ]

    idyx <- asym[["from_attr"]] > asym[["to_attr"]]
    asym2 <- asym[idyx, ]
    names(asym2) <- c(names(asym2)[2:1], "count2")
    asym <- merge(asym, asym2)
    names(asym)[3:4] <- c("asym_ab", "asym_ba")

    dc_df <- expand.grid(from_attr = 1:max(attr), to_attr = 1:max(attr))
    dc_df <- dc_df[dc_df[["from_attr"]] <= dc_df[["to_attr"]], ]
    dc_df <- merge(dc_df, asym, all.x = TRUE)
    dc_df[["asym_ab"]][is.na(dc_df[["asym_ab"]])] <- 0
    dc_df[["asym_ba"]][is.na(dc_df[["asym_ba"]])] <- 0

    dc_df <- merge(dc_df, sym, all.x = TRUE)
    names(dc_df)[5] <- "sym"
    dc_df[["sym"]][is.na(dc_df[["sym"]])] <- 0
    dc_df[["null"]] <- ifelse(dc_df[["from_attr"]] == dc_df[["to_attr"]],
        choose(ns[dc_df[["from_attr"]]], 2) - dc_df[["asym_ab"]] - dc_df[["asym_ba"]] - dc_df[["sym"]],
        ns[dc_df[["from_attr"]]] * ns[dc_df[["to_attr"]]] - dc_df[["asym_ab"]] - dc_df[["asym_ba"]] - dc_df[["sym"]]
    )
    dc_df
}
