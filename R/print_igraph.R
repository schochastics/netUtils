#' @title Print graphs to terminal
#' @description Prints an igraph object to terminal (different than the standard igraph method)
#' @param object An igraph object
#' @param ... additional arguments to print (ignored)
#' @return str does not return anything. The obvious side effect is output to the terminal.
#' @author David Schoch
#' @export
str.igraph <- function(object, ...) {
    maxpr <- getOption("width")
    gattrs <- igraph::graph.attributes(object)
    vattrs <- igraph::vertex.attributes(object)
    eattrs <- igraph::edge.attributes(object)
    comps <- igraph::components(object)
    dens <- igraph::graph.density(object)

    # header
    if (!"name" %in% names(gattrs)) {
        gname <- "Unnamed Network"
    } else {
        gname <- gattrs[["name"]]
    }

    head <- paste(toupper(gname), " ",
        c("(undirected", "(directed")[igraph::is.directed(object) + 1], ", ",
        c("unweighted", "weighted")[igraph::is.weighted(object) + 1], ", ",
        c("", "signed, ")["sign" %in% names(eattrs) + 1],
        c("one-mode", "two-mode")[igraph::is.bipartite(object) + 1], " ",
        "network)",
        sep = ""
    )

    head <- paste0(head, "\n")
    delim <- paste0(rep("-", min(c(nchar(head), maxpr))), collapse = "")
    delim <- paste0(delim, "\n")
    short_delim <- "---\n"

    # graph details
    n <- paste0("Nodes: ", igraph::vcount(object))
    m <- paste0("Edges: ", igraph::ecount(object))
    d <- paste0("Density: ", ifelse(dens < 1e-4, "<1e-4", round(dens, 4)))
    cc <- paste0("Components: ", comps$no)
    iso <- paste0("Isolates: ", sum(igraph::degree(object) == 0))
    gstats <- paste(n, m, d, cc, iso, sep = ", ")
    gstats <- paste0(strwrap(gstats), collapse = "\n")
    gstats <- paste0(gstats, "\n")

    # graph attrs
    gattr_str <- ""
    if (length(gattrs) > 0) {
        gattr_str <- apply(cbind(names(gattrs), paste0("(", substr(sapply(gattrs, mode), 1, 1), ")")), 1, paste0, collapse = "")
        gattr_str <- paste("-Graph Attributes:\n ", paste0(gattr_str, collapse = ", "))
        gattr_str <- paste0(gattr_str, "\n", short_delim)
    }

    vattr_str <- format_attr_section(vattrs, "Vertex", short_delim)
    eattr_str <- format_attr_section(eattrs, "Edge", short_delim)
    if (igraph::ecount(object) > 0) {
        edges <- igraph::get.edgelist(object)[1:min(c(10, igraph::ecount(object))), ]
        edges <- strwrap(paste0(apply(edges, 1, paste0, collapse = c("--", "->")[igraph::is.directed(object) + 1]), collapse = " "))
        edges <- paste(edges, collapse = "\n")
        if (igraph::ecount(object) > 10) {
            edges <- c("-Edges (first 10): \n ", edges)
        } else {
            edges <- c("-Edges: \n ", edges)
        }
    } else {
        edges <- ""
    }
    cat(delim, head, delim, gstats, gattr_str, vattr_str, eattr_str, edges, sep = "")
}


format_attr_section <- function(attrs, label, short_delim) {
    if (length(attrs) == 0) return("")
    lines <- vapply(seq_along(attrs), function(l) {
        peek <- head_dot(attrs[[l]], nchar(names(attrs)[l]) + 4)
        prefix <- if (l == 1) "" else " "
        paste0(prefix, names(attrs)[l], "(", substr(mode(attrs[[l]]), 1, 1), "): ", peek, "\n")
    }, character(1))
    paste0("-", label, " Attributes:\n ", paste0(lines, collapse = ""), short_delim)
}

head_dot <- function(x, lname) {
    stri <- strwrap(paste0(x, collapse = ", "), width = 0.9 * getOption("width") - lname)[1]
    paste0(stri, " ...")
}
