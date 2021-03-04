#' @title helper function
#' @description small functions to deal with typical network problems
#'
#' @param g igraph object
#' @name helpers
#' @return igraph object
#' @author David Schoch
NULL

#' @rdname helpers
#' @export
biggest_component <- function(g) {
  comps <- igraph::components(g,mode = "weak")
  igraph::induced_subgraph(g,which(comps$membership==which.max(comps$csize)))
}

#' @rdname helpers
#' @export
delete_isolates <- function(g) {
  igraph::delete_vertices(g,which(igraph::degree(g)==0))
}

#' #' @rdname helpers
#' graph_union <- function(glist){
#'   if(!all(sapply(glist,function(x) igraph::is.igraph(x)))){
#'     stop("glist must be a list of igraph objects")
#'   }
#'   vattrs <- unique(c(sapply(glist,igraph::vertex_attr_names)))
#'   dflist <- lapply(glist,igraph::as_data_frame,what="both")
#'   verts <- lapply(dflist,function(x) x$vertices)
#' }

#
# constraint_matrix <- function(g){
#   A <- igraph::as_adj(g,"both",sparse=TRUE)
#   AAT <- A+Matrix::t(A)
#   I <- rep(1,nrow(A))
#   x <- AAT %*% I
#   y <- as.vector(1/x)
#   P <- diag(y)%*%AAT
#   L <- (P+(A*P)%*%P)*(P+(A*P)%*%P)
#   Matrix::rowSums(L*A)
# }
