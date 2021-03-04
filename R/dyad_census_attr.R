#' dyad census with node attributes
#'
#' @param g igraph object. should be a directed graph.
#' @param vattr name of vertex attribute to be used.
#' @return dyad census with node attributes.
#' @details The node attribute should be integers from 1 to max(attr). Currently only works for 2
#' @author David Schoch
#' @export

dyad_census_attr <- function(g,vattr){
  if(!igraph::is_directed(g)){
    stop("g must be a directed graph")
  }
  if(!vattr%in%igraph::vertex_attr_names(g)){
    stop(paste0("there is no vertex attribute called ",vattr))
  }
  attr <- igraph::get.vertex.attribute(g,vattr)
  if(!all(is.numeric(attr))){
    stop("vertex attribute must be numeric ")
  }
  A <- igraph::as_adj(g)
  # attrcomb <- as.matrix(expand.grid(1:max(attr),1:max(attr)))
  # codes <- apply(attrcomb,1,paste0,collapse="")
  # types <- c("asym","mut","null")
  types <- c("asym-11", "mut-11", "null-11", "asym-12", "mut-12", "null-12", "asym-22", "mut-22", "null-22")
  stop("not implemented yet")
}
