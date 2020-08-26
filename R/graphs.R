#' @title k partite graphs
#' @description  Create a random k-partite graph.
#'
#' @param n number of nodes
#' @param grp vector of partition sizes
#' @return kpartite graph
#' @author David Schoch
#' @export

graph_kpartite <- function(n=10,grp=c(5,5)){
  g <- igraph::graph.empty(n=n,directed=FALSE)
  cur_node <- 1
  nodes <- 1:n
  for(i in 1:(length(grp)-1)){
    add_nodes <- cur_node:(cur_node + grp[i] - 1)
    add_edges <- c(t(expand.grid(add_nodes,nodes[nodes>max(add_nodes)])))
    g <- igraph::add.edges(g,add_edges)
    cur_node <- cur_node + grp[i]
  }
  return(g)
}

#' @title convert igraph object to sage format
#' @description  convert igraph object to sage format
#'
#' @param g igraph object
#' @return sage string
#' @author David Schoch
#' @export

graph_to_sage <- function(g){
  igraph::V(g)$name <- 1:igraph::vcount(g)
  tst <- igraph::get.adjlist(g)
  gstr <- sapply(1:igraph::vcount(g),function(x)paste0(x,":","[",paste(tst[[x]],collapse = ","),"]"))
  gstr <- paste(gstr,collapse = ",")
  gstr <- paste0("g=Graph({",gstr,"})")
  gstr
}
