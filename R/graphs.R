#' @title two-mode network from a data.frame
#' @description Create a two-mode network from a data.frame
#'
#' @param d data.frame
#' @param type1 column name of mode 1
#' @param type2 column name of mode 2
#' @return two mode network as igraph object
#' @author David Schoch
#' @examples
#' library(igraph)
#' edges <- data.frame(mode1=1:5,mode2=letters[1:5])
#' bipartite_from_data_frame(edges,"mode1","mode2")
#' @export

bipartite_from_data_frame <- function(d,type1,type2){
  if(!type1%in%names(d)){
    stop(paste0("no column named ",type1," found in data frame"))
  }
  if(!type2%in%names(d)){
    stop(paste0("no column named ",type2," found in data frame"))
  }

  mode1 <- unique(d[[type1]])
  mode2 <- unique(d[[type2]])
  if(any(mode1%in%mode2)){
    stop("some nodes appear in both modes. Modes in a two-mode network must be distinct.")
  }
  el <- cbind(d[[type1]],d[[type2]])

  g <- igraph::graph.empty(directed=F)
  g <- igraph::add_vertices(g,nv=length(mode1),attr = list(name=mode1,type=T))
  g <- igraph::add_vertices(g,nv=length(mode2),attr = list(name=mode2,type=F))
  g <- igraph::add_edges(g,c(t(el)))
  if(igraph::any_multiple(g)){
    igraph::E(g)$weight <- 1
    g <- igraph::simplify(g,remove.multiple = TRUE,
                          remove.loops = TRUE,
                          edge.attr.comb = "sum")
  }
  g
}

#' @title Multiple networks from a single edgelist with a typed attribute
#' @description Create a list of igraph objects from an edgelist according to a type attribute
#'
#' @param d data frame.
#' @param from column name of sender. If NULL, defaults to first column.
#' @param to column of receiver. If NULL, defaults to second column.
#' @param type type attribute to split the edgelist. If NULL, defaults to third column.
#' @param weight optional column name of edge weights. Ignored if NULL.
#' @param directed logical scalar, whether or not to create a directed graph.
#' @return list of igraph objects.
#' @author David Schoch
#' @examples
#' library(igraph)
#' d <- data.frame(from=rep(c(1,2,3),3),to=rep(c(2,3,1),3),
#'                 type=rep(c("a","b","c"),each=3),weight=1:9)
#' graph_from_multi_edgelist(d,"from","to","type","weight")
#' @export

graph_from_multi_edgelist <- function(d,from = NULL, to = NULL, type = NULL,weight = NULL, directed = FALSE){
  d <- as.data.frame(d)
  if (ncol(d) < 2) {
    stop("the data frame should contain at least three columns")
  }
  dnames <- names(d)

  if(is.null(from)){
    from <- dnames[1]
  }
  if(is.null(to)){
    to <- dnames[2]
  }
  if(is.null(type)){
    type <- dnames[3]
  }
  if(is.null(weight)){
    d <- d[,c(from,to,type)]
  } else{
    d <- d[,c(from,to,weight,type)]
  }

  if(!from%in%dnames){
    stop(paste0(from," is not a valid column name"))
  }
  if(!to%in%dnames){
    stop(paste0(to," is not a valid column name"))
  }
  if(!type%in%dnames){
    stop(paste0(type," is not a valid column name"))
  }

  d_lst <- split(d,d[[type]])
  g_lst <- lapply(d_lst,function(x) igraph::graph_from_data_frame(x,directed = directed))
  g_lst
}

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
#' @description  convert igraph object to sage format to be read in SAGE
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
