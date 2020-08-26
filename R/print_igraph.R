#' @title Print graphs to terminal
#' @description Prints an igraph object to terminal (different than the standard igraph method)
#' @param x An igraph object
#' @param ... additional arguments to print (ignored)
#' @author David Schoch
#' @export

print.igraph <- function(x,...){
  maxpr  <- getOption("width")
  gattrs <- igraph::graph.attributes(x)
  vattrs <- igraph::vertex.attributes(x)
  eattrs <- igraph::edge.attributes(x)
  comps  <- igraph::components(x)
  dens   <- igraph::graph.density(x)

  #header
  if(!"name" %in% names(gattrs)){
    gname <- "Unnamed Network"
  } else{
    gname <- gattrs[["name"]]
  }

  head <- paste(toupper(gname)," ",
                c("(undirected","(directed")[igraph::is.directed(x)+1],", ",
                c("unweighted","weighted")[igraph::is.weighted(x)+1],", ",
                c("","signed, ")["sign"%in%names(eattrs)+1],
                c("one-mode","two-mode")[igraph::is.bipartite(x)+1]," ",
                "network)",sep=""
  )

  head <- paste0(head,"\n")
  delim <- paste0(rep("-",min(c(nchar(head),maxpr))),collapse="")
  delim <- paste0(delim,"\n")
  short_delim <- "---\n"

  # graph details
  n   <- paste0("Nodes: ",igraph::vcount(x))
  m   <- paste0("Edges: ",igraph::ecount(x))
  d   <- paste0("Density: ",ifelse(dens<1e-4,"<1e-4",round(dens,4)))
  cc  <- paste0("Components: ",comps$no)
  iso <- paste0("Isolates: ",sum(igraph::degree(x)==0))
  gstats <- paste(n,m,d,cc,iso,sep=", ")
  gstats <- paste0(strwrap(gstats),collapse = "\n")
  gstats <- paste0(gstats,"\n")

  # graph attrs
  gattr_str <- ""
  if(length(gattrs)>0){
    gattr_str <- apply(cbind(names(gattrs),paste0("(",substr(sapply(gattrs,mode),1,1),")")),1,paste0,collapse="")
    gattr_str <- paste("-Graph Attributes:\n ",paste0(gattr_str,collapse=", "))
    gattr_str <- paste0(gattr_str,"\n",short_delim)
  }

  #vertex attrs
  vattr_str <- ""
  for(l in seq_along(vattrs)){
    peek <- head_dot(vattrs[[l]],nchar(names(vattrs)[l])+4)
    if(l==1){
      vattr_str <- "-Vertex Attributes:\n "
      vattr_str <- paste0(vattr_str,paste0(names(vattrs)[l],"(",substr(mode(vattrs[[l]]),1,1),"): ", peek,"\n"))
    } else{
      vattr_str <- paste0(vattr_str,paste0(" ",names(vattrs)[l],"(",substr(mode(vattrs[[l]]),1,1),"): ", peek,"\n"))
    }
  }
  if(length(vattrs)>0){
    vattr_str <- paste0(vattr_str,short_delim)
  }

  eattr_str <- ""
  for(l in seq_along(eattrs)){
    peek <- head_dot(eattrs[[l]],nchar(names(eattrs)[l])+4)
    if(l==1){
      eattr_str <- "-Edge Attributes:\n "
      eattr_str <- paste0(eattr_str,paste0(names(eattrs)[l],"(",substr(mode(eattrs[[l]]),1,1),"): ", peek,"\n"))
    } else{
      eattr_str <- paste0(eattr_str,paste0(" ",names(eattrs)[l],"(",substr(mode(eattrs[[l]]),1,1),"): ", peek,"\n"))
    }
  }

  if(length(eattrs)>0){
    eattr_str <- paste0(eattr_str,short_delim)
  }
  if(igraph::ecount(x)>0){
    edges <- igraph::get.edgelist(x)[1:min(c(10,igraph::ecount(x))),]
    edges <- strwrap(paste0(apply(edges,1,paste0,collapse=c("->","--")[igraph::is.directed(x)+1]),collapse=" "))
    edges <- paste(edges,collapse = "\n")
    if(igraph::ecount(x)>10){
      edges <- c("-Edges (first 10): \n ",edges)
    } else{
      edges <- c("-Edges: \n ",edges)
    }
  } else{
    edges <- ""
  }
  cat(delim,head,delim,gstats,gattr_str,vattr_str,eattr_str,edges,sep="")


}


head_dot <- function(x,lname){
  wd <- getOption("width")
  stri <- strwrap(paste0(x,collapse = ", "),width = 0.9 * getOption("width")-lname)[1]
  paste0(stri," ...")
}
