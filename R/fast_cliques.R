#' @title Find Cliques, maximal or not, fast
#' @description Enumerates all (maximal) cliques using MACE. Can be faster than igraph in some circumstances
#' @param g An igraph object
#' @param what either "M" for maximal cliques or "C" for all cliques
#' @param min Numeric constant, lower limit on the size of the cliques to find. NULL means no limit, ie. it is the same as 0
#' @param max Numeric constant, upper limit on the size of the cliques to find. NULL means no limit
#' @param outfile character. If not NA, cliques are written to file
#' @details C Code downloaded from http://research.nii.ac.jp/~uno/codes.htm. Download the code and run make and then point an environment variable called MACE_PATH to the binary. See http://research.nii.ac.jp/~uno/code/mace.html for more details
#' @return (maximal) cliques as a list
#' @author David Schoch
#' @references Kazuhisa Makino, Takeaki Uno, "New Algorithms for Enumerating All Maximal Cliques", Lecture Notes in Computer Science 3111 (Proceedings of SWAT 2004), Springer, pp.260-272, 2004
#' @export
fast_cliques <- function(g,what="M",min=NULL,max=NULL,outfile = NA){
  if(!igraph::is.igraph(g)){
    stop("g must be an igraph object")
  }
  what <- match.arg(what,c("M","C"))
  what <- paste0(what,"_")
  ##########
  binary <- Sys.getenv("MACE_PATH")
  if(binary == ""){
    stop("MACE not found. Please set the MACE_PATH as environment variable.")
  }
  ##########
  fin <- tempfile()
  if(is.na(outfile)){
    fout <- tempfile()
  } else{
    fout <- outfile
  }
  # adj <- igraph::get.adjlist(g)
  adj <- as_adj_list1(g)
  adj <- lapply(1:length(adj),function(x){
    neigh <- adj[[x]]
    neigh[neigh>x]-1
  })
  writeLines(sapply(adj,paste,collapse=","),fin)
  if(!is.null(min)){
    what <- paste(what,"-l",min)
  }
  if(!is.null(max)){
    what <- paste(what,"-u",max)
  }
  cmd <- paste(binary,what,fin,fout)
  a <- system(cmd,intern = TRUE)

  unlink(fin)
  if(is.na(outfile)){
    clix <- readLines(fout)
    lst <- sapply(clix,function(x) strsplit(x,split=" "))
    lst <- lapply(lst,function(x) as.integer(x)+1)
    lst <- unname(lst)
    unlink(fout)
    return(lst)
  } else{
    invisible(g)
  }
}
