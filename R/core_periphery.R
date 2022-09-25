#' Discrete core-periphery model
#' @description Fits a discrete core-periphery model to a given network
#' @param graph igraph object
#' @param method algorithm to use (see details)
#' @param iter number of iterations if `method=GA`
#' @param ... other parameters for GA
#' @details The function fits the data to an optimal pattern matrix with a genetic algorithm (method="GA") or a rank 1 approximation, either with degree centrality (method="rk1_dc") or eigenvector centrality (method="rk1_ec") . The rank 1 approximation is computationally far cheaper but also more experimental. Best is to compare the results from both models.
#' @return list with numeric vector with entries (k1,k2,...ki...) where ki assigns vertex i to either the core (ki=1) or periphery (ki=0), and the maximal correlation with an optimal pattern matrix
#' @references
#' Borgatti, Stephen P., and Martin G. Everett. "Models of core/periphery structures." Social networks 21.4 (2000): 375-395.
#' @author David Schoch
#' @examples
#' set.seed(121)
#' #split graphs have a perfect core-periphery structure
#' sg <- split_graph(n = 20, p = 0.3,core = 0.5)
#' core_periphery(sg)
#' @export
core_periphery <- function(graph,method="rk1_dc",iter=500,...){
  A <- igraph::as_adj(graph,type = "both",sparse = FALSE)
  if(method=="SA"){
    warning("method='SA' is deprecated, using 'GA' instead")
    method <- "GA"
  } else if(method=="GA"){
    if(!requireNamespace("GA", quietly = TRUE)){
      stop("The package 'GA' is needed for method='GA'")
    }

    n <- nrow(A)
    cvec <- sample(c(0,1),n,replace = TRUE)
    # res <- stats::optim(par = cvec, fn = cp_fct1__0, A = A,gr = genperm,method = "SANN",
    #              control = list(maxit = iter, temp = 10, tmax = 100, trace = FALSE))
    res <- GA::ga("real-valued",fitness = cp_fct1__0,A=A,
                  lower = rep(0,n),upper = rep(1,n),
                  monitor = FALSE,maxiter = iter,maxFitness = 1,...)
    return(list(vec=unname(round(res@solution[nrow(res@solution),])),
                corr=res@fitnessValue))
  } else if(method=="rk1_dc"){
    ev <- igraph::degree(graph,mode="all",loops = FALSE)

    thresh <- unique(ev)
    optcorr <- -2

    for(tr in thresh){
      evabs <- (ev>=tr)+0
      E <- outer(evabs,evabs,"+")
      E[E==1] <- NA
      E[E==2] <- 1
      diag(E) <- NA
      if(sum(E,na.rm = TRUE)==0){
        next()
      }
      tmp <- suppressWarnings(graph_cor(E,A))
      if(is.na(tmp)){
        next()
      }
      if(tmp>optcorr){
        optperm <- evabs
        optcorr <- tmp
      }
    }
    return(list(vec = optperm,corr=optcorr))
  }else if(method=="rk1_ec"){
    ev <- round(igraph::evcent(graph)$vector,8)

    thresh <- unique(ev)
    optcorr <- -2

    for(tr in thresh){
      evabs <- (ev>=tr)+0
      E <- outer(evabs,evabs,"+")
      E[E==1] <- NA
      E[E==2] <- 1
      diag(E) <- NA
      if(sum(E,na.rm = TRUE)==0){
        next()
      }
      tmp <- suppressWarnings(graph_cor(E,A))
      if(is.na(tmp)){
        next()
      }
      if(tmp>optcorr){
        optperm <- evabs
        optcorr <- tmp
      }
    }
    return(list(vec = optperm,corr=optcorr))
  } else{
    stop("method must be one of 'SA', 'rk1_dc', or 'rk1_ec'")
  }
}


#helper functions ----
cp_fct1__0 <- function(A,cvec){ #core=1 periphery=0
  cvec <- round(cvec)
  delta <- outer(cvec,cvec,"+")
  delta[delta==1] <- NA
  delta[delta==2] <- 1
  diag(delta) <- NA
  suppressWarnings(graph_cor(delta,A))
}

# cp_fct1110 <- function(A,cvec){ #core=1 periphery=0
#   delta <- outer(cvec,cvec,function(x,y) x==1 | y==1 )
#   -sum(A*delta,na.rm = TRUE)
# }
#
# cp_fct1__0 <- function(A,cvec){ #core=1 periphery=0
#   delta <- outer(cvec,cvec,"+")
#   delta[delta==1] <- NA
#   delta[delta==2] <- 1
#   diag(delta) <- NA
#   -graph_cor(delta,A)
# }
#
# genperm <- function(A,cvec){
#   # 1=switch between values, 2= switch two nodes
#   what <- sample(1:2,1,prob = c(0.5,0.5))
#   if(what==1){
#     v <- sample(1:length(cvec),1)
#     cvec[v] <- 1-cvec[v]
#   } else if(what==2){
#     core <- which(cvec==1)
#     pery <- which(cvec==0)
#     v <- sample(core,1)
#     w <- sample(pery,1)
#     cvec[v] <- 0
#     cvec[w] <- 1
#   } else{
#
#   }
#   cvec
# }
#
#
# genperm_switch <- function(A,cvec){
#   core <- which(cvec==1)
#   pery <- which(cvec==0)
#   v <- sample(core,1)
#   w <- sample(pery,1)
#   cvec[v] <- 0
#   cvec[w] <- 1
#   cvec
# }


# Rombach/Porter
# a <- 0.99
# b <- 0.2
# iter <- 5000
# trans_vec <- trans_fct(a,b,n)
# plot(trans_vec)
# perm <- sample(1:n)
#
# res <- stats::optim(par = perm, fn = cp_rombach, A = A,trans_vec = trans_vec,gr = genpermN,method = "SANN",
#                     control = list(maxit = iter, temp = 10, tmax = 100, trace = TRUE,
#                                    REPORT = 5))
#
# plot(trans_vec[res$par],degree(g))
#
# genpermN <- function(A,trans_vec,perm){
#   uv <- sample(perm,2)
#   u <- uv[1]
#   v <- uv[2]
#   tmp <- perm[u]
#   perm[u] <- perm[v]
#   perm[v] <- tmp
#   perm
# }
#
# cp_rombach <- function(A,trans_vec,perm){
#   -sum(A*outer(trans_vec[perm],trans_vec[perm],"*"))
# }
#
# trans_fct <- function(a,b,n){
#   csize <- floor(b*n)
#   nseq <- 1:n
#   c(nseq[1:csize]*(1-a)/(2*csize),
#     (nseq[(csize+1):n]-csize)*(1-a)/(2*(n-csize))+(1+a)/2
#   )
# }


#CONCOR
# VecFun <- Vectorize( cor )
# system.time({
#   for(k in 1:10){
#     M <- outer(M_rows, M_rows, VecFun)
#     M_rows <- split(M, row(M))
#   }
# })

# https://www.nature.com/articles/srep01467.pdf
# core_periphery_profile <- function(g){
#   M <- as_adj(g,sparse=FALSE)
#   rpi <- Matrix::rowSums(M)
#   rpi <- rpi/sum(rpi)
#   M <- M/Matrix::rowSums(M)
#
#   alpha <- rep(NA,vcount(g))
#   P <- c()
#   alpha[1] <- 0
#   P <- c(P,which.min(rpi))
#   nodes <- setdiff(1:vcount(g),P)
#   for(i in 2:(vcount(g))){
#     diagP <- diag(rpi[P],nrow = length(P),ncol = length(P))
#     diagpi <- diag(rpi[nodes],nrow = length(nodes),ncol = length(nodes))
#     res <- (sum(diagP%*%M[P,P]) +
#               colSums(diagP%*%M[P,nodes]) +
#               rowSums(diagpi%*%M[nodes,P]))/
#       (sum(rpi[P])+rpi[nodes])
#
#     idx <- which(res==min(res))
#     id <- sample(idx,1)
#     P <- c(P,nodes[id])
#     alpha[i] <- min(res)
#     nodes <- nodes[-id]
#   }
#   return(alpha)
# }
