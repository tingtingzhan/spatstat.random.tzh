

#' @title Generates \link[spatstat.geom]{ppp} and its `marks`
#' 
#' @description ..
#' 
#' @param ... see examples, for now
#' 
#' @param win \link[spatstat.geom]{owin} window, see function \link[spatstat.random]{rMatClust}, etc.
#' 
#' @return 
#' Function [rmarks] returns a \link[spatstat.geom]{ppp} object.
#' 
#' @examples
#' r1 = rmarks(
#'  rMatClust = list(kappa=c(10,5), scale=c(.15,.06), mu=c(8,4)), 
#'  rlnorm = list(meanlog=c(3,5), sdlog=c(.4,.2))
#' ); plot(r1)
#' 
#' r2 = rmarks(
#'  rpoispp = list(lambda=c(3,6)),
#'  rlnorm = list(meanlog=c(3,5), sdlog=c(.4,.2))
#' ); plot(r2)
#' 
#' plot(spatstat.geom::superimpose(r1, r2))
#' 
#' @importFrom spatstat.random rMatClust rpoispp
#' @importFrom spatstat.geom owin superimpose
#' @importFrom stats setNames
#' @export
rmarks <- function(..., win = owin(xrange = c(-1,1), yrange = c(-1,1))) {
  
  dots <- list(...)
  dots <- dots[lengths(dots, use.names = FALSE) > 0L]
  
  par0 <- as.data.frame(unlist(dots, recursive = FALSE)) # recycle length
  
  rfn <- names(dots)
  par <- lapply(setNames(nm = rfn), FUN = function(i) { # (i = 'rMatClust')
    z <- par0[startsWith(names(par0), prefix = i)]
    names(z) <- vapply(strsplit(names(z), split = '\\.'), FUN = `[[`, 2L, FUN.VALUE = '')
    return(z) # 'data.frame'
  })
  
  n <- .row_names_info(par0, type = 2L)
  
  ret <- lapply(seq_len(n), FUN = function(j) { # (j = 1L)
    par1 <- as.list.data.frame(par[[1L]][j,])
    par2 <- as.list.data.frame(par[[2L]][j,])
    X <- do.call(what = rfn[[1L]], args = c(list(win = win), par1)) # `X$n` is randomly generated too!
    do.call(what = paste0(rfn[[2L]], '.ppp'), args = c(list(x = X), par2))
  })
  
  do.call(what = superimpose, args = ret)
  
} 



# @seealso 
# Avoid name clash \link[stats]{window}.




