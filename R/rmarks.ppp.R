
#' @title Generate Random `marks` for \link[spatstat.geom]{ppp} Object
#' 
#' @description
#' Generate random `marks` for \link[spatstat.geom]{ppp} object.
#' 
#' @param x a \link[spatstat.geom]{ppp} object
#' 
#' @returns
#' All functions return a \link[spatstat.geom]{ppp} object.
#' 
#' @name rmarks_ppp
NULL

#' @rdname rmarks_ppp
#' @param meanlog,sdlog see function \link[stats]{rlnorm}
#' @details
#' Function [rlnorm.ppp] generates random log-normal `marks` on a \link[spatstat.geom]{ppp} object.
#' @importFrom stats rlnorm
#' @export rlnorm.ppp
#' @export
rlnorm.ppp <- function(x, meanlog = 0, sdlog = 1) {
  x$markformat <- 'vector'
  x$marks <- rlnorm(n = x$n, meanlog = meanlog, sdlog = sdlog)
  return(x)
  #ppp(x = x$x, y = x$y, window = x$window, marks = rlnorm(n = x$n, meanlog = meanlog, sdlog = sdlog))
  # they are ?base::identical, as of 2024-11-18 # packageDate('spatstat.geom')
  # so that Tingting does not need to @importFrom spatstat.geom ppp
}

#' @rdname rmarks_ppp
#' @param size,prob,mu see function \link[stats]{rnbinom}
#' @details
#' Function [rnbinom.ppp] generates random negative-binomial `marks` on a \link[spatstat.geom]{ppp} object.
#' @importFrom stats rnbinom
#' @export rnbinom.ppp
#' @export
rnbinom.ppp <- function(x, size, prob, mu) {
  x$markformat <- 'vector'
  x$marks <- rnbinom(n = x$n, size = size, prob = prob, mu = mu)
  return(x)
}


