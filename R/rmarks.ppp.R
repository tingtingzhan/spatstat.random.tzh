
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
#' Return from function [rfactor.ppp] \link[spatstat.geom]{is.multitype}.
#' 
#' @examples
#' plot(pp <- spatstat.random::rpoispp(lambda = 100))
#' 
#' plot(rlnorm.ppp(pp, sdlog = .5))
#' plot(rnbinom.ppp(pp, size = 5L, prob = .3))
#' 
#' plot(mpp <- rfactor.ppp(pp, prob = c(2,1,3)))
#' stopifnot(spatstat.geom::is.multitype(mpp))
#' plot(rfactor.ppp(pp, prob = c(2,1,3), levels = letters[1:3]))
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
#' @param size,prob,mu see function \link[stats]{rnbinom} and/or \link[base]{sample.int}
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


#' @rdname rmarks_ppp
#' @param levels see function \link[base]{factor}
#' @details
#' Function [rfactor.ppp] generates random \link[base]{factor} `marks` on a \link[spatstat.geom]{ppp} object.
#' @export rfactor.ppp
#' @export
rfactor.ppp <- function(x, prob, levels = as.character(seq_along(prob))) {
  x$markformat <- 'vector'
  x$marks <- rfactor(n = x$n, prob = prob, levels = levels)
  return(x)
}



#' @title Generate Random \link[base]{factor}
#' 
#' @description
#' ..
#' 
#' @param n \link[base]{integer} scalar
#' 
#' @param prob \link[base]{numeric} \link[base]{vector}, see function \link[base]{sample.int}
#' 
#' @param levels \link[base]{character} \link[base]{vector}, see function \link[base]{factor}
#' 
# @param ... additional parameters, currently not in use
#' 
#' @examples
#' rfactor(n = 100, prob = c(4,2,3))
#' rfactor(n = 100, prob = c(4,2,3), levels = letters[1:3])
#' @keywords internal
#' @export
rfactor <- function(n, prob, levels = as.character(seq_along(prob))) {
  ret <- sample.int(n = length(prob), size = n, prob = prob, replace = TRUE)
  attr(ret, which = 'levels') <- levels
  attr(ret, which = 'class') <- 'factor'
  return(ret)
}


