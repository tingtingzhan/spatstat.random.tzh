
# in example by W. Joel Schneider from 
# https://stackoverflow.com/questions/59997925/how-to-generate-multivariate-normal-data-in-r
# ?MASS::mvrnorm is faster than ?mvtnorm::rmvnorm

#' @title Batch Process of Random Multivariate Normal Generation
#' 
#' @param n \link[base]{integer} scalar
#' 
#' @param mu \link[base]{list} or \link[base]{data.frame}. 
#' Each element must be a \link[base]{numeric} scalar or \link[base]{vector}
#' 
#' @param Sigma \link[base]{list} or \link[base]{data.frame}.
#' Each element must be \link[base]{numeric}, either 
#' a scalar, a \link[base]{vector} or a \link[base]{matrix}.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @examples
#' rmvnorm_(n = 5L, mu = list(a = c(0, 3), b = c(3, 7)), Sigma = list(a = .5, b = 1.2))
#' rmvnorm_(n = 5L, mu = list(a = 0, b = c(3, 7)), Sigma = list(a = .5, b = 1.2))
#' @keywords internal
#' @importFrom MASS mvrnorm
#' @export
rmvnorm_ <- function(n, mu, Sigma, ...) {
  
  if (!setequal(names(mu), names(Sigma))) stop('`mu` and `Sigma` must have same names')
  nm <- names(mu)
  Sigma <- Sigma[nm]
  
  fn1 <- function(n, mu, Sigma) {
    d <- length(mu)
    if (is.matrix(Sigma)) {
      if (any(dim(Sigma) != d)) stop('`dim(Sigma)` not same with length(mu)')
    } else {
      nsd <- length(Sigma)
      if (nsd == 1L) Sigma <- rep(Sigma, times = d)
      if (length(Sigma) != d) stop('`length(Sigma)` not same with length(mu)')
      Sigma <- diag(x = Sigma, nrow = d, ncol = d)
    }
    mvrnorm(n = n, mu = mu, Sigma = Sigma) # matrix of `n`-by-`d`
  }
  
  return(mapply(FUN = fn1, mu = mu, Sigma = Sigma, MoreArgs = list(n = n), SIMPLIFY = FALSE))
  
}