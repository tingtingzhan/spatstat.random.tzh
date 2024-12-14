
# in example by W. Joel Schneider from 
# https://stackoverflow.com/questions/59997925/how-to-generate-multivariate-normal-data-in-r
# ?MASS::mvrnorm is faster than ?mvtnorm::rmvnorm

#' @title Batch Process of Random Multivariate Normal Generation
#' 
#' @description
#' Batch process of random multivariate normal generation.
#' 
#' @param n \link[base]{integer} scalar
#' 
#' @param mu \link[base]{list} or \link[base]{data.frame},
#' each element must be a \link[base]{numeric} scalar or \link[base]{vector},
#' used as the multivariate means \eqn{\mathbf{\mu}}'s
#' 
#' @param Sigma \link[base]{list}, 
#' each element must be \link[base]{numeric}, either 
#' a scalar, a \link[base]{vector} or a \link[base]{matrix}, 
#' used as the \link[stats]{var}iance-\link[stats]{cov}ariance 
#' \link[base]{matrix}es \eqn{\Sigma}'s
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' Function [rmvnorm_] returns a \link[base]{list} of \link[base]{matrix}es.
#' 
#' @note
#' Workhorse function \link[MASS]{mvrnorm} from package \CRANpkg{MASS} is faster than `?mvtnorm::rmvnorm`.
#' 
#' @examples
#' rmvnorm_(n = 5L, mu = list(a = c(0, 3), b = c(3, 7)), Sigma = list(a = .5, b = 1.2))
#' rmvnorm_(n = 5L, mu = list(a = 0, b = c(3, 7)), Sigma = list(a = .5, b = 1.2))
#' @keywords internal
#' @importFrom MASS mvrnorm
#' @export
rmvnorm_ <- function(n, mu, Sigma, ...) {
  
  if (!setequal(names(mu), names(Sigma))) stop('`mu` and `Sigma` must have same names')
  
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
  
  return(mapply(FUN = fn1, mu = mu, Sigma = Sigma[names(mu)], MoreArgs = list(n = n), SIMPLIFY = FALSE))
  
}