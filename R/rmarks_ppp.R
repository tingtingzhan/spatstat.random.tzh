

#' @title Create Random `marks` Generation Function for \link[spatstat.geom]{ppp} Object
#' 
#' @description
#' Create random `marks` generation ***\link[base]{function}*** for \link[spatstat.geom]{ppp} object.
#' 
#' @param f \link[base]{function} of random number generation, 
#' e.g., \link[stats]{rlnorm}, \link[stats]{rnbinom}, etc.
#' Can also be the function name as a \link[base]{character} scalar.
#' 
#' @returns 
#' Function [rmarks_ppp] returns a ***\link[base]{function}***,
#' which generates random marks of a \link[spatstat.geom]{ppp} object 
#' following the distribution specified by argument `f`.
#' 
#' The returned ***\link[base]{function}*** has first parameter `x` taking an argument of a \link[spatstat.geom]{ppp} object.
#' 
#' The returned ***\link[base]{function}*** returns a \link[spatstat.geom]{ppp} object.
#' 
#' @examples
#' rmarks_ppp(rlnorm)
#' rmarks_ppp('rnbinom')
#' 
#' plot(pp <- spatstat.random::rpoispp(lambda = 100))
#' 
#' plot(pp |> rmarks_ppp(rlnorm)(sdlog = .5))
#' plot(pp |> rmarks_ppp(rnbinom)(size = 5L, prob = .3))
#' 
#' plot(mpp <- pp |> rmarks_ppp(rfactor)(prob = c(2,1,3), levels = letters[1:3]))
#' stopifnot(spatstat.geom::is.multitype(mpp))
#' @export
rmarks_ppp <- function(f) {
  
  if (is.function(f)) {
    f_ <- substitute(f)
  } else if (is.character(f)) {
    if (length(f) != 1L || is.na(f) || !nzchar(f)) stop('`f` must be a character scalar')
    if (!exists(f)) stop('function ', sQuote(f), ' does not exist')
    f_ <- as.symbol(f)
  }
  if (!is.symbol(f_)) stop('`f` must be symbol')
  
  ag <- as.list(args(f)) # accepts both 'function' and 'character'
  
  ret <- ag
  names(ret)[[1L]] <- 'x'
  
  tmp <- ag[-length(ag)]
  par_ <- names(tmp)
  tmp[] <- lapply(par_, FUN = as.symbol) # names retained!
  names(tmp)[par_ == '...'] <- '' # important!!
  tmp[[1L]] <- quote(x$n) # overwrite at the end; easiest in programming
  
  ret[[length(ret)]] <- call(name = '{', 
    call(name = '<-', quote(x$markformat), 'vector'), 
    call(name = '<-', quote(x$marks), as.call(c(list(f_), tmp))),
    call(name = 'return', quote(x))
  )
  
  #  #ppp(x = x$x, y = x$y, window = x$window, marks = rlnorm(n = x$n, meanlog = meanlog, sdlog = sdlog))
  #  # they are ?base::identical, as of 2024-11-18 # packageDate('spatstat.geom')
  #  # so that Tingting does not need to @importFrom spatstat.geom ppp
  
  return(as.function.default(ret))
  
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
#' @returns 
#' Function [rfactor] returns a \link[base]{factor}
#' 
#' @examples
#' rfactor(n = 100L, prob = c(4,2,3))
#' rfactor(n = 100L, prob = c(4,2,3), levels = letters[1:3])
#' @keywords internal
#' @export
rfactor <- function(n, prob, levels = as.character(seq_along(prob))) {
  ret <- sample.int(n = length(prob), size = n, prob = prob, replace = TRUE)
  attr(ret, which = 'levels') <- levels
  attr(ret, which = 'class') <- 'factor'
  return(ret)
}


