

#' @title [sort_by.ppp]
#' 
#' @param x a \link[spatstat.geom]{ppp}
#' 
#' @param y \link[base]{character} scalar, either `'x'` or `'y'`
#' 
#' @param ... parameters of function \link[base]{order}
#' 
#' @keywords internal
#' @export sort_by.ppp
#' @export
sort_by.ppp <- function(x, y = c('x', 'y'), ...) {
  
  y <- match.arg(y)
  
  o <- order(x[[y]], ...)
    
  x$x <- x$x[o]
  x$y <- x$y[o]
  
  switch(x$markformat, none = {
    # do nothing
  }, vector = {
    x$marks <- x$marks[o]
  }, dataframe = {
    x$marks <- x$marks[o,]
  }, stop('shouldnt come here'))
  
  return(x)
  
}