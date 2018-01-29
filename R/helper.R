## mylag function
mylag <- function(x, lag) {
  n <- length(x)
  xnew <- x[n-(lag-1)]
  return(xnew)
}
