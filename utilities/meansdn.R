meansdn <- function(x, varname = '') {
  if (!class(x) == "numeric") { return() }
  r.str <- vector("list",4)
  if (varname == '') { 
    r.str <- vector("list",4)
    x.str <- deparse(substitute(x)) 
  } else {
    x.str= varname
  }
  r.str[1] <- sprintf("%s: mean = %.3f   sd = %.3f   n = %d", x.str, mean(x), sd(x), length(x))
  r.str[2] <- mean(x); r.str[3] <- sd(x); r.str[4] <- length(x); r.str[]
}
