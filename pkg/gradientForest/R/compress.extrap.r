compress.extrap <- function(x, p, a, b){

  if(!(p >= 0 & p<=1) stop("gradientForest prediction extrap param must be in range [0,1], got [", p,"]")
  if(!(a >= 0) stop("gradientForest prediction internal error: gradient 'a' must be positive. Got [", a,"]")
  if(!(all(x >= 0) stop("gradientForest prediction internal error: Not all 'x' are positive, but internal algorithm requires it. Got [", x,"]")

if(p == 1){
    ##linear extrapolation
    z <- a * x + b
} else if (a == 0 | p == 0){
    ##capping
    z <- b
} else {
    ##curve, x^p + b, with x and intercept shifted such that dz/dx = a at x = 0
    z <- (x + (a / p) ^ (1/(p-1)) ) ^ p + b - (a / p) ^ (p / (p-1))
}

return(z)
}
