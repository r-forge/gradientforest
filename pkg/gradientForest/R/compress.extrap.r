compress.extrap <- function(x, p, a, b){

assertthat::assert_that(p >=0, p<=1)
assertthat::assert_that(a >=0)
assertthat::assert_that(all(x >= 0))

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
