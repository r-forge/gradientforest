`predict.gradientForest` <-
function (object, newdata, extrap=TRUE, ...)
{
  compress_extrap_z <- function(x, p, a, b){

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
  ## refactor
  ## extrap can be
  ## NA - return NA's
  ## T - extrap linearly
  ## F - cap at max
  ## 0 <= x <= 1  extrapolate with power compression. T maps to 1, F maps to 0
  ## Keep test
    if (!inherits(object,"gradientForest"))
      stop(paste("'object' must be a gradientForest object"))
  #if it is na, ok, if it is between 0 and 1, ok, otherwise, error
  if (!(is.na(extrap) | (extrap >= 0 & extrap <= 1))){
    stop("extrap parameter must be 'NA', 'TRUE', 'FALSE', or between 0 and 1")
  }
    if (missing(newdata))
        newdata <- object$X
    if(!inherits(newdata,"data.frame"))
        stop("newdata must be a data.frame")
    newnames <- names(newdata)
    if(!all(ok <- newnames %in% as.character(unique(object$res$var)))) {
        badnames <- paste(newnames[!ok], collapse=", ")
        stop(paste("the following predictors are not in the gradientForest:\n\t",badnames,sep=""))
    }
    #object, newdata and extrap are all acceptable
    out <-  newdata
    for (varX in newnames) {
      ##refactor

  ##Now, branch according to
  ## Number of elements in ci -
  ## NA -
  ## F,
  ## T or number
      ##Take the cumimp over the observed range
        ci <- cumimp(object, varX, ...)
      if(length(ci$x) == 1){
      ##If only one observation exists, assume varX is constant everywhere. The two points are used by approxfun
        if (is.na(extrap)){
          ##only newdata matching the known point are accepted
          known <- newdata[,varX] == ci$x
          out[known,varX] <- ci$y
          out[!known, varX] <- NA
          break ##break should be redundant, but makes it clear we are done for this variable
        } else {
          ##only one observation, constant everywhere
          ## linear extrapolation and capping give the same result when gradient is 0,
          ##compression is bounded by linear extrapolation and capping, so will also be constant everywhere
          out[,varX] <- ci$y
          break
        }
      } else {
        ##ci has a length > 1, so we can do extrapolation
        f <- approxfun(ci, rule = 1)
        out[,varX] <- f(newdata[,varX])
        if(!is.na(extrap)){
          ##NA case is simple, no need to extrapolate at all, just use rule = 1 in approxfun()
          ##other cases need more work
          ##refactor
          ##approxfun(rule = 2) will cap.
          ##old approach was to add a linear section to ci, then use approxfun anyway, and nothing would be outside.
          ##rather than fiddling with approxfun, caps and linear, just apply the compression function
          ##the compression function takes a gradient (which matches linear extrapolation)
          ##a compression power between 0 and 1 (given by extrap)
          ##a y offset (max(ci$y) or min(ci$y))
          ##and the distance of each x value from the known max/min
      ##get the range of x and y used in fitting
          xold <- range(ci$x)
          yold <- range(ci$y)
          if(xold[1] == xold[2]) stop(paste0("variable [", varX, "] has badly formed cumulative importance curve:\n ", ci))
          grad <- diff(yold) / diff(xold)

          ##compression algorithm only works on one side at a time.
          upper_extrap <- newdata[,varX] > xold[2]
          if(length(upper_extrap) > 0){
            out[upper_extrap, varX] <- compress_extrap_z(newdata[upper_extrap, varX] - xold[2], as.numeric(extrap), grad, yold[2])
          }

          lower_extrap <- newdata[,varX] < xold[1]
          if(length(lower_extrap) > 0){
            out[lower_extrap, varX] <- - compress_extrap_z(xold[1] - newdata[lower_extrap, varX], as.numeric(extrap), grad, -yold[1])
          }
          break
        }
        break
      }
      ##end the loop here
    }

    class(out) <- c("predict.gradientForest", "data.frame")
    out
}

