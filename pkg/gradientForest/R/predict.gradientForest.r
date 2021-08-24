`predict.gradientForest` <-
function (object, newdata, extrap=TRUE, ...)
{
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
    if(!all(ok <- newnames %in% as.character(levels(object$res$var)))) {
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
        } else {
          ##only one observation, constant everywhere
          ## linear extrapolation and capping give the same result when gradient is 0,
          ##compression is bounded by linear extrapolation and capping, so will also be constant everywhere
          out[,varX] <- ci$y
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
          ## xold_norm <- (xold - min(xold)) / max(xold - min(xold))
          grad_norm <- diff(yold)

          ##compression algorithm only works on one side at a time.
          upper_extrap <- newdata[,varX] > xold[2]
          if(length(upper_extrap) > 0){
            ## When extrapolating, normalise x st. min(xold) == 0 and max(xold) == 1

            upper_norm <- (newdata[upper_extrap, varX] - min(xold)) / (max(xold) - min(xold))

            extrap_norm <- compress.extrap(upper_norm - 1, as.numeric(extrap), grad_norm, yold[2])
            out[upper_extrap, varX] <- extrap_norm
          }

          lower_extrap <- newdata[,varX] < xold[1]
          if(length(lower_extrap) > 0){
            lower_norm <- (newdata[lower_extrap, varX] - min(xold)) / (max(xold) - min(xold))
            extrap_norm <- - compress.extrap(-lower_norm, as.numeric(extrap), grad_norm, -yold[1])
            out[lower_extrap, varX] <- extrap_norm
          }
        }
      }
      ##end the loop here
    }

    class(out) <- c("predict.gradientForest", "data.frame")
    out
}

