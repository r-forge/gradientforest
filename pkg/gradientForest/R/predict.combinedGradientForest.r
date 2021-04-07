`predict.combinedGradientForest` <-
function (object, newdata, extrap=TRUE, ...)
{
  ## extrap can be
  ## NA - return NA's
  ## T - extrap linearly
  ## F - cap at max
  ## 0 <= x <= 1  extrapolate with power compression. T maps to 1, F maps to 0
    if (!inherits(object,"combinedGradientForest"))
      stop(paste("'object' must be a combinedGradientForest object"))
    ## linfun <- function(xold,yold,xnew)
    ##     yold[1] + (xnew-xold[1])*diff(yold)/diff(xold)
  #if it is na, ok, if it is between 0 and 1, ok, otherwise, error
  if (!(is.na(extrap) | (extrap >= 0 & extrap <= 1))){
    stop("extrap parameter must be 'NA', 'TRUE', 'FALSE', or between 0 and 1")
  }
    if (missing(newdata)) {
      n.gf <- length(object$nspec)
      n.preds <- sapply(object$gf.names, length)
      use.preds <- n.gf == n.preds
      use.preds.names <- names(n.preds[use.preds])
      if(!all(use.preds))
        warning("predict.combinedGradientForest called with missing newdata. By default, fitting data X is used as newdata, but not all gradientForest objects in the combinedGradientForest contain the same sets of predictors. Using the common subset of predictors from X.")
      newdata <- lapply(object$X, function(X) X[use.preds.names])
      newdata <- do.call("rbind",newdata)
      if(ncol(newdata) == 0)
        stop("predict.combinedGradientForest called with missing newdata, but no predictor appears in all gradientForest objects that make up the combinedGradientForest object. No meaningful prediction can be done, please provide newdata, eg. newdata = gf.combined$X[[1]].")
    }
    if(!inherits(newdata,"data.frame"))
        stop("newdata must be a data.frame")
    newnames <- names(newdata)
    if(!all(ok <- newnames %in% names(object$gf.names))) {
        badnames <- paste(newnames[!ok], collapse=", ")
        stop(paste("the following predictors are not in the gradientForest:\n\t",badnames,sep=""))
    }

    out <-  newdata
    for (varX in newnames) {
        ci <- cumimp(object, varX, ...)
        if(length(ci$x) == 1){
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
        f <- approxfun(ci, rule = 1)
        out[,varX] <- f(newdata[,varX])
        if(!is.na(extrap)){
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
    }
    class(out) <- c("predict.gradientForest", "data.frame")
    out
}
