summary.slmfit <- function(object, ...) {

  predictornames <- object$PredictorNames
  NAvec <- rep(NA, times = length(predictornames))

  regcoefs <- object$CoefficientEsts
  regvar <- object$BetaCov
  p <- length(regcoefs)
  n <- object$SampSize

  sereg <- sqrt(diag(regvar))

  tvec <- NAvec
  tvec <- regcoefs / sereg
  pvec <- NAvec
  pvec <- round(100000 * (1 - pt(abs(regcoefs / sereg),
    df = n - p)) * 2) / 100000

  fixed.eff.est <- data.frame(##FactorLevel = predictornames,
    Estimate = regcoefs,
    std.err = sereg, t.value = tvec, prob.t = pvec)
  fixed.effects.estimates = fixed.eff.est

 ## if(any(rownames(b.hat) %in% effnames == FALSE)) {
    ## dataXY issue
  ##  stop(cat("glmssn has computed estimates for",rownames(b.hat),"but the summary command expects estimates for",effnames,collapse=" "))
##  }
  outpt = list(fixed.effects.estimates = fixed.effects.estimates)
  class("summary.slmfit")
  return(outpt)

}
