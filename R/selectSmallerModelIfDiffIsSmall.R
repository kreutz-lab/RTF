#' Select smaller model if there is no significant difference to bigger model
#'
#' @description Model reduction: Select smaller model if the difference in the
#' value of the optimization measure is not significant for Chi-square
#' significance test.
#' @return optimObject of the smaller model if for the bigger and smaller model
#' there is no significance difference in the values of the optimization
#' measure achieved through fitting.
#' An optimObject is a list containing input data frame with time
#' resolved data ('data'), the vector of initial guesses ('initialGuess.vec'),
#' of lower bounds ('lb.vec'), of upper bounds ('ub.vec'), vector of fixed
#' parameters ('fixed'), if log10 is applied to bounds ('takeLog10'),
#' the parameters having no negative values in initialGuess.vec, lb.vec,
#' and ub.vec ('positive.par.names'), modus ('modus'), and a list of values of
#' fitted parameters ('fitted').
#' @param res optimObject for which no paramater of the retarded transient
#' has been fixed prior to fitting.
#' @param res.smallerModel optimObject for which parameters are fitted prior to
#' fitting.
#' @export selectSmallerModelIfDiffIsSmall
#' @examples
#' data <- getExampleDf()
#' modus <- "timeDependent"
#' optimObject.orig <- initializeOptimObject(data, modus = modus)
#' res.all <- getInitialGuessResults(optimObject.orig)
#' optimObjectTmp <- optimObject.orig
#' optimObjectTmp$positive.par.names <-
#'                       setdiff(optimObjectTmp$positive.par.names, "tau")
#' optimObjectTmp$fixed[["tau"]] <- optimObject.orig$lb.vec[["tau"]]
#' res.tauLB <- getInitialGuessResults(optimObjectTmp)
#' res <- selectSmallerModelIfDiffIsSmall(res.all, res.tauLB)

selectSmallerModelIfDiffIsSmall <- function(res, res.smallerModel) {
  # allg:
  # chi2cdf(m2LLworseSmaller-m2LLdetterLarger,
  #         df=WievieleGefixt_NpLarge-NpSmall, x=0.95)
  difference <-  res.smallerModel$value - res$value
  # number of fitted in large model - number of fitted in small model
  df <- sum(is.na(res[["fixed"]])) - sum(is.na(res.smallerModel[["fixed"]]))
  if (stats::pchisq(difference, df = df, lower.tail = FALSE) >= 0.05)
    res <- res.smallerModel
  res
}
