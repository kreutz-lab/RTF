#' @description Select smaller model if the difference in the value of the
#' optimization measure is not significant for Chi-square significance test.
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
#' data <- scaleTimeCol(data=data)
#' modus <- "RetardedTransientDynamics"
#' plot <- FALSE
#' optimObject.orig <- initializeOptimObject(data, modus = modus)
#' res.all.plusMinus <- getFittingResult(optimObject.orig, plot = plot, titlePrefixPrefix = "fullModel_")
#' res.all <- selectPlusOrMinus(res.all.plusMinus)
#' optimObjectTmp <- optimObject.orig
#' optimObjectTmp$positive.par.names <- setdiff(optimObjectTmp$positive.par.names, "T_shift")  # because lb.vec[["T_shift"]] corresponds to -2
#' optimObjectTmp$fixed[["T_shift"]] <- optimObject.orig$lb.vec[["T_shift"]]
#' res.T_shiftLB.plusMinus <- getFittingResult(optimObjectTmp, plot = plot, titlePrefixPrefix = "TshiftFixed_")
#' res.T_shiftLB <- selectPlusOrMinus(res.T_shiftLB.plusMinus)
#' res <- selectSmallerModelIfDiffIsSmall(res.all, res.T_shiftLB)

selectSmallerModelIfDiffIsSmall <- function(res, res.smallerModel) {
  # allg: chi2cdf(m2LLworseSmaller-m2LLdetterLarger, df=WievieleGefixt_NpLarge-NpSmall, x=0.95)
  difference <-  res.smallerModel$value - res$value
  df <- sum(is.na(res[["fixed"]])) - sum(is.na(res.smallerModel[["fixed"]])) # number of fitted in large model - number of fitted in small model
  if (pchisq(difference, df = df, lower.tail = FALSE) >= 0.05) res <- res.smallerModel
  res
}
