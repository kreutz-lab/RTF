#' Select best fit from fit with positive and fit with negative sign of RTF
#'
#' @description Select the best fit when using the positive sign for
#' the retarded transient function if the value for the
#' optimization measure is smaller than the one for the negative sign, otherwise
#' use the result for the negative sign.
#' @return Sublist which contains with the smallest value of the
#' optimization measure.
#' @param res List of two sublists for a retardad transient function
#' with positive (plus1) and negative sign (minus1). Sach containing information
#' of an optimObject as well as the value of the optimization measure
#' ('value').
#' A optimObject is a list containing input data frame with time resolved
#' data ('data'), the vector of initial guesses ('initialGuess.vec'),
#' of lower bounds ('lb.vec'), of upper bounds ('ub.vec'), vector of fixed
#' parameters ('fixed'), if log10 is applied to bounds ('takeLog10'),
#' the parameters having no negative values in initialGuess.vec, lb.vec,
#' and ub.vec ('positive.par.names'),
#' modus ('modus'), and a list of values of fitted parameters ('fitted').
#' @export selectBest
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(
#'                                   data, modus = 'RetardedTransientDynamics')
#' res.all.plusMinus <- getFittingResult(optimObject.orig)
#' res <- selectBest(res.all.plusMinus)

selectBest <- function(res) {
  resNames <- names(res)
  bestVal <- 10^10
  for (resName in resNames) {
    if (res[[resName]]$value < bestVal) {
      res.best <- res[[resName]]
      bestVal <-  res[[resName]]$value
    }
  }
  # if (res$plus1$value < res$minus1$value) {
  #   res.best <- res[["plus1"]]
  # } else {
  #   res.best <- res[["minus1"]]
  # }
  res.best
}
