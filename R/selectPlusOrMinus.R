#' @description Select the best fit when using the positive sign for
#' the retarded transient function if the value for the
#' optimization measure is smaller than the one for the negative sign, otherwise
#' use teh result for the negative sign.
#' @return Sublist which contains with the smallest value of the
#' optimization measure.
#' @param nInitialGuesses Integer corresponding to number of initial guesses
#' @param res List of two sublists for a retardad transient function
#' with positive (plus1) and negative sign (minus1). Sach containing information
#' of an optimObject as well as the value of the optimization measure
#' ('value') and the plots to the best fit ('bestFit.plot').
#' A optimObject is a list containing input data frame with time resolved
#' data ('data'), the vector of initial guesses ('initialGuess.vec'),
#' of lower bounds ('lb.vec'), of upper bounds ('ub.vec'), vector of fixed
#' parameters ('fixed'), if log10 is applied to bounds ('takeLog10'),
#' the parameters having no negative values in initialGuess.vec, lb.vec,
#' and ub.vec ('positive.par.names'),
#' modus ('modus'), and a list of values of fitted parameters ('fitted').
#' @export selectPlusOrMinus
#' @examples
#' data <- getExampleDf()
#' data <- scaleTimeCol(data=data)
#' optimObject.orig <- initializeOptimObject(
#'                                   data, modus = 'RetardedTransientDynamics')
#' res.all.plusMinus <- getFittingResult(
#'             optimObject.orig, plot = FALSE, titlePrefixPrefix = "fullModel_")
#' res <- selectPlusOrMinus(res.all.plusMinus)

selectPlusOrMinus <- function(res) {
  if (res$plus1$value < res$minus1$value) {
    res <- res[["plus1"]]
  } else {
    res <- res[["minus1"]]
  }
  res
}
