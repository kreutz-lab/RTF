#' Fit function for positive AND negative sign
#'
#' @description Fits function for positive AND negative sign (signum_TF)
#' @return List of two optimObjects for a positive and a negative signum_TF
#' ('plus1' and 'minus1'). Each including fitting result of the best
#' fit ('fitted'), optimization measure value of this fit ('value'), and, if
#' plot=TRUE, the plot to this fit ('bestFit.plot') has been added.
#' @param optimObject optimObject, which is a list containing input data frame
#' with time resolved data ('data'),
#' the vector of initial guesses ('initialGuess.vec'), of lower bounds
#' ('lb.vec'),
#' of upper bounds ('ub.vec'), vector of fixed parameters ('fixed'),
#' if log10 is applied to bounds ('takeLog10'), the parameters having no
#' negative values in initialGuess.vec, lb.vec, and ub.vec
#' ('positive.par.names'),
#' modus ('modus'), and a list of values of fitted parameters ('fitted')
#' @param plot Boolean value indicating if fitting results should be plotted
#' and saved as file
#' @param titlePrefixPrefix Prefix of file names of plots, if plot  was set
#' to TRUE
#' @export getFittingResult
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(
#'             data, modus = 'RetardedTransientDynamics')
#' res.all.plusMinus <- getFittingResult(
#'             optimObject.orig, plot = TRUE, titlePrefixPrefix = "fullModel_")

getFittingResult <- function(optimObject, plot = TRUE, titlePrefixPrefix = "") {
  for (pname in names(optimObject$initialGuess.vec)) {
    # for(pname in names(optimObject$fixed)){ # was replaced because signum_TF
    # is fixed but is ot listed in lb.vec, ub.vec, and initialGuess.vec
    if (!is.na(optimObject$fixed[pname]))
      optimObject$initialGuess.vec[pname] <-
        optimObject$lb.vec[pname] <-
        optimObject$ub.vec[pname] <-
        optimObject$fixed[pname]
  }
  optimObject.orig <- optimObject
  optim.res.plus1 <- getFittingResults_fixedSignumTF(
    optimObject, signum_TF = 1, plot = plot,
    titlePrefix = paste0(titlePrefixPrefix, "signum_TFPlus1_"))
  optim.res.minus1 <- getFittingResults_fixedSignumTF(
    optimObject.orig, signum_TF = -1, plot = plot,
    titlePrefix = paste0(titlePrefixPrefix, "signum_TFMinus1_"))

  list(plus1 = optim.res.plus1, minus1 = optim.res.minus1)
  # if (optim.res.plus1$value < optim.res.minus1$value){
  #   res <- optim.res.plus1
  # } else {
  #   res <- optim.res.minus1
  # }
  # res
}
