#' Run RTF on multiple times series
#'
#' @description Runs RTF on multiple columns corresponding to different time 
#' series.
#' @return List with the RTF result for each time series.
#' @param df Data frame with the first column named 'time'
#' defining the different time points and all the following columns 
#' corresponding to the different time series.
#' @param modelReduction Boolean indicating if model reduction should be 
#' performed for RTF
#' @param nInitialGuesses Integer indicating number of initial guesses 
#' (in addition to the default initial guess) used both for a signum_TF of -1 
#' and 1 (Default: 50).
#' @export RTFOnMultipleTimeSeries
#' @examples
#' data(strasen)
#' df <- strasen[, 1:3]
#' res.lst <- RTFOnMultipleTimeSeries(df)

RTFOnMultipleTimeSeries <- function(df,
                                    modelReduction = FALSE,
                                    nInitialGuesses = 50) {
  colNames <- colnames(df[2:ncol(df)])
  res.lst <- list()
  for (colIdx in 2:ncol(df)) {
    df.tmp <- data.frame(t = df$time, 
                     y = df[, colIdx])
    res <- RTF(df = df.tmp,
               nInitialGuesses = nInitialGuesses)
    if (modelReduction) res <- modelReduction(res)
    res.lst <- append(res.lst, list(res))
  }
  names(res.lst) <- colNames
  res.lst
}