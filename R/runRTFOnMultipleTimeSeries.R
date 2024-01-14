#' Run RTF on multiple times series
#'
#' @description Runs RTF on multiple columns corresponding to different times 
#' series.
#' @return List with the RTF result for each time series
#' @param df Data frame with the first column named 'time'
#' defining the different time points and all the following columns 
#' corresponding to the different time series.
#' @param modelReduction Boolean indicating if model reduction should be 
#' performed for RTF
#' @param nInitialGuesses Integer indicating the number of initial guesses for
#' RTF
#' @export runRTFOnMultipleTimeSeries
#' @examples
#' data(strasenTimeSeries)
#' df <- strasenTimeSeries[, 1:3]
#' res.lst <- runRTFOnMultipleTimeSeries(df)

runRTFOnMultipleTimeSeries <- function(df, 
                                      modelReduction = FALSE,
                                      nInitialGuesses = 200) {
  colNames <- colnames(df[2:ncol(df)])
  res.lst <- list()
  for (colIdx in 2:ncol(df)) {
    df.tmp <- data.frame(t = df$time, 
                     y = df[, colIdx])
    res <- runRTF(data = df.tmp, 
                  modelReduction = modelReduction, 
                  nInitialGuesses = nInitialGuesses)
    res.lst <- append(res.lst, list(res))
  }
  names(res.lst) <- colNames
  res.lst
}