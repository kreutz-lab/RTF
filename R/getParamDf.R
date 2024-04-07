#' Generate data frame containing the RTF parameters for multiple time series.
#'
#' @description Generate data frame containing the RTF parameters for multiple 
#' time series from list of RTF results.
#' @return Data frame containing the RTF parameters for each time 
#' series, where the rows correspond to the different time series and 
#' the columns to the RTF parameters.
#' @param res.lst List with the RTF result for each time series 
#' @export getParamDf
#' @examples
#' data(strasen)
#' df.multipleTimeSeries <- strasen[, 1:3]
#' res.lst <- RTFOnMultipleTimeSeries(df.multipleTimeSeries)
#' param.df <- getParamDf(res.lst)

getParamDf <- function(res.lst) {
  param.lst <- list()
  for (el in res.lst) {
    param.lst <- append(param.lst, list(el[["finalParams"]]))
  }
  names(param.lst) <- names(res.lst)
  param.df <- as.data.frame(do.call(cbind, param.lst))    
  param.df <- param.df[row.names(param.df) != "sigma", ]
  
  param.df <- data.frame(t(param.df))
  param.df <- param.df[, apply(param.df, 2, stats::var, na.rm = TRUE) != 0]
  param.df
}