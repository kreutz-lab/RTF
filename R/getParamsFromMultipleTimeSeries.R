#' Get RTF parameters for multiple time series
#'
#' @description Get RTF parameters for multiple time series specified in
#' data frame.
#' @return Data frame containing the RTF parameters for each time
#' series, where the rows correspond to the different time series and
#' the columns to the RTF parameters.
#' @param df Data frame with the first column named 'time' defining the
#' different time points and all the following columns corresponding to the
#' different time series.
#' @param doRTFPlots Boolean indicating if RTF plots should be generated.
#' @param fileString String the name of the created .rds file of the RTF
#' parameters should contain. If this string has length 0 or if
#' readInParamRdsFilePath is defined no .rds file will be created.
#' @param readInParamRdsFilePath if the RTF parameters to time series have
#' already been determined previously, the path of the resulting .rds file can
#' be specified  using readInParamRdsFilePath, so that the time-intensive
#' calculation of the RTF parameter does not have to be repreated a second time.
#' @param saveFolderPath Path of folder to where plots and rds files should be 
#' saved. Default: current folder.
#' @param modelReduction Boolean indicating if model reduction should be
#' performed for RTF
#' @param nInitialGuesses Integer indicating number of initial guesses 
#' (in addition to the default initial guess) used both for a signum_TF of -1 
#' and 1 (Default: 100).
#' @param plotFitsToSingleFile Boolean indicating if plots should be returned as a
#' single file.
#' @param plotFitOnly Plot fit only without additional information as provided
#' using function plotRTF().
#' @param plotAllPointsWaterfall Boolean indicating if all points should be 
#' plotted in waterfall plot (Default: FALSE). 
#' If FALSE, all values up to the median of those values are plotted.
#' @export getParamsFromMultipleTimeSeries
#' @examples
#' data(strasenTimeSeries)
#' df <- strasenTimeSeries[, 1:3]
#' param.df <- getParamsFromMultipleTimeSeries(df)

getParamsFromMultipleTimeSeries <- function(df,
                                            doRTFPlots = TRUE,
                                            fileString = "lowDimRTF",
                                            readInParamRdsFilePath = "",
                                            saveFolderPath = "",
                                            modelReduction = FALSE,
                                            nInitialGuesses = 100,
                                            plotFitsToSingleFile = TRUE,
                                            plotFitOnly = FALSE,
                                            plotAllPointsWaterfall = FALSE) {
  
  if (nchar(saveFolderPath) > 0 & !grepl("/$", saveFolderPath))
    saveFolderPath <- paste0(saveFolderPath, "/")
  
  if (nchar(readInParamRdsFilePath) > 0) {
    res.lst <- readRDS(file = readInParamRdsFilePath)
  } else {
    res.lst <- RTFOnMultipleTimeSeries(
      df = df,
      modelReduction = modelReduction,
      nInitialGuesses = nInitialGuesses
    )
  }
  
  if (nchar(fileString) > 0) {
    if (nchar(readInParamRdsFilePath) == 0)
      saveRDS(res.lst, file = paste0(saveFolderPath, fileString, ".RDS"))
    
    if (doRTFPlots)
      plotRTFOnMultipleTimeSeries(
        res.lst,
        fileString = fileString,
        saveFolderPath = saveFolderPath,
        plotFitsToSingleFile = plotFitsToSingleFile,
        plotFitOnly = plotFitOnly,
        plotAllPointsWaterfall = plotAllPointsWaterfall
      )
  }
  param.df <- getParamDf(res.lst)
  param.df
}