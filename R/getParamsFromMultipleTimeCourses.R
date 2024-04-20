#' Get RTF parameters for multiple time courses
#'
#' @description Get RTF parameters for multiple time courses specified in
#' data frame.
#' @return List of data frame containing the RTF parameters for each time
#' course, where the rows correspond to the different time courses and
#' the columns to the RTF parameters ('param.df'), and list with the RTF result 
#' for each time course ('RTFmodelLst').
#' @param df Data frame with the first column corresponding to the time points 
#' and all the following columns corresponding to the different time courses.
#' @param doRTFPlots Boolean indicating if RTF plots should be generated.
#' @param fileString String the name of the created .rds file with the RTF
#' parameters should contain. If this string has length 0 or if
#' readInParamRdsFilePath is defined no .rds file will be created.
#' @param readInParamRdsFilePath If the RTF parameters to time courses have
#' already been determined previously, the path of the resulting .rds file can
#' be specified  using readInParamRdsFilePath, so that the time-intensive
#' calculation of the RTF parameter does not have to be repreated a second time.
#' @param saveFolderPath Path of folder to where plots and rds files should be 
#' saved. Default: current folder.
#' @param modelReduction Boolean indicating if model reduction should be
#' performed for RTF
#' @param nInitialGuesses Integer indicating number of initial guesses 
#' (in addition to the default initial guess) used both for a signum_TF of -1 
#' and 1 (Default: 50).
#' @param plotFitsToSingleFile Boolean indicating if plots should be returned 
#' as a single file.
#' @param plotFitOnly Plot fit only without additional information as provided
#' using function plotRTF().
#' @param plotAllPointsWaterfall Boolean indicating if all points should be 
#' plotted in waterfall plot (Default: FALSE). 
#' If FALSE, all values up to the median of those values are plotted.
#' @export getParamsFromMultipleTimeCourses
#' @examples
#' data(strasen)
#' df <- strasen[, 1:3]
#' param.df <- getParamsFromMultipleTimeCourses(df)

getParamsFromMultipleTimeCourses <- function(df,
                                             doRTFPlots = TRUE,
                                             fileString = "lowDimRTF",
                                             readInParamRdsFilePath = "",
                                             saveFolderPath = "",
                                             modelReduction = FALSE,
                                             nInitialGuesses = 50,
                                             plotFitsToSingleFile = TRUE,
                                             plotFitOnly = FALSE,
                                             plotAllPointsWaterfall = FALSE) {
    
    if (nchar(saveFolderPath) > 0 & !grepl("/$", saveFolderPath))
        saveFolderPath <- paste0(saveFolderPath, "/")
    
    if (nchar(readInParamRdsFilePath) > 0) {
        RTFmodelLst <- readRDS(file = readInParamRdsFilePath)
    } else {
        RTFmodelLst <- RTFOnMultipleTimeCourses(
            df = df,
            modelReduction = modelReduction,
            nInitialGuesses = nInitialGuesses
        )
    }
    
    if (nchar(fileString) > 0) {
        if (nchar(readInParamRdsFilePath) == 0)
            saveRDS(RTFmodelLst, 
                    file = paste0(saveFolderPath, fileString, ".RDS"))
        
        if (doRTFPlots)
            plotRTFOnMultipleTimeCourses(
                RTFmodelLst,
                fileString = fileString,
                saveFolderPath = saveFolderPath,
                plotFitsToSingleFile = plotFitsToSingleFile,
                plotFitOnly = plotFitOnly,
                plotAllPointsWaterfall = plotAllPointsWaterfall
            )
    }
    param.df <- getParamDf(RTFmodelLst)
    list(param.df = param.df, RTFmodelLst = RTFmodelLst)
}