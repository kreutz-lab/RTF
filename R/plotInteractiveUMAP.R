#' Generates dynamic UMAP plot based on the RTF parameters for multiple time 
#' series
#'
#' @description Generates dynamic UMAP plot based on the RTF parameters for 
#' multiple time series, where each point corresponds to a single time series. 
#' By hovering over a point the corresponding time-resolved behavior is 
#' displayed in an additional smaller subplot.
#' @return Plotly object of the interactive UMAP plot.
#' @param df Data frame with the first column corresponding to the time points 
#' and all the following columns corresponding to the different time series.
#' @param fileString String of the name of the created .rds file with the RTF 
#' results.
#' @param conditions Vector specifying the condition for each time series. 
#' Needs to have the same length as the number of rows of df.
#' @param nInitialGuesses Integer indicating number of initial guesses 
#' (in addition to the default initial guess) used both for a signum_TF of -1 
#' and 1 (Default: 50).
#' @param takeRank Boolean indicating if rank should be used for UMAP instead 
#' of absolute value (Default: FALSE).
#' @param scaled Boolean indicating if values used for UMAP should be scaled 
#' for the time series in a cluster (Default: TRUE). Only relevant if 
#' takeRank = FALSE.
#' @param dimX Integer from 1 to 3 indicating the UMAP dimension plotted on 
#' the x axis (Default: 1).
#' @param dimY Integer from 1 to 3 indicating the UMAP dimension plotted 
#' on the y axis (Default: 2).
#' @param seed Seed for UMAP.
#' @param plotLines Boolean indicating if lines should be plotted between 
#' species of the same name (Default: TRUE). Can only be plotted if there are at
#' most two time series per species.
#' @param species Vector with names of molecular species for each time series. 
#' Needs to have the same length as the number of rows of df. 
#' Needs to be provided in plotLines = TRUE.
#' @param hRatio Float between 0 and 1 indicating where subplot should be placed
#' horizontally in relation to plot width (Default: 0).
#' @param vRatio Float between 0 and 1 indicating where subplot should be placed
#' vertically in relation to plot height (Default: 0).
#' @param RTFmodelLst (Optional) List with the RTF result for each time series. 
#' If this has already been calculated using the function 
#' getParamsFromMultipleTimeSeries() it can be provided here to save time, if 
#' param.df is provided as well.
#' @param param.df (Optional) data frame containing the RTF parameters for 
#' each time series, where the rows correspond to the different time series and
#' the columns to the RTF parameters.
#' If this has already been calculated using the function 
#' getParamsFromMultipleTimeSeries() it can be provided here to save time, if 
#' RTFmodelLst is provided as well.
#' @export plotInteractiveUMAP
#' @examples
#' timeSeries <- almaden
#' RTFmodelLst <- almadenModelLst
#' param.df <- almadenParams
#' colNames <- colnames(timeSeries[2:ncol(timeSeries)])
#' plt <- plotInteractiveUMAP(df = timeSeries, 
#'                            fileString = "almadenInteractiveUMAP", 
#'                            conditions = gsub(".*_", "", colNames), 
#'                            species = sub("_[^_]+$", "", colNames),
#'                            hRatio = 0.4,
#'                            RTFmodelLst = RTFmodelLst,
#'                            param.df = param.df)

plotInteractiveUMAP <- function(df, 
                                fileString = "interactiveUMAP", 
                                conditions = c(), 
                                nInitialGuesses = 50,
                                takeRank = FALSE, 
                                scaled = TRUE,
                                dimX = 1,
                                dimY = 2,
                                seed = 111,
                                plotLines = TRUE,
                                species = c(),
                                hRatio = 0,
                                vRatio = 0,
                                RTFmodelLst = NULL,
                                param.df = NULL) {
    
    if (is.null(RTFmodelLst) | is.null(param.df)) {
        # Besides returning the fitted parameters, this function also generates
        # a RDS with the final models, which is needed later on.
        params.lst <- getParamsFromMultipleTimeSeries(
            df = df,
            fileString = fileString,
            saveFolderPath = getwd(),
            nInitialGuesses = nInitialGuesses
        )
        param.df <- params.lst[["param.df"]]
        RTFmodelLst <- params.lst[["RTFmodelLst"]]
    }
    
    param.df.wConditions <- data.frame(param.df, conditions)
    colnames(param.df.wConditions) <- c(colnames(param.df), "conditions")
    gg.umap.conditions <- plotUMAP(df = param.df.wConditions, 
                                   groupColName = "conditions", 
                                   takeRank = takeRank,
                                   scaled = scaled,
                                   dimX = dimX,
                                   dimY = dimY,
                                   seed = seed)
    
    df.umap.conditions <- gg.umap.conditions[["data"]]
    
    ID <- row.names(df.umap.conditions)
    plt <- plotInteractive2DPlot(dim1Vec = df.umap.conditions$UMAP1,
                                 dim2Vec = df.umap.conditions$UMAP2,
                                 ID = ID,
                                 species = species,
                                 RTFmodelLst = RTFmodelLst,
                                 metaInfo = conditions,
                                 plotLines = plotLines,
                                 hRatio = hRatio,
                                 vRatio = vRatio)
    plt
}