#' Generate low-dimensional representation of multiple time series based on 
#' their RTF parameters
#' @description Generates low-dimensional representation of multiple time series
#'  based on their RTF parameters.
#' @return List with data frame of RTF parameters for each time series
#' ('param.data'), data frame of UMAP results ('umap.data'), data frame of
#' dynamics per cluster ('dynamics.data'), and ggplot2 plots of the results 
#' ('plots'). Function also saves those objects to files if saveToFile=TRUE.
#' @param df Data frame with the first column named 'time'
#' defining the different time points and all the following columns
#' corresponding to the different time series.
#' @param metaInfo String of the column name with meta information (.e.g.
#' groups.
#' @param metaInfoName String of the name of the meta information.
#' @param takeRank Boolean indicating if rank should be used for UMAP instead 
#' of absolute value (Default: TRUE)
#' @param scaled Boolean indicating if values used for UMAP should be scaled 
#' for the time series in a cluster (Default: TRUE). Only relevant if 
#' takeRank = FALSE.
#' @param metaInfoSecondRow Boolean indicating if second row contains the meta
#' info.
#' @param fileString String the output files should contain.
#' @param plotItemHeight Integer indicating height of each plot type in the
#' output plot.
#' @param plotWidth Integer indicating width of output plot.
#' @param readInParamRdsFilePath if the RTF parameters to time series have
#' already been determined previously, the path of the resulting .rds file can
#' be specified  using readInParamRdsFilePath, so that the time-intensive
#' calculation of the RTF parameter does not have to be repreated a second time.
#' @param modelReduction Boolean indicating if model reduction should be
#' performed for RTF
#' @param nInitialGuesses Integer indicating number of initial guesses 
#' (in addition to the default initial guess) used both for a signum_TF of -1 
#' and 1 (Default: 50).
#' @param saveToFile Boolean indicating if results should be saved to file
#' @param numClust (Optional) Number of clusters. If not specified, number of
#' clusters will be detrmined automatically using the function
#' NbClust::NbClust().
#' @param param.data (Optional) If a data frame with the RTF parameter values 
#' already available, you can provide it here for saving time.
#' Its columns 'alpha', 'gamma', 'A', 'B', 'b', 'tau', and 'signum_TF'
#' represent the different RTF parameters. The rows correspond to the different
#' time series. 
#' @param plotFitsToSingleFile Boolean indicating if plots should be returned as a
#' single file.
#' @param plotFitOnly Plot fit only without additional information as provided
#' using function plotRTF().
#' @param plotAllPointsWaterfall Boolean indicating if all points should be 
#' plotted in waterfall plot (Default: FALSE). 
#' If FALSE, all values up to the median of those values are plotted.
#' @export lowDimensionalRTF
#' @examples
#' \dontrun{
#' data(strasenTimeSeries)
#' df <- strasenTimeSeries[, 1:20]
#' colNames <- colnames(df[2:ncol(df)])
#' metaInfo <- species <- sub("_[^_]+$", "", colNames)
#' res <- lowDimensionalRTF(df = df,
#'                          metaInfo = metaInfo,
#'                          fileString = "strasen_subset",
#'                          metaInfoName = "Species", saveToFile = FALSE)
#' ggplot2::ggsave(filename = "test.pdf", plot = res[["plots"]],
#'                width = 10, height = 30)
#' }

lowDimensionalRTF <- function(df,
                              metaInfo = c(),
                              metaInfoName = "Species",
                              takeRank = TRUE,
                              scaled = TRUE,
                              metaInfoSecondRow = FALSE,
                              fileString = "lowDimRTF",
                              plotItemHeight = 9,
                              plotWidth = 22,
                              readInParamRdsFilePath = "",
                              modelReduction = FALSE,
                              nInitialGuesses = 50,
                              saveToFile = TRUE,
                              numClust = NULL,
                              param.data = NULL,
                              plotFitsToSingleFile = TRUE,
                              plotFitOnly = FALSE,
                              plotAllPointsWaterfall = FALSE) {
  if (metaInfoSecondRow & length(metaInfo) == 0) {
    metaInfo <- as.character(unlist(df[1, 2:ncol(df)]))
    df <- df[-1,]
  } else if (length(metaInfo) == 0) {
    metaInfo <- rep("Group", ncol(df) - 1)
  }
  
  if (length(metaInfo) != ncol(df) - 1) {
    warning("Please provide meta information of size #columns-1 .")
  }
  
  if (is.null(param.data)) {
    param.data <- getParamsFromMultipleTimeSeries(
      df = df,
      fileString = fileString,
      readInParamRdsFilePath = readInParamRdsFilePath,
      modelReduction = modelReduction,
      nInitialGuesses = nInitialGuesses,
      plotFitsToSingleFile = plotFitsToSingleFile,
      plotFitOnly = plotFitOnly,
      plotAllPointsWaterfall = plotAllPointsWaterfall
    )
  }
  
  plotsLst <- plotLowDimensionalRTF(
    df = param.data,
    metaInfo = metaInfo,
    metaInfoName = metaInfoName,
    takeRank = takeRank,
    scaled = scaled,
    maxTime = max(df$time),
    numClust = numClust
  )
  
  numCluster <- length(unique(plotsLst[["dynamics.data"]]$cluster))
  
  sumHeights <- (plotItemHeight * 3) + 
    (2 * (ceiling(numCluster / 3)) * plotItemHeight)
  
  plotsCombined <- patchwork::wrap_plots(
    plotsLst[["umap.metaInfo"]],
    plotsLst[["umap.cluster"]],
    plotsLst[["parDistribution"]],
    patchwork::wrap_elements(
      plotsLst[["dynamics.notscaled"]] +
        patchwork::plot_annotation(
          title = "Dynamics (unscaled)",
          theme = ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"))
        )
    ),
    patchwork::wrap_elements(
      plotsLst[["dynamics.scaled"]] +
        patchwork::plot_annotation(
          title = "Dynamics (scaled)",
          theme = ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"))
        )
    ),
    heights = c(rep(plotItemHeight / sumHeights, 3),
                rep(((ceiling(numCluster / 3)) * plotItemHeight
                ) /
                  sumHeights, 2))
  )
  
  if (saveToFile) {
    ggplot2::ggsave(
      paste0("plots_", fileString, ".pdf"),
      plot = plotsCombined,
      width = plotWidth,
      height = sumHeights,
      limitsize = FALSE
    )
    
    utils::write.csv(param.data,
                     file = paste0("param_", fileString, ".csv"),
                     row.names = TRUE)
    utils::write.csv(plotsLst[["umap.data"]],
                     file = paste0("umap_", fileString, ".csv"),
                     row.names = TRUE)
    utils::write.csv(
      plotsLst[["dynamics.data"]],
      file = paste0("dynamics_", fileString, ".csv"),
      row.names = FALSE
    )
  }
  
  list(
    param.data = param.data,
    umap.data = plotsLst[["umap.data"]],
    dynamics.data = plotsLst[["dynamics.data"]],
    plots = plotsCombined
  )
}
