#' Generate plots of low-dimensional representation of multiple time series, 
#' which is based on the RTF parameters of those time series.
#'
#' @description Generates plots of low-dimensional representation of multiple 
#' time series, which is based on the RTF parameters of those time series.
#' @return List containing UMAP plot colored by meta info ('umap.metaInfo'), 
#' UMAP plot colored by cluster ID ('umap.cluster'), interactive UMAP plot 
#' colored by condition ('umap.interactive'), plot of parameter 
#' distribution ('parDistribution'), unscaled dynamics plots 
#' ('dynamics.notscaled'), scaled dynamics plots ('dynamics.scaled'), 
#' UMAP result data frame ('umap.data'), and data frame with the dynamics values 
#' ('dynamics.data')
#' @param df Data frame with the RTF parameter values. 
#' Its columns 'alpha', 'gamma', 'A', 'B', 'b', 'tau', and 'signum_TF'
#' represent the different RTF parameters. The rows correspond to the different
#' time series. 
#' @param metaInfo String of the column name with meta information (.e.g.
#' groups.
#' @param metaInfoName String of the name of the meta information.
#' @param takeRank Boolean indicating if rank should be used for UMAP instead 
#' of absolute value (Default: FALSE)
#' @param scaled Boolean indicating if values used for UMAP should be scaled 
#' for the time series in a cluster (Default: TRUE). Only relevant if 
#' takeRank = FALSE.
#' @param dimX Integer from 1 to 3 indicating the UMAP dimension plotted on 
#' the x axis (Default: 1).
#' @param dimY Integer from 1 to 3 indicating the UMAP dimension plotted 
#' on the y axis (Default: 2).
#' @param maxTime Time point up to which the dynamics should be plotted.
#' @param numClust (Optional) Integer indicating the number of clusters used 
#' for k-means clustering. If not specified, number of clusters will be 
#' determined automatically using the function NbClust::NbClust().
#' @param seed Seed for UMAP.
#' @param alphaUMAP Alpha value (between 0 and 1) of the data points in the 
#' static UMAP plot (Default: 1).
#' @param sizeUMAP Size of the data points in the static UMAP plot 
#' (Default: 1.5).
#' @param doPlotInteractiveUMAP Boolean indicating if interactive UMAP should be
#' generated (Default: FALSE).
#' @param timeSeriesDf (Only relevant if doPlotInteractiveUMAP = TRUE) 
#' Data frame with the first column corresponding to the time points 
#' and all the following columns corresponding to the different time series.
#' @param RTFmodelLst (Only relevant if doPlotInteractiveUMAP = TRUE) 
#' List with the RTF result for each time series calculated using the function 
#' getParamsFromMultipleTimeSeries().
#' @param conditions (Only relevant if doPlotInteractiveUMAP = TRUE) 
#' Vector specifying the condition for each time series. If plotLines = TRUE,
#' number of different conditions may not exceed 2.
#' @param species (Only relevant if doPlotInteractiveUMAP = TRUE) 
#' Vector with names of molecular species for each time series. Needs to be 
#' provided in plotLines = TRUE.
#' @param plotLines (Only relevant if doPlotInteractiveUMAP = TRUE) 
#' Boolean indicating if lines should be plotted between species of the same 
#' name (Default: TRUE). Can only be plotted if there are at most two time 
#' series per molecular species.
#' @param hRatio (Only relevant if doPlotInteractiveUMAP = TRUE)
#' Float between 0 and 1 indicating where subplot should be placed
#' horizontally in relation to plot width (Default: 0).
#' @param vRatio (Only relevant if doPlotInteractiveUMAP = TRUE)
#' Float between 0 and 1 indicating where subplot should be placed
#' vertically in relation to plot height (Default: 0).
#' @export plotLowDimensionalRTF
#' @importFrom dplyr %>%
#' @examples
#' data(strasenParams)
#' metaInfo <- sub("_[^_]+$", "", row.names(strasenParams))
#' metaInfoName <- "Species"
#' res <- plotLowDimensionalRTF(df = strasenParams,
#'                              metaInfo = metaInfo,
#'                              metaInfoName = metaInfoName,
#'                              maxTime = 10,
#'                              doPlotInteractiveUMAP = FALSE)

plotLowDimensionalRTF <- function(df, 
                                  metaInfo, 
                                  metaInfoName, 
                                  takeRank = FALSE,
                                  scaled = TRUE,
                                  dimX = 1,
                                  dimY = 2,
                                  maxTime = 10,
                                  numClust = NULL,
                                  seed = 111,
                                  alphaUMAP = 1,
                                  sizeUMAP = 1.5,
                                  doPlotInteractiveUMAP = FALSE,
                                  timeSeriesDf = NULL,
                                  RTFmodelLst = NULL,
                                  conditions = c(),
                                  species = c(),
                                  plotLines = TRUE,
                                  hRatio = 0,
                                  vRatio = 0
                                  ) {
    params <- colnames(df)
    df.wMetaInfo <- data.frame(df, metaInfo)
    colnames(df.wMetaInfo) <- c(colnames(df), metaInfoName)
    gg.umap.metaInfo <- plotUMAP(df = df.wMetaInfo, 
                                 groupColName = metaInfoName, 
                                 takeRank = takeRank,
                                 scaled = scaled,
                                 dimX = dimX,
                                 dimY = dimY,
                                 alpha = alphaUMAP, 
                                 size = sizeUMAP,
                                 seed = seed)
    umap.metaInfo <- gg.umap.metaInfo +
        ggplot2::ggtitle(metaInfoName) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    df.umap.metaInfo <- gg.umap.metaInfo[["data"]]
    
    # Kmeans Clustering on UMAP data
    if (is.null(numClust)) {
        clustID <- as.factor(
            NbClust::NbClust(scale(df.umap.metaInfo[, c("UMAP1", "UMAP2")]), 
                             method = 'kmeans', index = 'all')$Best.partition)
    } else {
        clustID <- as.factor(stats::kmeans(
            scale(df.umap.metaInfo[, c("UMAP1", "UMAP2")]), numClust)$cluster)
    }
    
    gg.umap.cluster <- plotUMAP(df = data.frame(cbind(df, clustID)), 
                                groupColName = "clustID", 
                                takeRank = takeRank,
                                scaled = scaled,
                                dimX = dimX,
                                dimY = dimY,
                                alpha = alphaUMAP, 
                                size = sizeUMAP,
                                seed = seed)
    umap.cluster <- gg.umap.cluster +
        ggplot2::ggtitle("Clusters") + 
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    df.umap.cluster <- gg.umap.cluster[["data"]]
    
    umap.data <- data.frame(cbind(
        df.umap.cluster[, c("UMAP1", "UMAP2", "clustID")],
        metaInfo, df))
    colnames(umap.data) <- c("UMAP1", "UMAP2", "clustID", 
                             metaInfoName, colnames(df))
    
    dynamics <- getDynamicsForClusters(df = umap.data, 
                                       maxTime = maxTime)
    dynamics.scaled <- dynamics[["plot.scaled"]] 
    dynamics.notscaled <- dynamics[["plot.notscaled"]] 
    dynamics.data <- dynamics[["df"]]
    
    umap.data.selected <- umap.data %>% 
        dplyr::select(clustID, !!params) %>%
        reshape2::melt(id.vars = c("clustID"))
    parDistribution <- plotParamDistributionForClusters(
        umap.data.selected, clusterColName = "clustID") +
        ggplot2::ggtitle("Parameters") + 
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    
    umap.interactive <- NULL
    if (doPlotInteractiveUMAP) {
        umap.interactive <- plotInteractiveUMAP(
            df = timeSeriesDf, 
            conditions = conditions, 
            takeRank = takeRank, 
            scaled = scaled,
            dimX = dimX,
            dimY = dimY,
            seed = seed,
            plotLines = plotLines,
            species = species,
            hRatio = hRatio,
            vRatio = vRatio,
            RTFmodelLst = RTFmodelLst,
            param.df = df)
        RTFmodelLst <- NULL
    }
    
    list(umap.metaInfo = umap.metaInfo, 
         umap.cluster = umap.cluster, 
         umap.interactive = umap.interactive,
         parDistribution = parDistribution,
         dynamics.notscaled = dynamics.notscaled,
         dynamics.scaled = dynamics.scaled,
         umap.data = umap.data,
         dynamics.data = dynamics.data)
}