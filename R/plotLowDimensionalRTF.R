#' Generate plots of low-dimensional representation of multiple time series, 
#' which is based on the RTF parameters of those time series.
#'
#' @description Generates plots of low-dimensional representation of multiple 
#' time series, which is based on the RTF parameters of those time series.
#' @return List containing UMAP plot colored by meta info ('umap.metaInfo'), 
#' UMAP plot colored by cluster ID ('umap.cluster'), plot of parameter 
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
#' @param maxTime Time point up to which the dynamics should be plotted.
#' @param numClust (Optional) Integer indicating the number of clusters used 
#' for k-means clustering. If not specified, number of clusters will be 
#' determined automatically using the function NbClust::NbClust().
#' @export plotLowDimensionalRTF
#' @importFrom dplyr %>%
#' @examples
#' data(strasenParams)
#' metaInfo <- sub("_[^_]+$", "", row.names(strasenParams))
#' metaInfoName <- "Species"
#' res <- plotLowDimensionalRTF(df = strasenParams,
#'                              metaInfo = metaInfo,
#'                              metaInfoName = metaInfoName,
#'                              maxTime = 10)

plotLowDimensionalRTF <- function(df, 
                                  metaInfo, 
                                  metaInfoName, 
                                  takeRank = FALSE,
                                  scaled = TRUE,
                                  maxTime = 10,
                                  numClust = NULL) {
  params <- colnames(df)
  df.wMetaInfo <- data.frame(df, metaInfo)
  colnames(df.wMetaInfo) <- c(colnames(df), metaInfoName)
  gg.umap.metaInfo <- plotUMAP(df = df.wMetaInfo, 
                               groupColName = metaInfoName, 
                               takeRank = takeRank,
                               scaled = scaled,
                               alpha = 1, size = 1.5)
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
    clustID <- as.factor(stats::kmeans(scale(df.umap.metaInfo[, c("UMAP1", "UMAP2")]), 
                                numClust)$cluster)
  }
  
  gg.umap.cluster <- plotUMAP(df = data.frame(cbind(df, clustID)), 
                              groupColName = "clustID", 
                              takeRank = takeRank,
                              scaled = scaled,
                              alpha = 1, size = 1.5)
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
  
  list(umap.metaInfo = umap.metaInfo, 
       umap.cluster = umap.cluster, 
       parDistribution = parDistribution,
       dynamics.notscaled = dynamics.notscaled,
       dynamics.scaled = dynamics.scaled,
       umap.data = umap.data,
       dynamics.data = dynamics.data)
}