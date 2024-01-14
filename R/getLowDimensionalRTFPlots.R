#' Generate plots of low dimensional representation of RTF parameters
#'
#' @description Generates plots of low dimensional representation of RTF 
#' parameters
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
#' @param maxTime Time point up to which the dynamics should be plotted.
#' @export getLowDimensionalRTFPlots
#' @importFrom dplyr %>%
#' @examples
#' data(strasenParams)
#' metaInfo <- sub("_[^_]+$", "", row.names(strasenParams))
#' metaInfoName <- "species"
#' res <- getLowDimensionalRTFPlots(df = strasenParams,
#'                                 metaInfo = metaInfo,
#'                                 metaInfoName = metaInfoName,
#'                                 maxTime = 10)

getLowDimensionalRTFPlots <- function(df, metaInfo, 
                                      metaInfoName, maxTime = 10) {
  params <- colnames(df)
  df.wMetaInfo <- data.frame(df, metaInfo)
  colnames(df.wMetaInfo) <- c(colnames(df), metaInfoName)
  gg.umap.metaInfo <- getUMAPplot(df.wMetaInfo, 
                                   groupColName = metaInfoName, 
                                   alpha = 1, size = 1.5)
  umap.metaInfo <- gg.umap.metaInfo +
    ggplot2::ggtitle(metaInfoName) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  df.umap.metaInfo <- gg.umap.metaInfo[["data"]]
  
  # Kmeans Clustering on UMAP data
  clustID <- as.factor(
    NbClust::NbClust(df.umap.metaInfo[, c("UMAP1", "UMAP2")], 
                     method = 'complete', index = 'all')$Best.partition)
  gg.umap.cluster <- getUMAPplot(data.frame(cbind(df, clustID)), 
                          groupColName = "clustID", alpha = 1, size = 1.5)
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
  parDistribution <- plotParDistributionForClusters(
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