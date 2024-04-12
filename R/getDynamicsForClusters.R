#' Get time series dynamics plots separated by cluster
#'
#' @description Generates line plots of the dynamics of time series separated by
#' cluster, where each line represents one time series.
#' @return List with ggplot2 objects with 
#' scaled dynamics ('plot.scaled') and unscaled dynamics ('plot.notscaled) and 
#' data frame 'df' of the unscaled dynamics (with columns 't' (time), 'y' (value),
#' 'id' (time series name), 'cluster' (cluster ID)).
#' @param df Data frame with column for cluster ID ("clustID") and columns
#' named "alpha", "gamma", "A", "B", "b", "tau", and "signum_TF" corresponding 
#' to the parameter names of the RTF.
#' @param maxTime Maximum time until dynamics should be plotted
#' @export getDynamicsForClusters
#' @examples
#' data(strasenParams)
#' metaInfo <- sub("_[^_]+$", "", row.names(strasenParams))
#' metaInfoName <- "Species"
#' param.df.wMetaInfo <- data.frame(strasenParams, metaInfo)
#' colnames(param.df.wMetaInfo) <- c(colnames(strasenParams), metaInfoName)
#' gg.umap.metaInfo <- plotUMAP(df = param.df.wMetaInfo,
#'                              groupColName = metaInfoName,
#'                              alpha = 1, size = 1.5)
#' df.umap.metaInfo <- gg.umap.metaInfo[["data"]]
#' clustID <- as.factor(
#' NbClust::NbClust(df.umap.metaInfo[, c("UMAP1", "UMAP2")],
#'                  method = 'complete', index = 'all')$Best.partition)
#' gg.umap.cluster <- plotUMAP(df = data.frame(cbind(strasenParams, clustID)),
#'                             groupColName = "clustID", alpha = 1, 
#'                             size = 1.5)
#' df.umap.cluster <- gg.umap.cluster[["data"]]
#' df <- data.frame(cbind(
#'   df.umap.cluster[, c("UMAP1", "UMAP2", "clustID")],
#'   metaInfo, strasenParams))
#' colnames(df) <- c("UMAP1", "UMAP2", "clustID",
#'                          metaInfoName, colnames(strasenParams))
#' dynamics <- getDynamicsForClusters(df, maxTime = 10) 

getDynamicsForClusters <- function(df, maxTime = 10) {
    df.byCluster <- split(df, df$clustID)
    
    xi <- seq(0, maxTime, length.out = 1000)
    geom_line.lst <- list()
    for (cluster in df.byCluster) {
        for (rowId in seq(nrow(cluster))) {
            row <- cluster[rowId,]
            id <- rownames(cluster)[rowId]
            functionResVec <- getTransientFunctionResult(
                rtfPar = row[colnames(row) != "signum_TF"],
                t = xi,
                signum_TF = row[["signum_TF"]])
            
            geom_line.lst <- append(
                geom_line.lst, 
                list(data.frame(t = xi,
                                y = functionResVec,
                                id = rep(id, times = length(xi)),
                                cluster = rep(
                                    cluster$clustID, times = length(xi)))))
        }
    }
    
    geom_line.df <- dplyr::bind_rows(geom_line.lst)
    
    plot.scaled <- plotDynamicsForClusters(geom_line.df, scaled = TRUE)
    plot.notscaled <- plotDynamicsForClusters(geom_line.df, scaled = FALSE)
    
    list(df = geom_line.df, 
         plot.scaled = plot.scaled,
         plot.notscaled = plot.notscaled)
}