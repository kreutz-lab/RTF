#' Plot the scaled or unscaled dynamics of multiple time courses separated by
#' cluster
#'
#' @description Plots the scaled or unscaled dynamics of multiple time courses
#' separated by cluster
#' @return ggplot2 object with the dynamics plots separated by cluster.
#' @param df Data frame 'df' of the dynamics with columns 't' (time),
#'  'y' (value), 'id' (time course name), 'cluster' (cluster ID).
#' @param scaled Boolean indicating if values should be scaled for the time
#' courses in a cluster.
#' @export plotDynamicsForClusters
#' @importFrom dplyr %>%
#' @examples
#' data(strasenParams)
#' metaInfo <- sub("_[^_]+$", "", row.names(strasenParams))
#' metaInfoName <- "Species"
#' param.df.wMetaInfo <- data.frame(strasenParams, metaInfo)
#' colnames(param.df.wMetaInfo) <- c(colnames(strasenParams), metaInfoName)
#' gg.umap.metaInfo <- plotUMAP(param.df.wMetaInfo,
#'     groupColName = metaInfoName,
#'     alpha = 1, size = 1.5
#' )
#' df.umap.metaInfo <- gg.umap.metaInfo[["data"]]
#' clustID <- as.factor(
#'     NbClust::NbClust(df.umap.metaInfo[, c("UMAP1", "UMAP2")],
#'         method = "complete", index = "all"
#'     )$Best.partition
#' )
#' gg.umap.cluster <- plotUMAP(data.frame(cbind(strasenParams, clustID)),
#'     groupColName = "clustID", alpha = 1, size = 1.5
#' )
#' df.umap.cluster <- gg.umap.cluster[["data"]]
#' df <- data.frame(cbind(
#'     df.umap.cluster[, c("UMAP1", "UMAP2", "clustID")],
#'     metaInfo, strasenParams
#' ))
#' colnames(df) <- c(
#'     "UMAP1", "UMAP2", "clustID",
#'     metaInfoName, colnames(strasenParams)
#' )
#' df.byCluster <- split(df, df$clustID)
#' xi <- seq(0, 10, length.out = 1000)
#' geom_line.lst <- list()
#' for (cluster in df.byCluster) {
#'     for (rowId in seq(nrow(cluster))) {
#'         row <- cluster[rowId, ]
#'         id <- rownames(cluster)[rowId]
#'         functionResVec <- getTransientFunctionResult(
#'             rtfPar = row[!(colnames(row) %in% c(
#'                 "signumTF_sus",
#'                 "signumTF_trans"
#'             ))],
#'             t = xi,
#'             signumTF_sus = row[["signumTF_sus"]],
#'             signumTF_trans = row[["signumTF_trans"]]
#'         )
#'
#'         geom_line.lst <- append(
#'             geom_line.lst,
#'             list(data.frame(
#'                 t = xi,
#'                 y = functionResVec,
#'                 id = rep(id, times = length(xi)),
#'                 cluster = rep(cluster$clustID, times = length(xi))
#'             ))
#'         )
#'     }
#' }
#' df <- dplyr::bind_rows(geom_line.lst)
#' plot.scaled <- plotDynamicsForClusters(df, scaled = TRUE)
plotDynamicsForClusters <- function(df, scaled = TRUE) {
    if (scaled) {
        df <- df %>%
            dplyr::group_by(id, cluster) %>%
            dplyr::mutate(y = scale(y))
    }

    clusterIds <- sort(unique(df$cluster))
    gg.cluster.lst <- list()
    for (clustID in clusterIds) {
        gg <- ggplot2::ggplot(
            data = df %>% dplyr::filter(cluster == clustID),
            ggplot2::aes(x = t, y = y, color = id)
        ) +
            ggplot2::theme_bw() +
            ggplot2::geom_line() +
            ggplot2::ggtitle(paste0("Cluster ", clustID)) +
            ggplot2::theme(
                legend.direction = "horizontal",
                legend.position = "bottom",
                legend.text = ggplot2::element_text(size = 7)
            ) +
            ggplot2::scale_color_discrete(name = "")
        gg.cluster.lst <- append(gg.cluster.lst, list(gg))
    }
    names(gg.cluster.lst) <- clusterIds
    patchwork::wrap_plots(gg.cluster.lst, ncol = 3)
}
