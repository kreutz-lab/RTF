#' Plot the distribution of the parameters for all clusters
#'
#' @description Plots the parameters for all clusters, where the colored area
#' around the median of a cluster (indicated by point) for a specific parameter
#' is bounded by 25th and 75th quantile.
#' @return ggplot2 object of the parameter distribution for the clusters
#' @param clust.df Data frame with the cluster ID (defined by clusterColName),
#' parameters ('variable'), and value for the parameter of a specific cluster
#' ('value') as columns.
#' @param clusterColName Name of the column containing the cluster IDs.
#' @export plotParamDistributionForClusters
#' @importFrom dplyr %>%
#' @examples
#' data(strasenParams)
#' params <- colnames(strasenParams)
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
#'     groupColName = "clustID", alpha = 1,
#'     size = 1.5
#' )
#' df.umap.cluster <- gg.umap.cluster[["data"]]
#' umap.data <- data.frame(cbind(
#'     df.umap.cluster[, c("UMAP1", "UMAP2", "clustID")],
#'     metaInfo, strasenParams
#' ))
#' colnames(umap.data) <- c(
#'     "UMAP1", "UMAP2", "clustID",
#'     metaInfoName, colnames(strasenParams)
#' )
#' library(dplyr)
#' umap.data.selected <- umap.data %>%
#'     dplyr::select(clustID, !!params) %>%
#'     reshape2::melt(id.vars = c("clustID"))
#' parDistribution <- plotParamDistributionForClusters(umap.data.selected,
#'     clusterColName = "clustID"
#' )
plotParamDistributionForClusters <- function(clust.df, clusterColName = "") {
    params <- unique(clust.df$variable)
    nparams <- length(params)

    mapping <- data.frame(
        key = params,
        value = 1:nparams,
        stringsAsFactors = FALSE
    )

    range01 <- function(x, ...) {
        (x - min(x, ...)) / (max(x, ...) - min(x, ...))
    }

    clust.df <- clust.df %>%
        dplyr::group_by(variable) %>%
        dplyr::mutate(value = range01(value)) %>%
        dplyr::group_by(variable, !!as.name(clusterColName)) %>%
        dplyr::summarise(
            median = stats::median(value, na.rm = TRUE),
            Quantile25 = stats::quantile(value, 0.25),
            Quantile75 = stats::quantile(value, 0.75)
        )

    clust.df$x <- mapping$value[match(
        clust.df$variable, mapping$key
    )]

    vertical.lines <- 1:nparams
    gg <- ggplot2::ggplot(
        clust.df,
        ggplot2::aes(
            x = x, y = median, color = get(clusterColName)
        )
    ) +
        ggplot2::geom_point() +
        ggplot2::geom_ribbon(
            ggplot2::aes(
                ymax = Quantile75, ymin = Quantile25,
                fill = get(clusterColName)
            ),
            alpha = 1 / 5
        ) +
        ggplot2::geom_vline(xintercept = vertical.lines) +
        ggplot2::scale_x_continuous(
            breaks = 1:nparams,
            labels = params
        ) +
        ggplot2::scale_y_continuous(breaks = c(0, 1)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            legend.title = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(size = 10,
                                                angle = 45, 
                                                hjust = 1),
            axis.text.y = ggplot2::element_text(size = 12),
            legend.direction = "horizontal",
            legend.position = "bottom"
        )
    gg
}
