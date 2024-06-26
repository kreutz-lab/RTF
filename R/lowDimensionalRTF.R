#' Generate low-dimensional representation of multiple time courses based on
#' their RTF parameters
#' @description Generates low-dimensional representation of multiple time
#' courses based on their RTF parameters.
#' @return List with data frame of RTF parameters for each time course
#' ('param.data'), a list with the RTF result for each time course
#' ('RTFmodelLst'), data frame of UMAP results ('umap.data'), data frame of
#' dynamics per cluster ('dynamics.data'), and ggplot2 plots of the results
#' ('plots'). Function also saves those objects to files if
#' saveToFile=TRUE.
#' @param df Data frame with the first column corresponding to the time points
#' and all the following columns corresponding to the different time courses.
#' @param metaInfo String of the column name with meta information (.e.g.
#' groups.
#' @param metaInfoName String of the name of the meta information.
#' @param takeRank Boolean indicating if rank should be used for UMAP instead
#' of absolute value (Default: TRUE)
#' @param scaled Boolean indicating if values used for UMAP should be scaled
#' for the time courses in a cluster (Default: TRUE). Only relevant if
#' takeRank = FALSE.
#' @param dimX Integer from 1 to 3 indicating the UMAP dimension plotted on
#' the x axis (Default: 1).
#' @param dimY Integer from 1 to 3 indicating the UMAP dimension plotted
#' on the y axis (Default: 2).
#' @param metaInfoSecondRow Boolean indicating if second row contains the meta
#' info.
#' @param fileString String the output files should contain.
#' @param plotItemHeight Integer indicating height of each plot type in the
#' output plot.
#' @param plotWidth Integer indicating width of output plot.
#' @param readInParamRdsFilePath If the RTF parameters to time courses have
#' already been determined previously, the path of the resulting .rds file can
#' be specified  using readInParamRdsFilePath, so that the time-intensive
#' calculation of the RTF parameter does not have to be repreated a second time.
#' @param modelReduction Boolean indicating if model reduction should be
#' performed for RTF
#' @param nInitialGuesses Integer indicating number of initial guesses
#' (in addition to the default initial guess) used for all four combinations
#' of signSus and signTrans, which both can be -1 or 1 (Default: 50).
#' @param sameSign Boolean indicating if sign of sustained RTF part
#' (signSus) and transient RTF part (signTrans) should be equal
#' (Default: TRUE) (Only relevant if param.data is not provided).
#' @param saveToFile Boolean indicating if results should be saved to file
#' @param numClust (Optional) Number of clusters. If not specified, number of
#' clusters will be detrmined automatically using the function
#' NbClust::NbClust().
#' @param param.data (Optional) If a data frame with the RTF parameter values
#' is already available, you can provide it here for saving time.
#' Its columns 'alpha', 'gamma', 'A', 'B', 'b', 'tau', 'signSus',
#' and 'signTrans' represent the different RTF parameters. The rows
#' correspond to the different time courses.
#' @param RTFmodelLst (Optional) If a list with the RTF result for each time
#' course calculated, e.g. using the function getParamsFromMultipleTimeCourses()
#' or lowDimensionalRTF() is already available, you can provide it here for
#' saving time.
#' @param plotFitsToSingleFile Boolean indicating if plots should be returned
#' as a single file.
#' @param plotFitOnly Plot fit only without additional information as provided
#' using function plotRTF().
#' @param plotAllPointsWaterfall Boolean indicating if all points should be
#' plotted in waterfall plot (Default: FALSE).
#' If FALSE, all values up to the median of those values are plotted.
#' @param seed Seed for UMAP.
#' @param alphaUMAP Alpha value (between 0 and 1) of the data points in the
#' static UMAP plot (Default: 1).
#' @param sizeUMAP Size of the data points in the static UMAP plot
#' (Default: 1.5).
#' @export lowDimensionalRTF
#' @examples
#' \dontrun{
#' data(strasen)
#' df <- strasen[, 1:20]
#' colNames <- colnames(df[2:ncol(df)])
#' metaInfo <- species <- sub("_[^_]+$", "", colNames)
#' res <- lowDimensionalRTF(
#'     df = df,
#'     metaInfo = metaInfo,
#'     fileString = "strasen_subset",
#'     metaInfoName = "Species",
#'     saveToFile = FALSE
#' )
#' ggplot2::ggsave(
#'     filename = "test.pdf", plot = res[["plots"]],
#'     width = 10, height = 30
#' )
#' }
lowDimensionalRTF <- function(df,
                              metaInfo = c(),
                              metaInfoName = "Species",
                              takeRank = TRUE,
                              scaled = TRUE,
                              dimX = 1,
                              dimY = 2,
                              metaInfoSecondRow = FALSE,
                              fileString = "lowDimRTF",
                              plotItemHeight = 9,
                              plotWidth = 22,
                              readInParamRdsFilePath = "",
                              modelReduction = FALSE,
                              nInitialGuesses = 50,
                              sameSign = TRUE,
                              saveToFile = TRUE,
                              numClust = NULL,
                              param.data = NULL,
                              RTFmodelLst = NULL,
                              plotFitsToSingleFile = TRUE,
                              plotFitOnly = FALSE,
                              plotAllPointsWaterfall = FALSE,
                              seed = 111,
                              alphaUMAP = 1,
                              sizeUMAP = 1.5) {
    if (metaInfoSecondRow & length(metaInfo) == 0) {
        metaInfo <- as.character(unlist(df[1, 2:ncol(df)]))
        df <- df[-1, ]
    } else if (length(metaInfo) == 0) {
        metaInfo <- rep("Group", ncol(df) - 1)
    }

    if (length(metaInfo) != ncol(df) - 1) {
        warning("Please provide meta information of size #columns-1 .")
    }

    if (is.null(param.data)) {
        params.lst <- getParamsFromMultipleTimeCourses(
            df = df,
            fileString = fileString,
            readInParamRdsFilePath = readInParamRdsFilePath,
            modelReduction = modelReduction,
            nInitialGuesses = nInitialGuesses,
            sameSign = sameSign,
            plotFitsToSingleFile = plotFitsToSingleFile,
            plotFitOnly = plotFitOnly,
            plotAllPointsWaterfall = plotAllPointsWaterfall
        )
        param.data <- params.lst[["param.df"]]
        params.lst <- NULL
    }

    plotsLst <- plotLowDimensionalRTF(
        df = param.data,
        metaInfo = metaInfo,
        metaInfoName = metaInfoName,
        takeRank = takeRank,
        scaled = scaled,
        dimX = dimX,
        dimY = dimY,
        maxTime = max(df$time),
        numClust = numClust,
        seed = seed,
        alphaUMAP = alphaUMAP,
        sizeUMAP = sizeUMAP
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
                        plot.title = ggplot2::element_text(face = "bold")
                    )
                )
        ),
        patchwork::wrap_elements(
            plotsLst[["dynamics.scaled"]] +
                patchwork::plot_annotation(
                    title = "Dynamics (scaled)",
                    theme = ggplot2::theme(
                        plot.title = ggplot2::element_text(face = "bold")
                    )
                )
        ),
        heights = c(
            rep(plotItemHeight / sumHeights, 3),
            rep(((ceiling(numCluster / 3)) * plotItemHeight
            ) /
                sumHeights, 2)
        )
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
            row.names = TRUE
        )
        utils::write.csv(plotsLst[["umap.data"]],
            file = paste0("umap_", fileString, ".csv"),
            row.names = TRUE
        )
        utils::write.csv(
            plotsLst[["dynamics.data"]],
            file = paste0("dynamics_", fileString, ".csv"),
            row.names = FALSE
        )
    }

    list(
        param.data = param.data,
        RTFmodelLst = RTFmodelLst,
        umap.data = plotsLst[["umap.data"]],
        dynamics.data = plotsLst[["dynamics.data"]],
        plots = plotsCombined
    )
}
