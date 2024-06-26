#' Generate plots based on the list of RTF parameters to multiple time courses
#'
#' @description Generates plots using plotRTF based on the RTF parameters
#' derived for each of multiple time courses.
#' @return pdf file, where each page corresponds to one time course.
#' @param RTFmodelLst List with the RTF result for each time course.
#' @param fileString String that should be added to file name.
#' @param saveFolderPath Path of folder to where plots and rds files should be
#' saved. Default: current folder.
#' @param height Integer indicating page height
#' @param width Integer indicating page width
#' @param plotFitsToSingleFile Boolean indicating if plots should be returned
#' as a single file.
#' @param plotFitsToSingleFileExtension Image file extension, if
#' plotFitsToSingleFile=TRUE (Default: jpeg)
#' @param plotFitOnly Plot fit only without additional information as provided
#' using function plotRTF().
#' @param plotAllPointsWaterfall Boolean indicating if all points should be
#' plotted in waterfall plot (Default: FALSE).
#' If FALSE, all values up to the median of those values are plotted.
#' @export plotRTFOnMultipleTimeCourses
#' @examples
#' data(strasen)
#' df.multipleTimeCourses <- strasen[, 1:3]
#' RTFmodelLst <- RTFOnMultipleTimeCourses(df.multipleTimeCourses)
#' plotRTFOnMultipleTimeCourses(RTFmodelLst)
plotRTFOnMultipleTimeCourses <- function(RTFmodelLst,
                                         fileString = "",
                                         saveFolderPath = "",
                                         height = 12, width = 10,
                                         plotFitsToSingleFile = TRUE,
                                         plotFitsToSingleFileExtension = "jpeg",
                                         plotFitOnly = FALSE,
                                         plotAllPointsWaterfall = FALSE) {
    if (nchar(saveFolderPath) > 0 & !grepl("/$", saveFolderPath)) {
        saveFolderPath <- paste0(saveFolderPath, "/")
    }

    if (plotFitsToSingleFile & !(plotFitsToSingleFileExtension %in%
        c("jpeg", "png", "pdf", "svg"))) {
        stop("plotFitsToSingleFileExtension should be one of the following:
             'jpeg', 'png', 'pdf', 'svg'.")
    }

    if (plotFitsToSingleFile) {
        grDevices::pdf(paste0(saveFolderPath, "modelPlots_", fileString, ".pdf"),
            height = height,
            width = width
        )
    }
    for (i in seq(length(RTFmodelLst))) {
        el <- RTFmodelLst[[i]]
        title <- names(RTFmodelLst)[i]

        if (plotFitOnly) {
            optimObject <- el$finalModel
            bestOptimResult <- optimObject$bestOptimResult
            par <- bestOptimResult$par
            value <- bestOptimResult$value
            data <- optimObject$data

            plotTitle <- paste0(
                title, "; Log-likelihood: ",
                signif(value, 2),
                "; ",
                paste(names(par),
                    signif(par, 4),
                    sep = ": ", collapse = ", "
                )
            )
            plotTitle <- paste(strwrap(plotTitle, width = 70), collapse = "\n")
            plt <- plotFit(
                par = par,
                y = data$y,
                t = data$t,
                modus = "singleDose",
                withData = TRUE,
                title = plotTitle
            )
        } else {
            plt <- plotRTF(el,
                plotTitle = title,
                plotAllPointsWaterfall = plotAllPointsWaterfall
            )
        }

        if (!plotFitsToSingleFile) {
            ggplot2::ggsave(
                file = paste0(
                    saveFolderPath, "modelPlot_",
                    gsub("/", "_", title), ".",
                    plotFitsToSingleFileExtension
                ),
                device = plotFitsToSingleFileExtension,
                plot = plt,
                height = height,
                width = width
            )
        } else {
            print(plt)
        }
    }

    if (plotFitsToSingleFile) {
        grDevices::dev.off()
    }
}
