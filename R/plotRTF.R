#' Generate plots of RTF results
#'
#' @description Plot RTF results
#' @return ggplot object
#' @param optimObject optimObject containing elements "finalModel" and "modus"
#' @param fileNamePrefix File name prefix. If length>0 plots will be written to
#' file, otherwise they will be plotted directly. Default is empty string.
#' @param plotTitle Title of the plot (Default is no title).
#' @param plotAllFits Boolean indicating if all fits should be plotted. Only use
#' if fileNamePrefix is given. Only relevant for "singleDose"
#' modus. (Default: TRUE)
#' @param plotAllPointsWaterfall Boolean indicating if all points should be
#' plotted in waterfall plot (Default: FALSE).
#' If FALSE, all values up to the median of those values are plotted.
#' @param pointAlpha Transparency of points in plot of fit (Default: 0.5).
#' @param pointSize Point size for data points in plot of fit (Default: 0.75).
#' @param lineAlpha For modus 'singleDose': Transparency of line in plot of fit 
#' (Default: 0.5).
#' @param lineWidth For modus 'singleDose': Width of line in plot of fit 
#' (Default: 1.5).
#' @param lineAlpha Transparency of line in plot of fit (Default: 0.5 for 
#' modus 'singleDose', else 1).
#' @param lineWidth Width of line in plot of fit (Default: 1.5 for modus 
#' 'singleDose', else 1).
#' @param color Only relevant if modus = 'singleDose': Color of line and points
#'  in plot of fit (Default: "#290AD8")
#' @export plotRTF
#' @examples
#' data <- getSimData(modus = "singleDose")
#' res <- RTF(data)
#' plotRTF(res, plotAllFits = FALSE)
plotRTF <- function(optimObject,
                    fileNamePrefix = "",
                    plotTitle = "",
                    plotAllFits = TRUE,
                    plotAllPointsWaterfall = FALSE,
                    pointAlpha = 0.5,
                    pointSize = 0.75,
                    lineAlpha = NULL,
                    lineWidth = NULL,
                    color = "#290AD8") {
    optimObject <- optimObject$finalModel

    saveToFile <- nchar(fileNamePrefix) > 0

    modus <- optimObject$modus
    data <- optimObject$data
    optimResults <- optimObject$optimResults
    bestOptimResult <- optimObject$bestOptimResult

    if (is.null(optimResults) | is.null(bestOptimResult)) {
        stop("optimResults and bestOptimResult needs to be available in
             the optimObject.")
    }

    par <- bestOptimResult$par

    numCol <- 1
    if (modus == "singleDose") {
        height <- 12
    } else if (modus == "doseDependent") {
        height <- 17
    }

    ############################################################################
    waterfallPlot <- parDistributionPlot <- NA

    optimResTmpLstParsAll <- lapply(optimResults, function(x) {
        tmp <- unlist(x[grep("par", names(x))])
        names(tmp) <- sub("par.", "", names(tmp))
        tmp
    })

    ############################################################################

    optimResTmpLstValuesAll <- unlist(lapply(optimResults, function(x) {
        unlist(x[grep("value", names(x))])
    }))

    waterfallPlot <- plotWaterfallPlot(optimResTmpLstValuesAll,
        plotAllPoints = plotAllPointsWaterfall
    )

    ############################################################################

    waterfallPlotData <- waterfallPlot$data

    if (plotAllFits) {
        if (saveToFile) {
            gg.lst <- list()
            for (i in seq(length(optimResTmpLstParsAll))) {
                pars <- optimResTmpLstParsAll[[i]]
                value <- optimResTmpLstValuesAll[[i]]

                title <- paste0(
                    "Log-likelihood: ",
                    signif(value, 2),
                    "; ",
                    paste(
                        names(pars),
                        signif(pars, 4),
                        sep = ": ",
                        collapse = ", "
                    )
                )
                title <- paste(strwrap(title, width = 80), collapse = "\n")
                if (modus == "singleDose") {
                    gg <- plotFit(
                        par = pars,
                        y = data$y,
                        t = data$t,
                        withData = TRUE,
                        pointAlpha = pointAlpha,
                        pointSize = pointSize,
                        lineAlpha = lineAlpha,
                        lineWidth = lineWidth,
                        color = color
                    ) +
                        ggplot2::ggtitle(title)
                } else if (modus == "doseDependent") {
                    gg <- plotFit(
                        pars,
                        withData = TRUE,
                        modus = optimObject$modus,
                        y = data$y,
                        t = data$t,
                        d = data$d,
                        title = title,
                        pointAlpha = pointAlpha,
                        pointSize = pointSize,
                        lineAlpha = lineAlpha,
                        lineWidth = lineWidth
                    )
                }

                gg.lst <- append(gg.lst, list(gg = gg))
            }

            grDevices::pdf(
                file = paste0(fileNamePrefix, "_", modus, "_allFits.pdf"),
                width = 8,
                height = 10
            )
            for (i in seq(length(gg.lst))) {
                gg <- gg.lst[[i]]
                waterfall.tmp <- plotWaterfallPlot(
                    waterfallPlotData, i,
                    plotAllPoints = plotAllPointsWaterfall
                )
                waterfallPlot2 <- patchwork::wrap_plots(
                    gg, waterfall.tmp,
                    ncol = numCol
                )
                print(waterfallPlot2)
            }
            grDevices::dev.off()
        } # else {
        # message("Please enter a file name to save all fits to file.")
        # }
    }

    ############################################################################

    optimResTmpLstParsAll.df <- data.frame(
        do.call(rbind, optimResTmpLstParsAll)
    )
    optimResTmpLstParsAll.df.long <-
        reshape2::melt(optimResTmpLstParsAll.df)
    parDistributionPlot <-
        plotParamDistribution(optimResTmpLstParsAll.df.long)

    ############################################################################

    title <- paste0(
        "Log-likelihood: ",
        signif(bestOptimResult$value, 2),
        "; ",
        paste(
            names(par),
            signif(par, 4),
            sep = ": ",
            collapse = ", "
        )
    )
    title <- paste(strwrap(title, width = 100), collapse = "\n")

    if (modus == "singleDose") {
        RTFComponentsPlot <- plotFit(
            par = par,
            y = data$y,
            t = data$t,
            withData = TRUE,
            modus = modus,
            pointAlpha = pointAlpha,
            pointSize = pointSize,
            lineAlpha = lineAlpha,
            lineWidth = lineWidth,
            color = color
        ) # + ggplot2::ggtitle(title)

        bestFit.plot <-
            patchwork::wrap_plots(RTFComponentsPlot,
                waterfallPlot,
                parDistributionPlot,
                ncol = numCol
            )
    } else if (modus == "doseDependent") {
        bestFitWDataPlot <- plotFit(
            par,
            withData = TRUE,
            modus = modus,
            y = data$y,
            t = data$t,
            d = data$d,
            pointAlpha = pointAlpha,
            pointSize = pointSize,
            lineAlpha = lineAlpha,
            lineWidth = lineWidth
        )

        ##################################################

        doses <- sort(unique(data[["d"]]))
        doseSeries <- seq(0, max(doses), length.out = 1000)

        hillsResLst <- list()
        for (dose in doseSeries) {
            RTFResVec <- getHillResults(d = dose, params = par)

            hillsResLst <- append(
                hillsResLst,
                list(c(d = dose, RTFResVec))
            )
        }

        hillsResLst.df <- dplyr::bind_rows(hillsResLst)

        # d <- seq(0, max(data[["d"]]), length.out = 1000)
        # df <- getHillResults(d = d, params = par)

        # Group1: A, B, b
        # Group2: alpha, beta, gamma, tau

        hillsResLst.df.long <- reshape2::melt(hillsResLst.df, id.vars = c("d"))

        hillsResLst.df.long$Group <- 2
        hillsResLst.df.long[
            hillsResLst.df.long$variable %in% c("A", "B", "b"),
        ]$Group <- 1

        doseParamPlot1 <- ggplot2::ggplot(
            hillsResLst.df.long[hillsResLst.df.long$Group == 1, ],
            ggplot2::aes(x = d, y = value, color = variable)
        ) +
            ggplot2::geom_line(ggplot2::aes(
                linetype = variable,
                color = variable
            )) +
            ggplot2::xlab("Dose") +
            ggplot2::ylab("Parameter value") +
            # ggplot2::ggtitle(title) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.position = "bottom",
                legend.title = ggplot2::element_blank()
            ) +
            ggplot2::scale_colour_manual(
                values = c(A = "#004949", B = "#db6d00", b = "#b66dff")
            ) +
            ggplot2::scale_linetype_manual(
                values = c(A = "twodash", B = "dotted", b = "longdash")
            )

        doseParamPlot2 <- ggplot2::ggplot(
            hillsResLst.df.long[hillsResLst.df.long$Group == 2, ],
            ggplot2::aes(x = d, y = value, color = variable)
        ) +
            ggplot2::geom_line(ggplot2::aes(
                linetype = variable,
                color = variable
            )) +
            ggplot2::xlab("Dose") +
            ggplot2::ylab("Parameter value") +
            # ggplot2::ggtitle(title) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.position = "bottom",
                legend.title = ggplot2::element_blank()
            ) +
            ggplot2::scale_colour_manual(
                values = c(
                    alpha = "#490092",
                    beta = "#009E73",
                    gamma = "#ff6db6",
                    tau = "#006ddb"
                )
            ) +
            ggplot2::scale_linetype_manual(
                values = c(
                    alpha = "twodash", beta = "dotted",
                    gamma = "longdash", tau = "dashed"
                )
            )

        bestFit.plot <- patchwork::wrap_plots(
            bestFitWDataPlot,
            patchwork::wrap_plots(
                doseParamPlot1, doseParamPlot2,
                nrow = 1
            ),
            waterfallPlot,
            parDistributionPlot,
            ncol = numCol
        )
    }

    ############################################################################

    bestFit.plot <-
        bestFit.plot + patchwork::plot_annotation(
            title = plotTitle,
            subtitle = title
        )

    if (saveToFile) {
        ggplot2::ggsave(
            filename = paste0(
                fileNamePrefix, "_", modus,
                "_bestFit.pdf"
            ),
            bestFit.plot,
            width = 8,
            height = height
        )
        bestFit.plot
    } else {
        bestFit.plot
    }
}
