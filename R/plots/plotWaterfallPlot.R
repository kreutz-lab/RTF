#' @description Generate waterfall plot of all fits. Optionally a specific fit
#' can be highlighted using the argument 'idxCurrentFit'.
#' @return ggplot object of the waterfall plot
#' @param optimObject Dataframe with columns indicating for each fit
#' "value" (optimization measure), "idx" (position of a fit in the sequence
#' of ordered values), "col" (HEX values for color that should be given to each
#'  fit)
#' @param idxCurrentFit Index indicating the position of the fit in the
#' waterfall plot that should be highlighted.
#' @export plotWaterfallPlot
#' @examples
#' data <- getExampleDf()
#' data <- scaleTimeCol(data)
#' optimObject.orig <- initializeOptimObject(
#'                         data, modus = 'RetardedTransientDynamics')
#' signum_TF <- 1
#' optimObject.orig$fixed[["signum_TF"]] <- signum_TF
#' nInitialGuesses <- 50
#' res.lst.wFinal <- getInitialGuessResults(optimObject.orig, objFunct,
#'                     nInitialGuesses, plot = TRUE)
#' final <- res.lst.wFinal$final
#' waterfallPlotData <- final$gg.waterfall$data
#' plotWaterfallPlot(waterfallPlotData, idxCurrentFit = 4)

plotWaterfallPlot <- function(optimResTmpLstValuesAll, idxCurrentFit = NULL) {
  if (!is.null(idxCurrentFit)) {
    #colVals <- rep("black", nrow(optimResTmpLstValuesAll))
    # colVals[idxCurrentFit] <- "red"
    df <- optimResTmpLstValuesAll
    df$col <- "#000000" # "black"
    df[df$idx == idxCurrentFit, ]$col <- "#FF0000" #  "red"
  } else {
    df <- data.frame(value = sort(optimResTmpLstValuesAll))
    df$idx <- as.numeric(row.names(df))
    df[df == 10^10] <- NA
    df$col <- "#000000" # "black"
  }

  gg <- ggplot(data = df, aes(x = idx, y = value, color = col)) +
    geom_point() +
    # geom_line() +
    labs(x='Index') +
    scale_colour_manual(values=c("#000000",  "#FF0000")) +
    theme_bw() +
    theme(legend.position="none")

  gg
}
