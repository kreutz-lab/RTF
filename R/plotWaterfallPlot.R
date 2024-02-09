#' Generate waterfall plot of all fits
#'
#' @description Generate waterfall plot of all fits. Optionally a specific fit
#' can be highlighted using the argument 'idxCurrentFit'.
#' @return ggplot object of the waterfall plot
#' @param waterfallValues List of optimization measures or, if idxCurrentFit is
#'  given, Dataframe with columns indicating for each fit "value" 
#'  (optimization measure), "idx" (position of a fit in the sequence
#' of ordered values), "col" (HEX values for color that should be given to each
#'  fit)
#' @param idxCurrentFit Index indicating the position of the fit in the
#' waterfall plot that should be highlighted.
#' @param plotAllPoints Boolean indicating if all points should be plotted 
#' (Default: FALSE). If FALSE, all values up to the median of those values 
#' are plotted.
#' @export plotWaterfallPlot
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(
#'                         data, modus = 'RetardedTransientDynamics')
#' signum_TF <- 1
#' optimObject.orig$fixed[["signum_TF"]] <- signum_TF
#' nInitialGuesses <- 100
#' res.lst.wFinal <- getMultiStartResults(optimObject.orig, objFunct,
#'                     nInitialGuesses)
#' 
#' optimResults <- res.lst.wFinal$optimResults
#' 
#' optimResults.optimRes <- lapply(optimResults, function(x) {
#'   tmp <- unlist(x[grep("optimRes", names(x))], recursive = FALSE)
#'   names(tmp) <- sub("optimRes.", "", names(tmp))
#'   tmp
#' })
#' 
#' optimResTmpLstValuesAll <- unlist(
#'   lapply(optimResults.optimRes, function(x) unlist(x[grep("value",names(x))])))
#' gg <- plotWaterfallPlot(optimResTmpLstValuesAll)

plotWaterfallPlot <- function(waterfallValues, idxCurrentFit = NULL,
                              plotAllPoints = FALSE) {
  
  if (!is.null(idxCurrentFit)) {
    #colVals <- rep("black", nrow(waterfallValues))
    # colVals[idxCurrentFit] <- "red"
    df <- waterfallValues
    df$col <- "#000000" # "black"
    df[df$idx == idxCurrentFit, ]$col <- "#FF0000" #  "red"
  } else {
    df <- data.frame(value = sort(waterfallValues))
    df$idx <- as.numeric(row.names(df))
    # df[df >= 10^10] <- NA
    df$col <- "#000000" # "black"
  }

  if (plotAllPoints) {
    maxValue <- max(df$value)
  } else {
    maxValue <- median(df$value)
  }
  
  gg <- ggplot2::ggplot(data = df,
                        ggplot2::aes(x = idx, y = value, color = col)) +
    ggplot2::geom_point() +
    # geom_line() +
    ggplot2::labs(x = 'Index', y = 'Value') +
    ggplot2::scale_colour_manual(values = c("#000000",  "#FF0000")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylim(NA, maxValue)

  gg
}
