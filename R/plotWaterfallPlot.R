#' Generate waterfall plot of all fits
#'
#' @description Generate waterfall plot of all fits. Optionally, a specific fit
#' can be highlighted using the argument 'idxCurrentFit'.
#' @return ggplot object of the waterfall plot
#' @param waterfallValues List of likelihood values or, if idxCurrentFit is
#'  given, data frame with columns indicating for each fit "value" 
#'  (likelihood value), "idx" (position of a fit in the sequence
#' of ordered values), "col" (HEX values for color that should be given to each
#'  fit)
#' @param idxCurrentFit Index indicating the position of the fit in the
#' waterfall plot that should be highlighted.
#' @param plotAllPoints Boolean indicating if all points should be plotted 
#' (Default: FALSE). If FALSE, all values up to the median of those values 
#' are plotted.
#' @export plotWaterfallPlot
#' @examples
#' data <- getSimData()
#' optimObject.orig <- initializeOptimObject(data, modus = 'singleDose')
#' RTFmodelLst.wFinal <- getInitialGuessResults(optimObject.orig,
#'                                              nInitialGuesses = 50)
#' optimResults <- RTFmodelLst.wFinal$optimResults
#' optimResTmpLstValuesAll <- unlist(lapply(optimResults, function(x) {
#'   unlist(x[grep("value", names(x))])
#' }))
#' gg <- plotWaterfallPlot(optimResTmpLstValuesAll)

plotWaterfallPlot <- function(waterfallValues, idxCurrentFit = NULL,
                              plotAllPoints = FALSE) {
    
    if (!is.null(idxCurrentFit)) {
        df <- waterfallValues
        df$col <- "#000000" # "black"
        df[df$idx == idxCurrentFit, ]$col <- "#FF0000" #  "red"
    } else {
        df <- data.frame(value = sort(waterfallValues))
        df$idx <- as.numeric(row.names(df))
        df$col <- "#000000" # "black"
    }
    
    values <- df$value
    if (plotAllPoints) {
        maxValue <- max(values[values < 10^20])
    } else {
        maxValue <- stats::median(values)
        if (maxValue == 10^20) maxValue <- max(values[values < 10^20])
    }
    
    gg <- ggplot2::ggplot(data = df,
                          ggplot2::aes(x = idx, y = value, color = col)) +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(x = 'Index', y = 'Likelihood value') +
        ggplot2::scale_colour_manual(values = c("#000000",  "#FF0000")) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ylim(NA, maxValue)
    
    gg
}
