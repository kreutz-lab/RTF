#' Generate plots of data
#'
#' @description Plot data. If there are doses, i.e., if there is a column d,
#' plot data points of different doses in different colours.
#' @return ggplot object
#' @param data Data frame with columns t, y, and d (optional)
#' @export plotData
#' @examples
#' data.doseResponse <- getSimData(modus = "doseDependent")
#' plotData(data.doseResponse)

plotData <- function(data) {
    if (("d" %in% names(data))) {
        data <- data[order(data$d),]
        
        ggplot2::ggplot(data, ggplot2::aes(x = t, y = y, color = factor(d))) +
            ggplot2::geom_point() +
            ggplot2::labs(color = 'Dose') +
            ggplot2::scale_colour_viridis_d(direction = -1) +
            ggplot2::theme_bw()
    } else {
        ggplot2::ggplot(data, ggplot2::aes(x = t, y = y)) +
            ggplot2::geom_point() +
            ggplot2::theme_bw()
    }
}