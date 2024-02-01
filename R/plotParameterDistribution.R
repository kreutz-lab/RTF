#' Plot histograms of fitted parameters
#'
#' @description Plot histograms of fitted parameters
#' @return ggplot object of the histograms of the all parameters resulting
#' from each fit
#' @param df.parameterValues.long Data frame of the parameter values which
#' have been determined by each of the fits. Two columnns: parameter name
#' ('variable'), value of parameter ('value').
#' @export plotParameterDistribution
#' @examples
#' parameters <- c("par1", "par2", "par3", "par4", "par5")
#' means <- c(0.5, 2.3, 1.2, 6.5, 4.1)
#' sds <- c(0.02, 1.2, 0.1, 0.8, 1.1)
#' nFits <- 50
#' values <- c()
#' for (i in 1:length(parameters)) {
#'  parameter <- parameters[i]
#'  values <- c(values, rnorm(n = nFits, mean = means[i], sd = sds[i]))
#' }
#' df.parameterValues.long <- data.frame(variable = rep(parameters, each = nFits), value = values)
#' gg.paramDistr <- plotParameterDistribution(df.parameterValues.long)

plotParameterDistribution <- function(df.parameterValues.long) {
  ggplot2::ggplot(df.parameterValues.long, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      panel.spacing = ggplot2::unit(0.1, "lines"),
      strip.text.x = ggplot2::element_text(size = 8)
    ) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::ylab("Count")
}
