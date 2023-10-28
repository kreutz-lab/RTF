#' Generate plots for dose-response RTF
#'
#' @description Plot fitting results for optimObjects of modus 
#' "DoseDependentRetardedTransientDynamics"
#' @return ggplot object
#' @param optimObject optimObject containing element "finalModel"
#' @param fileNamePrefix File name prefix. If length>0 plots will be written to
#' file, otherwise they will be plotted directly.
#' @export plotDoseResponse
#' @examples
#' data.doseResponse <- getExampleDf(
#'                             modus = "DoseDependentRetardedTransientDynamics")
#' plotData(data.doseResponse)
#' res.doseResponse <- runRTF(data.doseResponse, 
#'                           modus = "DoseDependentRetardedTransientDynamics")
#' plotDoseResponse(res.doseResponse)

plotDoseResponse <- function(optimObject, fileNamePrefix = "", 
                             customParams = c()) {
  saveToFile <- nchar(fileNamePrefix) > 0
  
  par <- optimObject$finalParams
  for (v in 1:length(par)) assign(names(par)[v], par[[v]])
  
  data <- optimObject[["finalModel"]][["data"]]
  
  #############################################################################
  # 
  # plotFitWithDefinedParams <- function(data, par, t, y, d) {
  #   xi <- seq(0, max(t), length.out = 1000)
  #   
  #   doses <- sort(unique(d))
  #   
  #   for (v in 1:length(par)) assign(names(par)[v], par[[v]])
  # 
  #   geom_point.lst <- list()
  #   geom_line.lst <- list()
  #   for (i in seq(length(doses))) {
  #     df.dosis <- NULL
  #     dosis <- doses[i]
  #     
  #     # df.dosis <- getHillResults(d = dosis, param = par)
  #     
  #     geom_line.lst <- append(geom_line.lst, 
  #                             list(data.frame(t = xi,
  #                                             y = getTransientFunctionResult(
  #                                               t = xi,
  #                                               # par = c(tau_1 = df.dosis$tau_1,
  #                                               #         tau_2 = df.dosis$tau_2,
  #                                               #         A_sus = df.dosis$A_sus,
  #                                               #         A_trans = df.dosis$A_trans,
  #                                               #         p_0 = p_0,
  #                                               #         T_shift = df.dosis$T_shift,
  #                                               #         signum_TF = signum_TF),
  #                                               par = par,
  #                                               modus = "RetardedTransientDynamics"),
  #                                             d = rep(dosis, times = length(xi)))))
  #     
  #   } 
  #   
  #   geom_line.df <- dplyr::bind_rows(geom_line.lst)
  #   
  #   bestFitWData <- ggplot2::ggplot() +
  #     ggplot2::theme_bw() +
  #     ggplot2::labs(color='Dosis') +
  #     
  #     ggplot2::scale_color_brewer() +
  #     ggplot2::geom_point(data = data.frame(t = t, y = y),
  #                         ggplot2::aes(x = t, y = y, color = factor(d))) +
  #     ggplot2::geom_line(data = geom_line.df,
  #                        ggplot2::aes(x = t, y = y, color = factor(d)))
  # }
  
  bestFitWData <- plotFit(par, withData = TRUE,
                      modus = "DoseDependentRetardedTransientDynamics", 
                      y = data$y, t = data$t, d = data$d)
  
  #############################################################################
  d <- seq(0, max(data[["d"]]), length.out = 1000)
  
  df <- getHillResults(d = d, param = par)
  
  title <- paste0("OptimValue: ", 
                  round(optimObject[["finalModel"]][["value"]], 2),
                  "; ", 
                  paste(names(par), round(par, 4), sep = ": ", collapse = ", "))
  title <- paste(strwrap(title, width = 120), collapse = "\n")
  
  df.long <- reshape2::melt(df, id.vars = c("d"))
  dosisParamPlot <- ggplot2::ggplot(
    df.long, ggplot2::aes(x = d, y = value, color = variable)) +
    ggplot2::geom_line() +
    ggplot2::xlab("Dosis") +
    ggplot2::ylab("Value") +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  #############################################################################
  
  optimResults.optimRes <- lapply(
    optimObject[["finalModel"]][["optimResults"]], function(x) {
    tmp <- unlist(x[grep("optimRes", names(x))], recursive = FALSE)
    names(tmp) <- sub("optimRes.", "", names(tmp))
    tmp
  })
  
  optimResTmpLstValuesAll <- unlist(
    lapply(optimResults.optimRes, function(x) unlist(x[grep("value",names(x))])))
  
  waterfallPlot <- plotWaterfallPlot(optimResTmpLstValuesAll)
  
  #############################################################################
  
  gg <- dosisParamPlot + 
    bestFitWData + 
    waterfallPlot + 
    plot_layout(ncol = 1)
  
  if (saveToFile){
    ggplot2::ggsave(filename = paste0(fileNamePrefix, "_doseResponseFit.pdf"),
                    gg, width = 12, height = 13)
    gg
  } else {
    gg
  }
}