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
#' data.doseResponse <- getDoseResponseExampleDf()
#' plot(data.doseResponse)
#' res.doseResponse <- runRTF(data.doseResponse, 
#'                           modus = "DoseDependentRetardedTransientDynamics")
#' plotDoseResponse(res.doseResponse)

plotDoseResponse <- function(optimObject, fileNamePrefix = "") {
  saveToFile <- nchar(fileNamePrefix) > 0
  
  par <- optimObject$finalParams
  for (v in 1:length(par)) assign(names(par)[v], par[[v]])
  
  d <- seq(0, max(optimObject[["finalModel"]][["data"]][["d"]]), length.out = 1000)
  
  A_sus <- hillEquation(d = d, M = M_Asus, h = h_Asus, K = K_Asus)
  A_trans <- hillEquation(d = d, M = M_Atrans, h = h_Atrans, K = K_Atrans)
  tau_1 <- hillEquationReciprocal(d = d, M = M_tau1, 
                                  h = h_tau1, K = K_tau1)
  tau_2 <- hillEquationReciprocal(d = d, M = M_tau2, 
                                  h = h_tau2, K = K_tau2)
  T_shift <- hillEquationReciprocal(d = d, M = M_Tshift, 
                                    h = h_Tshift, K = K_Tshift)
  
  df <- data.frame(d = d,
                   A_sus = A_sus,
                   A_trans = A_trans,
                   tau_1 = tau_1,
                   tau_2 = tau_2,
                   T_shift = T_shift)
  df.long <- reshape2::melt(df, id.vars = c("d"))
  gg <- ggplot2::ggplot(df.long, ggplot2::aes(x = d, y = value, color = variable)) +
    ggplot2::geom_line() +
    ggplot2::xlab("Dosis") +
    ggplot2::ylab("Value") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  optimResults.optimRes <- lapply(optimObject[["finalModel"]][["optimResults"]], function(x) {
    tmp <- unlist(x[grep("optimRes", names(x))], recursive = FALSE)
    names(tmp) <- sub("optimRes.", "", names(tmp))
    tmp
  })
  
  optimResTmpLstValuesAll <- unlist(
    lapply(optimResults.optimRes, function(x) unlist(x[grep("value",names(x))])))
  
  gg <- gg + plotWaterfallPlot(optimResTmpLstValuesAll) +  plot_layout(nrow = 2)
  
  
  if (saveToFile){
    ggplot2::ggsave(filename = paste0(fileNamePrefix, "_doseResponseFit.pdf"),
                    gg, width = 12, height = 13)
  } else {
    gg
  }
}