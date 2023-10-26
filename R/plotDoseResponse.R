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
#' plotData(data.doseResponse)
#' res.doseResponse <- runRTF(data.doseResponse, 
#'                           modus = "DoseDependentRetardedTransientDynamics")
#' plotDoseResponse(res.doseResponse)

plotDoseResponse <- function(optimObject, fileNamePrefix = "") {
  saveToFile <- nchar(fileNamePrefix) > 0
  
  par <- optimObject$finalParams
  for (v in 1:length(par)) assign(names(par)[v], par[[v]])
  
  data <- optimObject[["finalModel"]][["data"]]
  d <- seq(0, max(data[["d"]]), length.out = 1000)
  
  A_sus <- hillEquation(d = d, M = M_Asus, h = h_Asus, K = K_Asus)
  A_trans <- hillEquation(d = d, M = M_Atrans, h = h_Atrans, K = K_Atrans)
  tau_1 <- hillEquationReciprocal(d = d, M = M_tau1, 
                                  h = h_tau1, K = K_tau1, minval = 1e-6)
  tau_2 <- hillEquationReciprocal(d = d, M = M_tau2, 
                                  h = h_tau2, K = K_tau2, minval = 1e-6)
  T_shift <- hillEquationReciprocal(d = d, M = M_Tshift, 
                                    h = h_Tshift, K = K_Tshift)
  
  df <- data.frame(d = d,
                   A_sus = A_sus,
                   A_trans = A_trans,
                   tau_1 = tau_1,
                   tau_2 = tau_2,
                   T_shift = T_shift)
  
  title <- paste0("OptimValue: ", round(optimObject[["finalModel"]][["value"]], 2),
                  "; ", 
                  paste(names(par), round(par, 4), sep = ": ", collapse = ", "))
  title <- paste(strwrap(title, width = 120), collapse = "\n")
  
  df.long <- reshape2::melt(df, id.vars = c("d"))
  gg <- ggplot2::ggplot(df.long, ggplot2::aes(x = d, y = value, color = variable)) +
    ggplot2::geom_line() +
    ggplot2::xlab("Dosis") +
    ggplot2::ylab("Value") +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  optimResults.optimRes <- lapply(optimObject[["finalModel"]][["optimResults"]], function(x) {
    tmp <- unlist(x[grep("optimRes", names(x))], recursive = FALSE)
    names(tmp) <- sub("optimRes.", "", names(tmp))
    tmp
  })
  
  ##################
  
  xi <- seq(0, max(data$t), length.out = 1000)
  
  doses <- sort(unique(data$d))
  
  # library(RColorBrewer)
  # # colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, name = "YlGnBu"))(20)[11:20]
  # colors <- RColorBrewer::brewer.pal(9, name = "YlGnBu")
  # colors <- colors[1:length(doses)]
  
  geom_point.lst <- list()
  geom_line.lst <- list()
  for (i in seq(length(doses))) {
    dosis <- doses[i]
    data.dosis <- data[data$d == dosis,]
    
    A_sus.dosis <- hillEquation(d = dosis, M = M_Asus, h = h_Asus, K = K_Asus)
    A_trans.dosis <- hillEquation(d = dosis, M = M_Atrans, h = h_Atrans, K = K_Atrans)
    tau_1.dosis <- hillEquationReciprocal(d = dosis, M = M_tau1, 
                                    h = h_tau1, K = K_tau1, minval = 1e-6)
    tau_2.dosis <- hillEquationReciprocal(d = dosis, M = M_tau2, 
                                    h = h_tau2, K = K_tau2, minval = 1e-6)
    T_shift.dosis <- hillEquationReciprocal(d = dosis, M = M_Tshift, 
                                      h = h_Tshift, K = K_Tshift)
    
    geom_line.lst <- append(geom_line.lst, list(data.frame(t = xi,
                                             y = getTransientFunctionExampleData(
                                                        t = xi,
                                                        tau_1 = tau_1.dosis,
                                                        tau_2 = tau_2.dosis,
                                                        A_sus = A_sus.dosis,
                                                        A_trans = A_trans.dosis,
                                                        p_0 = p_0,
                                                        T_shift = T_shift.dosis,
                                                        signum_TF = signum_TF),
                                             d = rep(dosis, times = length(xi)))))
     
  } 
 
  geom_line.df <- dplyr::bind_rows(geom_line.lst)
  
  bestFitWData <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::labs(color='Dosis') +
    
    ggplot2::scale_color_brewer() +
    ggplot2::geom_point(data = data,
                        ggplot2::aes(x = t, y = y, color = factor(d))) +
   ggplot2::geom_line(data = geom_line.df,
                      ggplot2::aes(x = t, y = y, color = factor(d)))
  ##################
  
  optimResTmpLstValuesAll <- unlist(
    lapply(optimResults.optimRes, function(x) unlist(x[grep("value",names(x))])))
  
  gg <- gg + 
    bestFitWData + 
    plotWaterfallPlot(optimResTmpLstValuesAll) + 
    plot_layout(ncol = 1)
  
  if (saveToFile){
    ggplot2::ggsave(filename = paste0(fileNamePrefix, "_doseResponseFit.pdf"),
                    gg, width = 12, height = 13)
    gg
  } else {
    gg
  }
}