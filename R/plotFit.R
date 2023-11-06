#' Plot RTF together with experimental data points
#'
#' @description Plot RTF together with experimental data points
#' @return ggplot object showing RTF together with experimental data points
#' @param par Vector containing 
#' tau_1, tau_2, A_sus, A_trans, p_0, T_shift, and signum_TF (modus 
#' 'RetardedTransientDynamics') or 
#' M_tau1, h_tau1,  K_tau1, M_tau2, h_tau2, K_tau2, M_Asus, h_Asus, K_Asus, 
#' M_Atrans, h_Atrans, K_Atrans, M_Tshift, h_Tshift, K_Tshift (modus 
#' 'DoseDependentRetardedTransientDynamics').
#' @param withData Boolean indicating if data should be added to fit line
#' @param modus Modus ('RetardedTransientDynamics' or 
#' DoseDependentRetardedTransientDynamics')
#' @param y Experimental outcome for time points (corresponds to y column in
#' experimental data frame)
#' @param t Time points
#' @param d Dose (obligatory for 'DoseDependentRetardedTransientDynamics')
#' @param plotType String indication if transient function should be plotted
#' ('all') (Default) or if in case of modus = 'RetardedTransientDynamics'
#'  components of transient function ("nonLinearTransformationOnly", 
#' "transientOnly", "sustainedOnly") should be plotted.
#' @param title Plot title
#' @param alpha Transparency of points
#' @export plotFit
#' @examples
#' gg <- plotFit(
#'        par = c(tau_1 = 1.00, tau_2 = 1.00,
#'        A_sus = 1.05, A_trans = 3.05,
#'        p_0 = -0.28, T_shift = -1, signum_TF = 1),
#'        withData = TRUE,
#'        y = c(0.45, 0.96, 1.13, 1.1, 0.9, 0.76, 0.78),
#'        t = c(0, 0.7, 1.2, 1.55, 2.3, 7.45, 10))

plotFit <- function(par,
                    withData = FALSE,
                    modus = "RetardedTransientDynamics", 
                    y = NULL, t = NULL, d = NULL, 
                    plotType = "all",
                    title = "",
                    alpha = 0.5) {
  
  for (v in 1:length(par)) assign(names(par)[v], par[[v]])
  xi <- seq(0, max(t), length.out = 1000)
  
  if (!is.null(d)) {
    doses <- sort(unique(d))
  } else {
    doses <- 1
  }

  # for (v in 1:length(par)) assign(names(par)[v], par[[v]])
  
  geom_point.lst <- list()
  geom_line.lst <- list()
  for (i in seq(length(doses))) {
    functionResVec <- NULL
    dose <- doses[i]
    
    if (plotType == "all" | modus == 'DoseDependentRetardedTransientDynamics') {
      functionResVec <- getTransientFunctionResult(
        t = xi,
        d = dose,
        par = par,
        modus = modus)
      if (nchar(title) == 0) title <- "RTF =  SignalSus + SignalTrans + p_0"
      
    } else if (plotType == "nonLinearTransformationOnly") {
      functionResVec <- getNonLinTransformationPlusOffset(
        t = xi,
        T_shift = T_shift,
        p_0 = p_0)
      if (nchar(title) == 0) title <- "NonLinTransformation + p_0"
      
    } else if (plotType == "sustainedOnly") {
      functionResVec <- getSignalSusPlusOffset(
        t = xi,
        tau_1 = tau_1,
        A_sus = A_sus,
        p_0 = p_0,
        T_shift = T_shift,
        signum_TF = signum_TF)
      if (nchar(title) == 0) title <- "SignalSus + p_0"
      
    } else if (plotType == "transientOnly") {
      functionResVec <-  getSignalTransPlusOffset(
        t = xi,
        tau_1 = tau_1,
        tau_2 = tau_2,
        A_trans = A_trans,
        p_0 = p_0,
        T_shift = T_shift,
        signum_TF = signum_TF)
      if (nchar(title) == 0) title <- "SignalTrans + p_0"
    }
    
    geom_line.lst <- append(geom_line.lst, 
                            list(data.frame(t = xi,
                                            y = functionResVec,
                                            d = rep(dose, times = length(xi)))))
    
  } 
  
  geom_line.df <- dplyr::bind_rows(geom_line.lst)
  
  gg <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::scale_color_brewer() 
  
  if (length(doses) > 1) {
    gg <- gg + ggplot2::geom_line(data = geom_line.df,
                       ggplot2::aes(x = t, y = y, color = factor(d)))
    if (withData) {
      gg <- gg + ggplot2::geom_point(data = data.frame(t = t, y = y),
                                     ggplot2::aes(x = t, y = y, 
                                                  color = factor(d)),
                                     alpha = alpha
                                     )
    }
  } else {
    gg <- gg + ggplot2::geom_line(data = geom_line.df,
                       ggplot2::aes(x = t, y = y))
    if (withData) {
      gg <- gg + ggplot2::geom_point(data = data.frame(t = t, y = y),
                                     ggplot2::aes(x = t, y = y),
                                     alpha = alpha)
    }
  }
  
  if (!is.null(d)) gg <- gg + ggplot2::labs(color='Dose') 
  if (nchar(title) > 0) gg <- gg + ggplot2::ggtitle(title) 
  
  gg
}
