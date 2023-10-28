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
#' @param d Dosis (obligatory for 'DoseDependentRetardedTransientDynamics')
#' @param title Plot title
#' @export plotFit
#' @examples
#' gg <- plotFit(
#'        par = c(tau_1 = 1.00, tau_2 = 1.00,
#'        A_sus = 1.05, A_trans = 3.05,
#'        p_0 = -0.28, T_shift = -1, signum_TF = 1),
#'        y = c(0.45, 0.96, 1.13, 1.1, 0.9, 0.76, 0.78),
#'        t = c(0, 0.7, 1.2, 1.55, 2.3, 7.45, 10))

plotFit <- function(par,
                    withData = FALSE,
                    modus = "RetardedTransientDynamics", 
                    y = NULL, t = NULL, d = NULL, 
                    title = "") {
  
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
    df.dosis <- NULL
    dosis <- doses[i]
    
    geom_line.lst <- append(geom_line.lst, 
                            list(data.frame(t = xi,
                                            y = getTransientFunctionResult(
                                              t = xi,
                                              d = dosis,
                                              par = par,
                                              modus = modus),
                                            d = rep(dosis, times = length(xi)))))
    
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
                                     ggplot2::aes(x = t, y = y, color = factor(d)))
    }
  } else {
    gg <- gg + ggplot2::geom_line(data = geom_line.df,
                       ggplot2::aes(x = t, y = y))
    if (withData) {
      gg <- gg + ggplot2::geom_point(data = data.frame(t = t, y = y),
                                     ggplot2::aes(x = t, y = y))
    }
  }
  
  if (!is.null(d)) gg <- gg + ggplot2::labs(color='Dosis') 
  if (nchar(title) > 0) gg <- gg + ggplot2::ggtitle(title) 
  
  gg
}
