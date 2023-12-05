#' Plot RTF together with experimental data points
#'
#' @description Plot RTF together with experimental data points
#' @return ggplot object showing RTF together with experimental data points
#' @param par Vector containing 
#' alpha, gamma, A, B, b, tau, and signum_TF (modus 
#' 'RetardedTransientDynamics') or 
#' M_alpha, h_alpha,  K_alpha, M_gamma, h_gamma, K_gamma, 
#' M_A, h_A, K_A, M_B, h_B, K_B, M_tau, h_tau, K_tau (modus 
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
#' @param alphaVal Transparency of points
#' @export plotFit
#' @examples
#' gg <- plotFit(
#'        par = c(alpha = 1.00, gamma = 1.00,
#'        A = 1.05, B = 3.05,
#'        b = -0.28, tau = -1, signum_TF = 1),
#'        withData = TRUE,
#'        y = c(0.45, 0.96, 1.13, 1.1, 0.9, 0.76, 0.78),
#'        t = c(0, 0.7, 1.2, 1.55, 2.3, 7.45, 10))

plotFit <- function(par,
                    withData = FALSE,
                    modus = "RetardedTransientDynamics", 
                    y = NULL, t = NULL, d = NULL, 
                    plotType = "all",
                    title = "",
                    alphaVal = 0.5) {
  
  if (is.null(t))
    stop("Please provide vector of time points or maximum time point.")
  
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
      if (nchar(title) == 0) title <- "RTF =  SignalSus + SignalTrans + b"
      
    } else if (plotType == "nonLinearTransformationOnly") {
      functionResVec <- getNonLinTransformationPlusOffset(
        t = xi,
        tau = tau,
        b = b)
      if (nchar(title) == 0) title <- "NonLinTransformation + b"
      
    } else if (plotType == "sustainedOnly") {
      functionResVec <- getSignalSusPlusOffset(
        t = xi,
        alpha = alpha,
        A = A,
        b = b,
        tau = tau,
        signum_TF = signum_TF)
      if (nchar(title) == 0) title <- "SignalSus + b"
      
    } else if (plotType == "transientOnly") {
      functionResVec <-  getSignalTransPlusOffset(
        t = xi,
        alpha = alpha,
        gamma = gamma,
        B = B,
        b = b,
        tau = tau,
        signum_TF = signum_TF)
      if (nchar(title) == 0) title <- "SignalTrans + b"
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
                                     alpha = alphaVal
                                     )
    }
  } else {
    gg <- gg + ggplot2::geom_line(data = geom_line.df,
                       ggplot2::aes(x = t, y = y))
    if (withData) {
      gg <- gg + ggplot2::geom_point(data = data.frame(t = t, y = y),
                                     ggplot2::aes(x = t, y = y),
                                     alpha = alphaVal)
    }
  }
  
  if (!is.null(d)) gg <- gg + ggplot2::labs(color='Dose') 
  if (nchar(title) > 0) gg <- gg + ggplot2::ggtitle(title) 
  
  gg
}
