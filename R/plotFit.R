#' Plot RTF together with experimental data points
#'
#' @description Plot RTF together with experimental data points
#' @return ggplot object showing RTF together with experimental data points
#' @param par Vector containing
#' alpha, gamma, A, B, b, tau, and signum_TF (modus
#' 'timeDependent') or
#' M_alpha, h_alpha,  K_alpha, M_gamma, h_gamma, K_gamma,
#' M_A, h_A, K_A, M_B, h_B, K_B, M_tau, h_tau, K_tau (modus
#' 'doseDependent').
#' @param withData Boolean indicating if data should be added to fit line
#' @param modus Modus ('timeDependent' or
#' 'doseDependent')
#' @param y Experimental outcome for time points (corresponds to y column in
#' experimental data frame)
#' @param t Time points
#' @param d Dose (obligatory for 'doseDependent')
#' @param title Plot title
#' @param pointAlpha Transparency of points
#' @param lineAlpha For modus 'timeDependent': Transparency of lines
#' @param pointSize Point size
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
                    modus = "timeDependent",
                    y = NULL,
                    t = NULL,
                    d = NULL,
                    title = "",
                    pointAlpha = 0.5,
                    lineAlpha = 0.5,
                    pointSize = 0.75) {
  if (is.null(t))
    stop("Please provide vector of time points or maximum time point.")
  
  xi <- seq(min(t), max(t), length.out = 1000)
  
  if (!is.null(d)) {
    doses <- sort(unique(d))
  } else {
    doses <- 1
  }
  
  if (modus == 'timeDependent') {
    RTFResVec <- getTransientFunctionResult(t = xi,
                                            rtfPar = par)
    
    RTFResDf <- data.frame(t = xi,
                           y = RTFResVec,
                           Component = "RTF")
    
    # Only Signal_sus: B = 0
    # Only Signal_trans: A = 0
    parSus <- parTrans <- par
    parSus[names(parSus) == "B"] <-
      parTrans[names(parTrans) == "A"] <- 0
    
    susOnlyResVec <- getTransientFunctionResult(t = xi,
                                                rtfPar = parSus)
    susOnlyResDf <- data.frame(t = xi,
                               y = susOnlyResVec,
                               Component = "Sustained")
    
    transOnlyResVec <- getTransientFunctionResult(t = xi,
                                                  rtfPar = parTrans)
    
    transOnlyResDf <- data.frame(t = xi,
                                 y = transOnlyResVec,
                                 Component = "Transient")
    
    geom_line.df <- do.call("rbind", list(RTFResDf,
                                          susOnlyResDf,
                                          transOnlyResDf))
    
  } else if (modus == 'doseDependent') {
    geom_line.lst <- list()
    for (i in seq(length(doses))) {
      RTFResVec <- NULL
      dose <- doses[i]
      
      # TODO
      RTFResVec <- getTransientFunctionResult(
        t = xi,
        par = par
      )
      
      geom_line.lst <- append(geom_line.lst,
                              list(data.frame(
                                t = xi,
                                y = RTFResVec,
                                # d = rep(dose, times = length(xi))
                                d = dose
                              )))
    }
    
    geom_line.df <- dplyr::bind_rows(geom_line.lst)
  }
  
  gg <- ggplot2::ggplot() +
    ggplot2::theme_bw()
  
  if (length(doses) > 1) {
    gg <- gg + ggplot2::geom_line(data = geom_line.df,
                                  ggplot2::aes(
                                    x = t,
                                    y = y,
                                    color = factor(d)
                                  )) +
      ggplot2::theme(legend.position = "bottom",
                     legend.title = ggplot2::element_blank()) +
      ggplot2::scale_color_brewer()
    if (withData) {
      gg <- gg + ggplot2::geom_point(
        data = data.frame(t = t, y = y),
        ggplot2::aes(
          x = t,
          y = y,
          color = factor(d)
        ),
        alpha = pointAlpha,
        size = pointSize
      )
    }
  } else {
    gg <- gg +
      ggplot2::geom_line(
        data = geom_line.df,
        ggplot2::aes(
          x = t,
          y = y,
          color = factor(Component),
          size = factor(Component)
        ),
        alpha = lineAlpha
      ) +
      ggplot2::scale_colour_manual(values = c(
        RTF = "black",
        Transient = "blue",
        Sustained = "#339999"
      )) +
      ggplot2::scale_size_manual(values = c(
        RTF = 2,
        Transient = 1,
        Sustained = 1
      )) +
      ggplot2::theme(legend.position = "bottom",
                     legend.title = ggplot2::element_blank()) +
      ggplot2::guides(colour = ggplot2::guide_legend(
        override.aes = list(alpha = lineAlpha)))
    if (withData) {
      gg <- gg + ggplot2::geom_point(
        data = data.frame(t = t, y = y),
        ggplot2::aes(x = t, y = y),
        alpha = pointAlpha,
        size = pointSize
      )
    }
  }
  
  if (!is.null(d))
    gg <- gg + ggplot2::labs(color = 'Dose')
  if (nchar(title) > 0)
    gg <- gg + ggplot2::ggtitle(title)
  
  gg
}
