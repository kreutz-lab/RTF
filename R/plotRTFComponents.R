#' Plot t against the components of the RTF
#'
#' @description Plot t against the components of the retarded transient
#' function (RTF)
#' @return Combined plot of scatter plot of  t vs.
#' the components of the retarded transient function
#' (Nonlinear transformation + p_0, sustained signal + p_0,
#' transient signal + p_0, retarded transient function (sustained signal +
#' transient signal + p_0))
#' @param pars Named vector of the values of the model parameters
#' @param data Data frame which needs contain columns 'y' (with quantitative
#' value of outcome) and 't'
#' @param signum_TF Sign of transient function. Positive (1) or negative (-1) sign
#' @param title String of the title of the plot
#' @export plotRTFComponents
#' @import patchwork
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(data,
#'                                          modus = 'RetardedTransientDynamics')
#' signum_TF <- 1
#' optimObject.orig$fixed[["signum_TF"]] <- signum_TF
#' nInitialGuesses <- 50
#' initialGuess.vec.lst <- getInitialGuessVec(
#'   initialGuess.vec = optimObject.orig$initialGuess.vec,
#'   lb.vec = optimObject.orig$lb.vec,
#'   ub.vec = optimObject.orig$ub.vec,
#'   nInitialGuesses = nInitialGuesses)
#' initialGuessResults <- runOptimization(initialGuess.vec.lst,
#'                                       optimObject.orig, objFunct)
#' res.lst <- initialGuessResults[["res.lst"]]
#' bestOptimRes <- initialGuessResults[["bestOptimResult"]]
#' gg.final <- plotRTFComponents(
#'   pars = bestOptimRes$par,
#'   data = optimObject.orig$data,
#'   signum_TF = optimObject.orig$fixed[["signum_TF"]], title = ""
#' )

plotRTFComponents <- function(pars, data, signum_TF, title = "") {
  for (v in 1:length(pars)) assign(names(pars)[v], pars[[v]])
  for (v in 1:ncol(data)) assign(names(data)[v], data[,v])

  alpha <- 0.5

  gg1 <- plotFit(par = c(p_0 = p_0, T_shift = T_shift),
                 y = y, t = t, 
                 plotType = "nonLinearTransformationOnly",
                 withData = TRUE, alpha = alpha)
  gg2 <- plotFit(par = c(tau_1 = tau_1, A_sus = A_sus, p_0 = p_0,
                         T_shift = T_shift, signum_TF = signum_TF),
                 y = y, t = t, 
                 plotType = "sustainedOnly",
                 withData = TRUE, alpha = alpha)
  gg3 <- plotFit(par = c(tau_1 = tau_1, tau_2 = tau_2,
                         A_trans = A_trans,
                         p_0 = p_0, T_shift = T_shift, signum_TF = signum_TF),
                 y = y, t = t, 
                 plotType = "transientOnly",
                 withData = TRUE, alpha = alpha)
  gg4 <- plotFit(par = c(tau_1 = tau_1, tau_2 = tau_2, A_sus = A_sus,
                         A_trans = A_trans, p_0 = p_0, T_shift = T_shift,
                         signum_TF = signum_TF),
                 y = y, t = t,
                 plotType = "all",
                 withData = TRUE, alpha = alpha)

  patchworkObj <- patchwork::wrap_plots(gg1, gg2, gg3, gg4, ncol = 2)

  patchworkObj + patchwork::plot_annotation(
    title = title
  )
}
