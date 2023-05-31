#' @description Plot t_prime against the components of the retarded transient
#' function (RTF)
#' @return Combined plot of scatter plot of t plotted vs. t_prime, and t_prime vs.
#' the components of the retarded transient function
#' (Nonlinear transformation + p_0, sustained signal + p_0,
#' transient signal + p_0, retarded transient function (sustained signal +
#' transient signal + p_0))
#' @param pars Named vector of the values of the model parameters
#' @param data Data frame which needs contain columns 'y' (with quantitative
#' value of outcome) and 't_prime' (time scaled to interval [0, 10])
#' @param signum_TF Sign of transient function. Positive (1) or negative (-1) sign
#' @param title String of the title of the plot
#' @export plotModelComponents
#' @examples
#' data <- getExampleDf()
#' data <- scaleTimeCol(data)
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
#' bestOptimRes <- initialGuessResults[["bestOptimRes"]]
#' gg.final <- plotModelComponents(
#'   pars = bestOptimRes$par,
#'   data = optimObject.orig$data,
#'   signum_TF = optimObject.orig$fixed[["signum_TF"]], title = ""
#' )

plotModelComponents <- function(pars, data, signum_TF, title = "") {
  for (v in 1:length(pars)) assign(names(pars)[v], pars[[v]])
  for (v in 1:ncol(data)) assign(names(data)[v], data[,v])

  gg1 <- plotTvsTprime(t = t, t_prime = t_prime)
  gg2 <- plotNonLinearTransformation(p_0 = p_0, T_shift = T_shift,
                                  y = y, t_prime = t_prime)
  gg3 <- plotSignalSus(tau_1 = tau_1, A_sus = A_sus, p_0 = p_0,
                       T_shift = T_shift, signum_TF = signum_TF,
                       y = y, t_prime = t_prime)
  gg4 <- plotSignalTrans(tau_1 = tau_1, tau_2 = tau_2,
                         A_trans = A_trans,
                         p_0 = p_0, T_shift = T_shift, signum_TF = signum_TF,
                         y = y, t_prime = t_prime)
  gg5 <- plotFit(tau_1 = tau_1, tau_2 = tau_2, A_sus = A_sus,
                 A_trans = A_trans, p_0 = p_0, T_shift = T_shift,
                 signum_TF = signum_TF,
                 y = y, t_prime = t_prime)

  # library(patchwork)
  patchwork <- gg1 + gg2 + gg3 + gg4 + gg5 + patchwork::plot_layout(ncol = 2)
  patchwork + patchwork::plot_annotation(
    title = title
  )
}
