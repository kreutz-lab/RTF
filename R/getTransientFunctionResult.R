#' @description Apply RTF with defined parameters for defined time points
#' @return Vector of predicted quantitative values resulting from applying RTF
#' with the specified parameters to time points given in t_prime.
#' @param par Named vector of the parameter values used for RTF.
#' @param t_prime Vector of t_prime
#' @param fixed Vector of fixed parameters, which are used to overwrite values
#' in par, if they are non-NAs
#' @param modus String indicating if modus 'RetardedTransientDynamics' or
#' 'ImmediateResponseFunction' should be used
#' @export getTransientFunctionResult
#' @examples
#' par <- c(tau_1 = 1.00768682866629, tau_2 = 1.00768682866629, A_sus = 1.05921992002951,
#'          A_trans = 1.05921992002951, p_0 = -0.28043396979027, T_shift = -1)
#' t_prime <- c(0, 0.714285714285714, 1.42857142857143, 2.14285714285714, 2.85714285714286,
#'              3.57142857142857, 4.28571428571429, 5, 5.71428571428572, 6.42857142857143,
#'              7.14285714285714, 7.85714285714286, 8.57142857142857, 9.28571428571429,
#'              10)
#' fixed <- c(signum_TF = 1, tau_1 = NA, tau_2 = 2.5, A_sus = NA, A_trans = NA,
#'            p_0 = NA, T_shift = NA)
#' modus <- "RetardedTransientDynamics"
#' y <- getTransientFunctionResult(par = par,
#'                            t_prime = t_prime,
#'                            fixed = fixed,
#'                            modus = modus)
#' plot(t_prime, y)

getTransientFunctionResult <- function(par,
                                       t_prime,
                                       fixed,
                                       modus = "RetardedTransientDynamics") {

  for (v in 1:length(par)) assign(names(par)[v], par[[v]])

  # Fixed parameters will overwrite the values in par
  for (v in 1:length(fixed)) {
    if (!is.na(fixed[[v]])) assign(names(fixed)[v], fixed[[v]])
  }

  # if (is.null(t_prime)) t_prime <- data$t_prime
  nonLinTransformation <- log10(10^t_prime+10^T_shift)-log10(1+10^T_shift)

  if (modus == "ImmediateResponseFunction"){
    Signal_sus <- A_sus * (1-exp(- t_prime/tau_1))
    Signal_trans <- A_trans * (1-exp(- t_prime/tau_1))*exp(- t_prime/tau_2)
    transientFunctionRes <-  signum_TF * Signal_sus + signum_TF * Signal_trans + p_0
  } else if (modus == "RetardedTransientDynamics"){
    Signal_sus <- A_sus * (1-exp(- nonLinTransformation /tau_1))
    Signal_trans <- A_trans * (1-exp(- nonLinTransformation /tau_1))* exp(- nonLinTransformation /tau_2)
    transientFunctionRes <- signum_TF * Signal_sus + signum_TF * Signal_trans + p_0
  }
  transientFunctionRes
}
