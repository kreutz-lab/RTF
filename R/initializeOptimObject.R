#' Initializes OptimObject
#'
#' @description Initializes OptimObject
#' @return Initialized OptimObject, which is a list containing input data frame
#'  with time resolved data ('data'),
#' the vector of initial guesses ('initialGuess.vec'), of lower bounds ('lb.vec'),
#' of upper bounds ('ub.vec'), vector of fixed parameters ('fixed'),
#' if log10 is applied to bounds ('takeLog10'), the parameters having no
#' negative values in initialGuess.vec, lb.vec, and ub.vec ('positive.par.names'),
#' modus ('modus'), and a list of values of fitted parameters ('fitted')
#' @param data Data frame containing columns named 't' (time) and 'y'
#' (quantitative value)
#' @param modus String indicating if modus 'RetardedTransientDynamics' or
#' 'ImmediateResponseFunction' should be used
#' @param optimFunction String indicating the optimization function which 
#' should be used (Default: "chiSquare")
#' @param control List of control arguments passed to the function stats::optim 
#' (Default: list(trace = 1, maxit = 1000, factr = 1.0e-20))
#' @param takeLog10 Boolean value indicating if log10 of bounds should be applied
#' @export initializeOptimObject
#' @examples
#' data <- getExampleDf()
#' optimObject <- initializeOptimObject(data, modus = 'RetardedTransientDynamics')

initializeOptimObject <- function(data, modus, optimFunction = "chiSquare", 
                                  control = list(trace = 1, maxit = 1000,
                                                 factr = 1.0e-20), 
                                  takeLog10 = TRUE) {
  for (v in 1:ncol(data)) assign(names(data)[v], data[,v])

  # New variables: 
  # d, M_Asus, h_Asus, K_Asus, M_Atrans, h_Atrans, K_Atrans, M_tau1, h_tau1, K_tau1, M_tau2, h_tau2, K_tau2, M_Tshift, h_Tshift, K_Tshift
  
  # d Dose
  # M Maximum value
  # h Hill coefficient
  # K Half-maximum quantity
  # A_sus <- hillEquation(d = d, M = M_Asus, h = h_Asus, K = K_Asus)
  # A_trans <- hillEquation(d = d, M = M_Atrans, h = h_Atrans, K = K_Atrans)
  # tau_1 <- hillEquationReciprocal(d = d, M = M_tau1, 
  #                                 h = h_tau1, K = K_tau1)
  # tau_2 <- hillEquationReciprocal(d = d, M = M_tau2, 
  #                                 h = h_tau2, K = K_tau2)
  # T_shift <- hillEquationReciprocal(d = d, M = M_Tshift, 
  #                                   h = h_Tshift, K = K_Tshift)
  
  if (modus == "DoseDependentRetardedTransientDynamics") {
    if (!("d" %in% names(data))) {
      stop("Please provide data frame with data column d.")
    }

    hillCoef.lb <- 1
    hillCoef.ub <- 10
    K.lb <- min(d[d > 0]) / 10
    K.ub <- max(d) / 10
    lb.vec <- c(M_tau1 = min(diff(unique(t))) / 2,
                h_tau1 = hillCoef.lb,
                K_tau1 = K.lb,
                M_tau2 = min(diff(unique(t))) / 2,
                h_tau2 = hillCoef.lb,
                K_tau2 = K.lb,
                M_Asus = 0,
                h_Asus = hillCoef.lb,
                K_Asus = K.lb,
                M_Atrans = 0,
                h_Atrans = hillCoef.lb,
                K_Atrans = K.lb,
                M_Tshift = -(max(t) - min(t)) / 5,
                h_Tshift = hillCoef.lb,
                K_Tshift = K.lb,
                p_0 = min(y),
                sigma = max(1e-8, diff(range(y)) / (10^4)))

    ub.vec <- c(M_tau1 = 2 * (max(t) - min(t)),
                h_tau1 = hillCoef.ub,
                K_tau1 = K.ub,
                M_tau2 = 2 * (max(t) - min(t)),
                h_tau2 = hillCoef.ub,
                K_tau2 = K.ub,
                M_Asus = 2 * (max(y) - min(y)),
                h_Asus = hillCoef.ub,
                K_Asus = K.ub,
                M_Atrans = 2 * (max(y) - min(y)),
                h_Atrans = hillCoef.ub,
                K_Atrans = K.ub,
                M_Tshift = (max(t) - min(t)) / 2,
                h_Tshift = hillCoef.ub,
                K_Tshift = K.ub,
                p_0 = max(y),
                sigma = stats::sd(y, na.rm = TRUE))

    initialGuess.vec <- c(M_tau1 =  0.5 * lb.vec[["M_tau1"]] +
  0.5 * ub.vec[["M_tau1"]],
                          h_tau1 = 0.5 * lb.vec[["h_tau1"]] +
    0.5 * ub.vec[["h_tau1"]],
                          K_tau1 =  0.5 * lb.vec[["K_tau1"]] +
    0.5 * ub.vec[["K_tau1"]],
                          M_tau2 = 0.5 * lb.vec[["M_tau2"]] +
  0.5 * ub.vec[["M_tau2"]],
                          h_tau2 = 0.5 * lb.vec[["h_tau2"]] +
    0.5 * ub.vec[["h_tau2"]],
                          K_tau2 = 0.5 * lb.vec[["K_tau2"]] +
    0.5 * ub.vec[["K_tau2"]],
                          M_Asus =  0.1 * lb.vec[["M_Asus"]] +
  0.9 * ub.vec[["M_Asus"]],
                          h_Asus = 0.5 * lb.vec[["h_Asus"]] +
    0.5 * ub.vec[["h_Asus"]],
                          K_Asus = 0.5 * lb.vec[["K_Asus"]] +
    0.5 * ub.vec[["K_Asus"]],
                          M_Atrans = 0.1*lb.vec[["M_Atrans"]] +
  0.9 * ub.vec[["M_Atrans"]],
                          h_Atrans = 0.5 * lb.vec[["h_Atrans"]] +
    0.5 * ub.vec[["h_Atrans"]],
                          K_Atrans = 0.5 * lb.vec[["K_Atrans"]] +
    0.5 * ub.vec[["K_Atrans"]],
                          M_Tshift = -(max(t) - min(t)) / 10,
                          h_Tshift = 0.5 * lb.vec[["h_Tshift"]] +
    0.5 * ub.vec[["h_Tshift"]],
                          K_Tshift = 0.5 * lb.vec[["K_Tshift"]] +
    0.5 * ub.vec[["K_Tshift"]],
                          p_0 = 0.5 * lb.vec[["p_0"]] +
    0.5 * ub.vec[["p_0"]],
                          sigma = max(lb.vec["sigma"], 0.1 * diff(range(y))))
  } else {
    # Define Lower and Upper bounds, and Default initial guess
    lb.vec <- c(tau_1 = min(diff(unique(t))) / 2, # minimal sampling interval
                tau_2 = min(diff(unique(t))) / 2, # minimal sampling interval
                A_sus = 0,
                A_trans = 0,
                p_0 = min(y),
                T_shift = -(max(t) - min(t)) / 5,
                sigma = max(1e-8, diff(range(y)) / (10^4)))
    
    ub.vec <- c(tau_1 = 2 * (max(t) - min(t)),
                tau_2 = 2 * (max(t) - min(t)),
                A_sus = 2 * (max(y) - min(y)),
                A_trans = 2 * (max(y) - min(y)),
                p_0 = max(y),
                T_shift = (max(t) - min(t)) / 2,
                sigma = stats::sd(y, na.rm = TRUE))
    
    initialGuess.vec <- c(tau_1 = 0.5 * lb.vec[["tau_1"]] +
                            0.5 * ub.vec[["tau_1"]],
                          tau_2 = 0.5 * lb.vec[["tau_2"]] +
                            0.5 * ub.vec[["tau_2"]],
                          A_sus = 0.1 * lb.vec[["A_sus"]] +
                            0.9 * ub.vec[["A_sus"]],
                          A_trans = 0.1*lb.vec[["A_trans"]] +
                            0.9 * ub.vec[["A_trans"]],
                          p_0 = 0.5 * lb.vec[["p_0"]] +
                            0.5 * ub.vec[["p_0"]],
                          T_shift = -(max(t) - min(t)) / 10,
                          sigma = max(lb.vec["sigma"], 0.1 * diff(range(y))))
    
  }

  if ("sdExp" %in% colnames(data)){
    lb.vec <- lb.vec[names(lb.vec) != "sigma"]
    ub.vec <- ub.vec[names(ub.vec) != "sigma"]
    initialGuess.vec <- initialGuess.vec[names(initialGuess.vec) != "sigma"]
  }

  optimObject.orig <- list(data = data,
                           initialGuess.vec = initialGuess.vec,
                           lb.vec = lb.vec,
                           ub.vec = ub.vec,
                           fixed = setNames(
                             rep(NA, length(lb.vec)), names(lb.vec)),
                           takeLog10 = setNames(
                             rep(FALSE, length(lb.vec)), names(lb.vec)),
                           positive.par.names = NULL,
                           modus = modus,
                           optimFunction = optimFunction,
                           control = control,
                           fitted = list()
  )

  if (takeLog10) {
    positive.par.names <- getPositiveParNames(
      lb.vec = optimObject.orig$lb.vec,
      ub.vec = optimObject.orig$ub.vec,
      initialGuess.vec = optimObject.orig$initialGuess.vec)
    positive.par.names <- setdiff(positive.par.names, "sigma")
    optimObject.orig$positive.par.names <- positive.par.names
    optimObject.orig <- takeLog10OfBounds(optimObject.orig)
  }
  optimObject.orig
}
