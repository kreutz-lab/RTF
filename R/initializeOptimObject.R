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
  
  # data <- data[order(data$t),]
  for (v in 1:ncol(data)) assign(names(data)[v], data[,v])
  
  t <- sort(t[!is.na(t)])
  y <- y[!is.na(y)]
  # New variables: 
  # d, M_A, h_A, K_A, M_B, h_B, K_B, M_alpha, h_alpha, K_alpha, M_gamma, h_gamma, K_gamma, M_tau, h_tau, K_tau
  
  # d Dose
  # M Maximum value
  # h Hill coefficient
  # K Half-maximum quantity
  
  if (modus == "DoseDependentRetardedTransientDynamics") {
    if (!("d" %in% names(data))) {
      stop("Please provide data frame with data column d.")
    }

    d <- d[!is.na(d)]
    hillCoef.lb <- 1
    hillCoef.ub <- 10
    K.lb <- min(d[d > 0]) / 10
    K.ub <- max(d) * 10
    lb.vec <- c(M_alpha = 1 / (2 * (max(t) - min(t))),
                h_alpha = hillCoef.lb,
                K_alpha = K.lb,
                M_gamma = 1 / (2 * (max(t) - min(t))),
                h_gamma = hillCoef.lb,
                K_gamma = K.lb,
                M_A = 0,
                h_A = hillCoef.lb,
                K_A = K.lb,
                M_B = 0,
                h_B = hillCoef.lb,
                K_B = K.lb,
                M_tau = -(max(t) - min(t)) / 5,
                h_tau = hillCoef.lb,
                K_tau = K.lb,
                b = min(y),
                sigma = max(1e-8, diff(range(y)) / (10^4)))

    ub.vec <- c(M_alpha = 2 / (min(diff(unique(t)))),
                h_alpha = hillCoef.ub,
                K_alpha = K.ub,
                M_gamma = 2 / (min(diff(unique(t)))),
                h_gamma = hillCoef.ub,
                K_gamma = K.ub,
                M_A = 10 * (max(y) - min(y)),
                h_A = hillCoef.ub,
                K_A = K.ub,
                M_B = 10 * (max(y) - min(y)),
                h_B = hillCoef.ub,
                K_B = K.ub,
                M_tau = (max(t) - min(t)) * 0.9,
                h_tau = hillCoef.ub,
                K_tau = K.ub,
                b = max(y),
                sigma = stats::sd(y, na.rm = TRUE))

    initialGuess.vec <- c(M_alpha =  0.5 * lb.vec[["M_alpha"]] +
  0.5 * ub.vec[["M_alpha"]],
                          h_alpha = 0.5 * lb.vec[["h_alpha"]] +
    0.5 * ub.vec[["h_alpha"]],
                          K_alpha =  0.5 * lb.vec[["K_alpha"]] +
    0.5 * ub.vec[["K_alpha"]],
                          M_gamma = 0.5 * lb.vec[["M_gamma"]] +
  0.5 * ub.vec[["M_gamma"]],
                          h_gamma = 0.5 * lb.vec[["h_gamma"]] +
    0.5 * ub.vec[["h_gamma"]],
                          K_gamma = 0.5 * lb.vec[["K_gamma"]] +
    0.5 * ub.vec[["K_gamma"]],
                          M_A =  0.1 * lb.vec[["M_A"]] +
  0.9 * ub.vec[["M_A"]],
                          h_A = 0.5 * lb.vec[["h_A"]] +
    0.5 * ub.vec[["h_A"]],
                          K_A = 0.5 * lb.vec[["K_A"]] +
    0.5 * ub.vec[["K_A"]],
                          M_B = 0.1*lb.vec[["M_B"]] +
  0.9 * ub.vec[["M_B"]],
                          h_B = 0.5 * lb.vec[["h_B"]] +
    0.5 * ub.vec[["h_B"]],
                          K_B = 0.5 * lb.vec[["K_B"]] +
    0.5 * ub.vec[["K_B"]],
                          M_tau = -(max(t) - min(t)) / 10,
                          h_tau = 0.5 * lb.vec[["h_tau"]] +
    0.5 * ub.vec[["h_tau"]],
                          K_tau = 0.5 * lb.vec[["K_tau"]] +
    0.5 * ub.vec[["K_tau"]],
                          b = 0.5 * lb.vec[["b"]] +
    0.5 * ub.vec[["b"]],
                          sigma = max(lb.vec["sigma"], 0.1 * diff(range(y))))
  } else {
    # Define Lower and Upper bounds, and Default initial guess
    lb.vec <- c(alpha = 1 / (2 * (max(t) - min(t))),
                gamma = 1 / (2 * (max(t) - min(t))),
                A = 0,
                B = 0,
                b = min(y),
                tau = -(max(t) - min(t)) / 5,
                sigma = max(1e-8, diff(range(y)) / (10^4)))
    
    ub.vec <- c(alpha = 2 / (min(diff(unique(t)))),
                gamma = 2 / (min(diff(unique(t)))),
                A = 2 * (max(y) - min(y)),
                B = 2 * (max(y) - min(y)),
                b = max(y),
                tau = (max(t) - min(t)) * 0.9,
                sigma = stats::sd(y, na.rm = TRUE))
    
    initialGuess.vec <- c(alpha = 0.5 * lb.vec[["alpha"]] +
                            0.5 * ub.vec[["alpha"]],
                          gamma = 0.5 * lb.vec[["gamma"]] +
                            0.5 * ub.vec[["gamma"]],
                          A = 0.1 * lb.vec[["A"]] +
                            0.9 * ub.vec[["A"]],
                          B = 0.1 * lb.vec[["B"]] +
                            0.9 * ub.vec[["B"]],
                          b = 0.5 * lb.vec[["b"]] +
                            0.5 * ub.vec[["b"]],
                          tau = -(max(t) - min(t)) / 10,
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
                           fixed = stats::setNames(
                             rep(NA, length(lb.vec)), names(lb.vec)),
                           takeLog10 = stats::setNames(
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
    optimObject.orig[["takeLog10"]][positive.par.names] <- TRUE
    # optimObject.orig <- takeLog10OfBounds(optimObject.orig)
  }
  optimObject.orig
}
