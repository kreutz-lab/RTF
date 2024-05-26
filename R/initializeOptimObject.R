#' Initializes OptimObject
#'
#' @description Initializes OptimObject
#' @return Initialized OptimObject, which is a list containing input data frame
#' with time-resolved data ('data'), the vector of initial guesses
#' ('initialGuess.vec'), of lower bounds ('lb.vec'), of upper bounds ('ub.vec'),
#' vector of fixed parameters ('fixed'), if log10 is applied to bounds
#' ('takeLog10'), the parameters having no negative values in initialGuess.vec,
#' lb.vec, and ub.vec ('positive.par.names'), modus ('modus'),
#' log-likelihood function for the parameter optimization ('optimFunction'),
#' if the sign of the sustained and transient RTF part should be equal
#' ('sameSign'), list of control parameters passed to stats::optim ('control'),
#' and a list of values of fitted parameters ('fitted', can be empty).
#' @param data Data frame containing columns named 't' (time) and 'y'
#' (quantitative value).
#' @param modus String indicating if modus 'singleDose' or 'doseDependent'
#' should be used.
#' @param optimFunction String indicating the optimization function which
#' should be used (Default: "logLikelihood").
#' @param control List of control arguments passed to the function stats::optim
#' (Default: list(trace = 1, maxit = 1000, factr = 1e7)).
#' @param takeLog10 Boolean value indicating if log10 of bounds should be
#' applied.
#' @param sameSign Boolean indicating if sign of sustained RTF part
#' (signumTF_sus) and transient RTF part (signumTF_trans) should be equal
#' (Default: TRUE).
#' @export initializeOptimObject
#' @examples
#' # Modus: Single-dose RTF
#' data <- getSimData()
#' optimObject.singleDose <- initializeOptimObject(data, modus = "singleDose")
#'
#' # Modus: Dose-dependent RTF
#' data.doseResponse <- getSimData("doseDependent")
#' optimObject.doseDep <- initializeOptimObject(data.doseResponse,
#'     modus = "doseDependent"
#' )
initializeOptimObject <- function(data, modus, optimFunction = "logLikelihood",
                                  control = list(
                                      trace = 1, maxit = 1000,
                                      factr = 1e7
                                  ),
                                  takeLog10 = TRUE,
                                  sameSign = TRUE) {
    data.orig <- data
    data <- data[stats::complete.cases(data), ]

    mintVal <- min(data$t)
    if (mintVal > 0) {
        warning("Minimum time point of dataset is larger than 0.
            To make the time course start at 0, minimum time point will be
            subtracted from all time points of dataset.")
        data$t <- data$t - mintVal
    }

    # data <- data[order(data$t),]
    for (v in 1:ncol(data)) assign(names(data)[v], data[, v])

    t <- sort(t[!is.na(t)])
    y <- y[!is.na(y)]

    # d Dose
    # M Maximum value
    # h Hill coefficient
    # K Half-maximum quantity
    if (modus == "doseDependent") {
        if (!("d" %in% names(data))) {
            stop("Please provide data frame with data column d.")
        }

        d <- d[!is.na(d)]
        hillCoef.lb <- 1
        hillCoef.ub <- 10
        K.lb <- min(d[d > 0]) / 10
        K.ub <- max(d) * 10
        lb.vec <- c(
            M_alpha = 1 / (100 * (max(t) - min(t))),
            h_alpha = hillCoef.lb,
            K_alpha = K.lb,
            M_beta = 1 / (100 * (max(t) - min(t))),
            h_beta = hillCoef.lb,
            K_beta = K.lb,
            M_gamma = 1 / (100 * (max(t) - min(t))),
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
            sigma = min(
                max(1e-10, stats::sd(y, na.rm = TRUE)),
                max(1e-10, diff(range(y)) / (10^3))
            )
        )

        ub.vec <- c(
            M_alpha = 2 / (min(diff(unique(t)))),
            h_alpha = hillCoef.ub,
            K_alpha = K.ub,
            M_beta = 2 / (min(diff(unique(t)))),
            h_beta = hillCoef.ub,
            K_beta = K.ub,
            M_gamma = 2 / (min(diff(unique(t)))),
            h_gamma = hillCoef.ub,
            K_gamma = K.ub,
            M_A = 10 * (max(y[t == max(t)]) - min(y[t == max(t)])),
            h_A = hillCoef.ub,
            K_A = K.ub,
            M_B = 10 * (max(y) - min(y)),
            h_B = hillCoef.ub,
            K_B = K.ub,
            M_tau = (max(t) - min(t)) * 0.9,
            h_tau = hillCoef.ub,
            K_tau = K.ub,
            b = max(y),
            sigma = max(1e-10, stats::sd(y, na.rm = TRUE))
        )

        initialGuess.vec <- c(
            M_alpha = sqrt(lb.vec[["M_alpha"]] * ub.vec[["M_alpha"]]),
            h_alpha = sqrt(lb.vec[["h_alpha"]] * ub.vec[["h_alpha"]]),
            K_alpha = sqrt(lb.vec[["K_alpha"]] * ub.vec[["K_alpha"]]),
            M_beta = sqrt(lb.vec[["M_beta"]] * ub.vec[["M_beta"]]),
            h_beta = sqrt(lb.vec[["h_beta"]] * ub.vec[["h_beta"]]),
            K_beta = sqrt(lb.vec[["K_beta"]] * ub.vec[["K_beta"]]),
            M_gamma = sqrt(lb.vec[["M_gamma"]] * ub.vec[["M_gamma"]]),
            h_gamma = sqrt(lb.vec[["h_gamma"]] * ub.vec[["h_gamma"]]),
            K_gamma = sqrt(lb.vec[["K_gamma"]] * ub.vec[["K_gamma"]]),
            M_A = 0.1 * lb.vec[["M_A"]] + 0.9 * ub.vec[["M_A"]],
            h_A = 0.5 * lb.vec[["h_A"]] + 0.5 * ub.vec[["h_A"]],
            K_A = 0.5 * lb.vec[["K_A"]] + 0.5 * ub.vec[["K_A"]],
            M_B = 0.1 * lb.vec[["M_B"]] + 0.9 * ub.vec[["M_B"]],
            h_B = 0.5 * lb.vec[["h_B"]] + 0.5 * ub.vec[["h_B"]],
            K_B = 0.5 * lb.vec[["K_B"]] + 0.5 * ub.vec[["K_B"]],
            M_tau = -(max(t) - min(t)) / 10,
            h_tau = 0.5 * lb.vec[["h_tau"]] + 0.5 * ub.vec[["h_tau"]],
            K_tau = 0.5 * lb.vec[["K_tau"]] + 0.5 * ub.vec[["K_tau"]],
            b = 0.5 * lb.vec[["b"]] + 0.5 * ub.vec[["b"]],
            sigma = 0.5 * lb.vec[["sigma"]] + 0.5 * ub.vec[["sigma"]]
        )
    } else {
        # Define Lower and Upper bounds, and Default initial guess
        lb.vec <- c(
            alpha = 1 / (100 * (max(t) - min(t))),
            beta = 1 / (100 * (max(t) - min(t))),
            gamma = 1 / (100 * (max(t) - min(t))),
            A = 0,
            B = 0,
            b = min(y),
            tau = -(max(t) - min(t)) / 5,
            sigma = min(
                max(1e-10, stats::sd(y, na.rm = TRUE)),
                max(1e-10, diff(range(y)) / (10^3))
            )
        )

        ub.vec <- c(
            alpha = 2 / (min(diff(unique(t)))),
            beta = 2 / (min(diff(unique(t)))),
            gamma = 2 / (min(diff(unique(t)))),
            A = 2 * (max(y) - min(y)),
            B = 2 * (max(y) - min(y)),
            b = max(y),
            tau = (max(t) - min(t)) * 0.9,
            sigma = max(1e-10, stats::sd(y, na.rm = TRUE))
        )

        initialGuess.vec <- c(
            alpha = sqrt(lb.vec[["alpha"]] *
                ub.vec[["alpha"]]),
            beta = sqrt(lb.vec[["beta"]] *
                ub.vec[["beta"]]),
            gamma = sqrt(lb.vec[["gamma"]] *
                ub.vec[["gamma"]]),
            A = 0.1 * lb.vec[["A"]] +
                0.9 * ub.vec[["A"]],
            B = 0.1 * lb.vec[["B"]] +
                0.9 * ub.vec[["B"]],
            b = 0.5 * lb.vec[["b"]] +
                0.5 * ub.vec[["b"]],
            tau = -(max(t) - min(t)) / 10,
            sigma = 0.5 * lb.vec[["sigma"]] +
                0.5 * ub.vec[["sigma"]]
        )
    }

    if ("sigmaExp" %in% colnames(data)) {
        lb.vec <- lb.vec[names(lb.vec) != "sigma"]
        ub.vec <- ub.vec[names(ub.vec) != "sigma"]
        initialGuess.vec <- initialGuess.vec[names(initialGuess.vec) != "sigma"]
    }

    optimObject.orig <- list(
        data = data.orig,
        initialGuess.vec = initialGuess.vec,
        lb.vec = lb.vec,
        ub.vec = ub.vec,
        fixed = c(
            stats::setNames(
                rep(NA, length(lb.vec)), names(lb.vec)
            ),
            signumTF_sus = 1,
            signumTF_trans = 1
        ),
        takeLog10 = stats::setNames(
            rep(FALSE, length(lb.vec)), names(lb.vec)
        ),
        positive.par.names = NULL,
        modus = modus,
        optimFunction = optimFunction,
        sameSign = sameSign,
        control = control,
        fitted = list()
    )

    if (takeLog10) {
        positive.par.names <- getPositiveParNames(
            lb.vec = optimObject.orig$lb.vec,
            ub.vec = optimObject.orig$ub.vec,
            initialGuess.vec = optimObject.orig$initialGuess.vec
        )
        positive.par.names <- setdiff(positive.par.names, "sigma")
        optimObject.orig$positive.par.names <- positive.par.names
        optimObject.orig[["takeLog10"]][positive.par.names] <- TRUE
    }
    optimObject.orig
}
