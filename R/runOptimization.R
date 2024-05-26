#' Run optimization
#'
#' @description Run optimization using stats::optim with method = "L-BFGS-B".
#' @return List of with optimization results ('optimResults') and best
#' optimization result ('bestOptimResult').
#' @param initialGuess.vec.lst List of default initial guess for each model
#' parameter a defined number of further initial guesses lying between the lower
#' and upper bound of each model parameter.
#' @param optimObject optimObject, which is a list containing input data frame
#' with time-resolved data ('data'), the vector of initial guesses
#' ('initialGuess.vec'), of lower bounds ('lb.vec'), of upper bounds ('ub.vec'),
#' vector of fixed parameters ('fixed'), if log10 is applied to bounds
#' ('takeLog10'), the parameters having no negative values in initialGuess.vec,
#' lb.vec, and ub.vec ('positive.par.names'), modus ('modus'),
#' log-likelihood function for the parameter optimization ('optimFunction'), if the
#' sign of the sustained and transient RTF part should be equal ('sameSign'),
#' list of control parameters passed to stats::optim ('control'), and a list of
#' values of fitted parameters ('fitted', can be empty).
#' @param objFunct Name of the objective function.
#' @export runOptimization
#' @examples
#' data <- getSimData()
#' optimObject.orig <- initializeOptimObject(data, modus = "singleDose")
#' initialGuess.vec.lst <- getInitialGuessVecLst(
#'     initialGuess.vec =
#'         optimObject.orig$initialGuess.vec,
#'     lb.vec = optimObject.orig$lb.vec,
#'     ub.vec = optimObject.orig$ub.vec,
#'     takeLog10 = optimObject.orig$takeLog10,
#'     nInitialGuesses = 50
#' )
#' res <- runOptimization(initialGuess.vec.lst, optimObject.orig, objFunct)
runOptimization <- function(initialGuess.vec.lst, optimObject, objFunct) {
    currentBestResValue <- NULL
    RTFmodelLst <- list()
    vecOrder <- names(optimObject$fixed)

    ############################################################################
    # Bring y data to range 0-10 and scale parameters accordingly,
    # as optimization works better on higher values
    yDependentPars <- c("A", "B", "b", "sigma", "M_A", "M_B")
    scaleFactor <- 10 / max(optimObject$data$y, na.rm = TRUE)
    optimObject$data$y <- optimObject$data$y * scaleFactor

    if ("sigmaExp" %in% colnames(optimObject$data)) {
        optimObject$data$sigmaExp <-
            optimObject$data$sigmaExp * scaleFactor
    }

    optimObject$lb.vec[names(optimObject$lb.vec) %in% yDependentPars] <-
        optimObject$lb.vec[names(optimObject$lb.vec)
        %in% yDependentPars] * scaleFactor

    optimObject$ub.vec[names(optimObject$ub.vec) %in% yDependentPars] <-
        optimObject$ub.vec[names(optimObject$ub.vec)
        %in% yDependentPars] * scaleFactor

    optimObject$fixed[names(optimObject$fixed) %in% yDependentPars] <-
        optimObject$fixed[names(optimObject$fixed)
        %in% yDependentPars] * scaleFactor

    for (i in seq(length(initialGuess.vec.lst))) {
        initialGuess.vec.lst[[i]][
            names(initialGuess.vec.lst[[i]]) %in% yDependentPars
        ] <-
            initialGuess.vec.lst[[i]][
                names(initialGuess.vec.lst[[i]]) %in% yDependentPars
            ] *
                scaleFactor
    }
    ############################################################################

    paramsToBeFitted <- names(initialGuess.vec.lst[[1]])
    pars.tmp <- c()
    # Remove each fixedParam from vec, optimObject$lb.vec,
    # and optimObject$ub.vec
    for (el in paramsToBeFitted) {
        #       c("alpha", "beta", "gamma", "A", "B", "b", "tau", "sigma")) {
        if (!is.na(optimObject$fixed[[el]])) {
            nam <- names(pars.tmp)
            pars.tmp <- c(pars.tmp, optimObject$fixed[[el]])
            names(pars.tmp) <- c(nam, el)
            optimObject$lb.vec <- optimObject$lb.vec[
                -which(names(optimObject$lb.vec) == el)
            ]
            optimObject$ub.vec <- optimObject$ub.vec[
                -which(names(optimObject$ub.vec) == el)
            ]

            # remove from each sublist in initialGuess.vec.lst
            initialGuess.vec.lst <- lapply(
                initialGuess.vec.lst, function(x) x[-which(names(x) == el)]
            )
        }
    }

    takeLog10 <- optimObject$takeLog10

    lower <- applyLog10ForTakeLog10(optimObject$lb.vec, takeLog10)
    upper <- applyLog10ForTakeLog10(optimObject$ub.vec, takeLog10)

    # Take logarithm of fixed parameters for which takeLog10 = TRUE
    fixed <- optimObject$fixed
    intersectFixedAndTakeLog10 <- intersect(
        names(fixed[!is.na(fixed)]),
        names(which(takeLog10))
    )
    fixedAndTakeLog10 <- rep(NA, length(takeLog10))
    names(fixedAndTakeLog10) <- names(takeLog10)
    fixedAndTakeLog10[intersectFixedAndTakeLog10] <- TRUE
    optimObject$fixed <- applyLog10ForTakeLog10(fixed, fixedAndTakeLog10)


    if (optimObject$modus == "doseDependent") {
        ds <- sort(unique(optimObject$data$d))
        if (length(ds) == 1) {
            warning("Please provide at least two different doses.")
        } else {
            secondHighestD <- ds[2]
            optimObject$data$d[optimObject$data$d == 0] <-
                secondHighestD / 1000
        }
    }

    for (vec in initialGuess.vec.lst) {
        print(vec)
        vec <- applyLog10ForTakeLog10(vec, takeLog10)

        ########################################################################
        # Replace 0 because for gradient calculation log(K) is calculated
        if (optimObject$modus == "doseDependent") {
            if (length(ds) > 1) {
                RTFparams <- c("alpha", "beta", "gamma", "A", "B", "tau")

                for (RTFparam in RTFparams) {
                    KRTFparam <- paste0("K_", RTFparam)
                    if (KRTFparam %in% names(vec)) {
                        if (vec[KRTFparam] == 0) {
                            vec[KRTFparam] <- secondHighestD / 100000
                        }
                    }

                    if (KRTFparam %in%
                        names(optimObject$fixed[
                            !is.na(optimObject$fixed)
                        ])) {
                        if (optimObject$fixed[
                            !is.na(optimObject$fixed)
                        ][KRTFparam] == 0) {
                            optimObject$fixed[
                                !is.na(optimObject$fixed)
                            ][KRTFparam] <-
                                secondHighestD / 100000
                        }
                    }
                }
            }
        }
        ########################################################################

        if (optimObject$sameSign) {
            signCombs <- list(
                c(signumTF_sus = -1, signumTF_trans = -1),
                c(signumTF_sus = 1, signumTF_trans = 1)
            )
        } else {
            signCombs <- list(
                c(signumTF_sus = -1, signumTF_trans = -1),
                c(signumTF_sus = -1, signumTF_trans = 1),
                c(signumTF_sus = 1, signumTF_trans = -1),
                c(signumTF_sus = 1, signumTF_trans = 1)
            )
        }

        # for (signumTF in c(-1, 1)) {
        for (signComb in signCombs) {
            # for (signumTF_sus in c(-1, 1)) {
            # for (signumTF_trans in c(-1, 1)) {
            signumTF_sus <- signComb[["signumTF_sus"]]
            signumTF_trans <- signComb[["signumTF_trans"]]
            optimObject$fixed[["signumTF_sus"]] <- signumTF_sus
            optimObject$fixed[["signumTF_trans"]] <- signumTF_trans
            optimResTmp <- stats::optim(
                par = vec,
                fn = objFunct,
                gr = objFunctGradient,
                method = "L-BFGS-B",
                lower = lower,
                upper = upper,
                data = optimObject$data,
                optimObject = optimObject,
                calcGradient = FALSE,
                control = optimObject$control
            )

            optimResTmp$par <- applyLog10ForTakeLog10(
                c(optimResTmp$par), takeLog10,
                reverse = TRUE
            )

            # vecOrder <- names(optimObject$fixed)
            parsFinal <- c(
                fixed[!is.na(fixed) &
                    !(names(fixed) %in% c(
                        "signumTF_sus",
                        "signumTF_trans"
                    ))],
                signumTF_sus = signumTF_sus,
                signumTF_trans = signumTF_trans,
                optimResTmp$par
            )[vecOrder]
            parsFinal[names(parsFinal) %in% yDependentPars] <-
                parsFinal[names(parsFinal) %in% yDependentPars] /
                    scaleFactor
            optimResTmp$par <- parsFinal

            # Punish if two different signs are used
            # 3.8 as result from stats::qchisq(0.1, df = 1, lower.tail = FALSE)
            if (signumTF_sus != signumTF_trans) {
                optimResTmp$value <- optimResTmp$value + 2.7
            }

            value <- c(optimResTmp$value)

            RTFmodelLst <- append(RTFmodelLst, list(optimResTmp))

            if (is.null(currentBestResValue)) {
                currentBestResValue <- value
                optimRes <- optimResTmp
            }

            if (value < currentBestResValue) {
                currentBestResValue <- value
                optimRes <- optimResTmp
            }
            # }
        }
    }

    RTFmodelLst <- sortListByValue(RTFmodelLst)

    list(optimResults = RTFmodelLst, bestOptimResult = optimRes)
}
