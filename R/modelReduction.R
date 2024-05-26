#' Run model reduction on full RTF model
#'
#' @description Run model reduction on full RTF model
#' @return List of RTF model after model reduction ('finalModel') and
#' corresponding model parameters ('finalParams').
#' @param res Full RTF model, i.e., generated without fixed parameters
#' @param nInitialGuesses Integer indicating number of initial guesses
#' (in addition to the default initial guess) used for all four combinations
#' of signumTF_sus and signumTF_trans, which both can be -1 or 1 (Default: 50).
#' @param plotAllPointsWaterfall Boolean indicating if all points should be
#' plotted in waterfall plot (Default: FALSE).
#' If FALSE, all values up to the median of those values are plotted.
#' @export modelReduction
#' @examples
#' # Single-dose RTF
#' modus <- "singleDose"
#' data <- getSimData(modus = modus)
#' plotData(data)
#' res <- RTF(data, modus = modus)
#' res.reduced <- modelReduction(res$finalModel)
#'
#' \dontrun{
#' # Dose-dependent RTF
#' modus <- "doseDependent"
#' data.dose <- getSimData(modus = modus)
#' plotData(data.dose)
#' res.dose <- RTF(data.dose, modus = modus)
#' res.reduced.dose <- modelReduction(res.dose$finalModel)
#' }
modelReduction <- function(res,
                           nInitialGuesses = 50,
                           plotAllPointsWaterfall = FALSE) {
    modus <- res[["modus"]]
    res.orig <- res

    optimParamsFullModel <- res[["bestOptimResult"]][["par"]]

    optimParamsFullModel <- optimParamsFullModel[
        !(names(optimParamsFullModel) %in% c(
            "signumTF_sus",
            "signumTF_trans"
        ))
    ]

    res.orig$initialGuess.vec <- optimParamsFullModel

    if (modus != "doseDependent") {
        #  MODEL REDUCTION
        # 1. Testing whether there is time retardation,
        # i.e., if tau parameter is significantly different from the lower
        # bound.
        # If not significant, tau is set to the lower bound which is
        # tau = −2 by default.
        optimObjectTmp <- res.orig
        optimObjectTmp[["takeLog10"]][
            names(optimObjectTmp[["takeLog10"]]) == "tau"
        ] <- FALSE
        optimObjectTmp$fixed[["tau"]] <- res.orig$lb.vec[["tau"]]
        res.tauLB <- getInitialGuessResults(
            optimObjectTmp,
            nInitialGuesses = nInitialGuesses
        )
        res <- selectSmallerModelIfDiffIsSmall(res, res.tauLB)

        # 2. Testing whether the model is in agreement with a constant.
        # If not significant, we set A = B = 0.
        optimObjectTmp2 <- res
        optimObjectTmp2[["takeLog10"]][names(optimObjectTmp2[["takeLog10"]])
        %in% c("A", "B")] <- FALSE
        optimObjectTmp2$fixed[["A"]] <- optimObjectTmp2$fixed[["B"]] <- 0
        res.constant <- getInitialGuessResults(
            optimObjectTmp2,
            nInitialGuesses = nInitialGuesses
        )
        res <- selectSmallerModelIfDiffIsSmall(res, res.constant)

        # 3. Testing whether the offset b is significantly different from zero.
        # If not significant, we set b = 0.
        optimObjectTmp3 <- res
        optimObjectTmp3[["takeLog10"]][
            names(optimObjectTmp3[["takeLog10"]]) == "b"
        ] <- FALSE
        optimObjectTmp3$fixed[["b"]] <- 0
        res.bZero <- getInitialGuessResults(
            optimObjectTmp3,
            nInitialGuesses = nInitialGuesses
        )
        res <- selectSmallerModelIfDiffIsSmall(res, res.bZero)
    } else {
        # This order is most plausible based on our experience
        RTFparams <- c("gamma", "alpha", "beta", "tau", "A", "B")
        # statLst <- list()
        # statObjLst <- list()

        res <- res.orig

        for (RTFparam in RTFparams) {
            print(RTFparam)
            optimObjectTmp <- res

            optimObjectTmp$fixed[[paste0("h_", RTFparam)]] <- 1

            optimObjectTmp[["takeLog10"]][
                names(optimObjectTmp[["takeLog10"]]) ==
                    paste0("K_", RTFparam)
            ] <- FALSE
            optimObjectTmp$fixed[[paste0("K_", RTFparam)]] <- 0

            res.fixed <- getInitialGuessResults(
                optimObjectTmp,
                nInitialGuesses = nInitialGuesses
            )

            res <- selectSmallerModelIfDiffIsSmall(res, res.fixed)
        }
    }

    finalParams <- res$fitted

    print("The parameters of the best fit after model reduction are:")
    print(paste(
        names(finalParams),
        signif(finalParams, 4),
        sep = ": ",
        collapse = ", "
    ))
    print("Log-likelihood:")
    print(res[["bestOptimResult"]][["value"]])

    reductionResults <- list(finalModel = res, finalParams = finalParams)

    if (all(res.orig$fitted == reductionResults$finalParams)) {
        warning("No model reduction indicated. Full model is retained.")
    }
    reductionResults
}
