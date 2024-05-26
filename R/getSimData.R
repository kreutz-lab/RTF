#' Simulate example data frame for defined RTF parameters
#'
#' @description Simulate an example data frame with data following an RTF with
#' predefined parameters plus some added noise.
#' @return Data frame with data following an RTF with predefined parameters plus
#' some added noise.
#' @param modus String indicating modus. Either "singleDose" or
#' "doseDependent" (Default: "singleDose").
#' @param noise Noise to be added. Default for modus = "singleDose" is 0.4,
#'  for modus = "doseDependent" 0.02.
#' @param numDoses (Only relevant if modus = "doseDependent") Number of distinct
#' doses to be simulated.
#' @export getSimData
#' @examples
#' getSimData()
getSimData <- function(modus = "singleDose", noise = NULL, numDoses = 7) {
    if (is.null(noise)) {
        if (modus == "singleDose") {
            noise <- 0.4
        } else if (modus == "doseDependent") {
            noise <- 0.02
        }
    }

    if (modus == "singleDose") {
        t <- seq(0, 17, 0.7)

        y <- getTransientFunctionResult(
            t = t,
            rtfPar = c(
                alpha = 0.4, beta = 0.4, gamma = 0.1, A = 5, B = 20,
                b = 2,
                tau = 3,
                signumTF_sus = 1,
                signumTF_trans = 1
            )
        )

        data.frame(t = t, y = y + stats::rnorm(length(t), 0, noise))
    } else if (modus == "doseDependent") {
        t <- seq(0, 17, 0.7)
        doses <- seq(numDoses)
        times <- length(doses)

        vec <- c()
        for (dose in doses) {
            # If B is dose-dependent, then M_B = B in RTF
            # (corresponds to K_A = (0 or lb) and h = 1)
            # In this example, A is dose-dependent
            par <- c(
                M_alpha = 1,
                h_alpha = 0.1,
                K_alpha = 0.03,
                M_beta = 1,
                h_beta = 0.1,
                K_beta = 0.03,
                M_gamma = 0.5,
                h_gamma = 0.2,
                K_gamma = 0.01,
                M_A = 1,
                h_A = 1,
                K_A = 0, # # 0 oder lb
                M_B = 6,
                h_B = 1.5,
                K_B = 8,
                M_tau = 4,
                h_tau = 6,
                K_tau = 20,
                b = 0.3
            )
            rtfPar <- getHillResults(d = dose, params = par)

            y <- NULL
            y <- getTransientFunctionResult(
                t = t,
                rtfPar = rtfPar,
                scale = FALSE,
                signumTF_sus = 1,
                signumTF_trans = 1
            )
            vec <- c(vec, y)
        }

        d <- rep(doses, each = length(t))
        t <- rep(t, times)
        df <- data.frame(
            t = t, y = vec +
                stats::rnorm(length(t), 0, noise),
            d = d
        )
        df <- rbind(c(t = 2, y = NA, d = 2), df)
        df
    }
}
