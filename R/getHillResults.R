#' Get RTF parameters (A, B, alpha, gamma, and tau) for a defined dose by
#' applying Hill equations
#'
#' @description Get RTF parameters (A, B, alpha, beta, gamma, and tau) for a
#' defined dose by applying Hill equations
#' @return Named vector of the calculated RTF parameters for the provided dose.
#' @param d Numeric value corresponding to a dose
#' @param params Named vector containing the parameters
#' 'M_alpha', 'h_alpha', 'K_alpha', 'M_beta', 'h_beta', 'K_beta',
#' 'M_gamma', 'h_gamma', 'K_gamma', 'M_A', 'h_A', 'K_A',
#' 'M_B', 'h_B', 'K_B', 'M_tau', 'h_tau', 'K_tau', 'b'
#' (but can also contain additional ones)
#' @param calcGradient Boolean indicating if gradient should be calculated
#' (Default: FALSE)
#' @export getHillResults
#' @examples
#' df <- getHillResults(
#'     d = 2,
#'     params = c(
#'         M_alpha = 1, h_alpha = 2, K_alpha = 2,
#'         M_beta = 1, h_beta = 2, K_beta = 2,
#'         M_gamma = 1, h_gamma = 3, K_gamma = 2,
#'         M_A = 4, h_A = 2, K_A = 1,
#'         M_B = 2, h_B = 3, K_B = 1,
#'         M_tau = 2, h_tau = 3, K_tau = 2, b = 1
#'     )
#' )
getHillResults <- function(d = NULL,
                           params = c(),
                           calcGradient = FALSE) {
    for (v in 1:length(params)) assign(names(params)[v], params[[v]])

    rtfParams <- c("A", "B", "alpha", "beta", "gamma", "tau")

    for (el in c(
        "d",
        "M_alpha", "h_alpha", "K_alpha",
        "M_beta", "h_beta", "K_beta",
        "M_gamma", "h_gamma", "K_gamma",
        "M_A", "h_A", "K_A",
        "M_B", "h_B", "K_B",
        "M_tau", "h_tau", "K_tau", "b"
    )) {
        if (!exists(el)) stop(paste0(el, " missing in function getHillResults"))
    }

    A <- hillEquation(d = d, M = M_A, h = h_A, K = K_A, reciprocal = FALSE)
    B <- hillEquation(d = d, M = M_B, h = h_B, K = K_B, reciprocal = FALSE)
    alpha <- hillEquation(
        d = d, M = M_alpha, h = h_alpha, K = K_alpha,
        reciprocal = FALSE
    )
    beta <- hillEquation(
        d = d, M = M_beta, h = h_beta, K = K_beta,
        reciprocal = FALSE
    )
    gamma <- hillEquation(
        d = d, M = M_gamma, h = h_gamma, K = K_gamma,
        reciprocal = FALSE
    )
    tau <- hillEquation(
        d = d, M = M_tau, h = h_tau, K = K_tau,
        reciprocal = TRUE
    )

    if (calcGradient) {
        drtfParams_dparams <- matrix(0,
            nrow = length(rtfParams) + 1,
            # +1 because of b, ugly
            ncol = length(params)
        )
        rownames(drtfParams_dparams) <- c(rtfParams, "b") # ugly
        colnames(drtfParams_dparams) <- names(params)

        for (el in rtfParams) {
            if (el == "tau") {
                reciprocal <- TRUE
            } else {
                reciprocal <- FALSE
            }
            del_dmhk <- hillEquation(
                d = d, M = get(paste0("M_", el)),
                h = get(paste0("h_", el)),
                K = get(paste0("K_", el)),
                reciprocal = reciprocal,
                gradientNames = c("M", "h", "K")
            )

            drtfParams_dparams[el, paste0("M_", el)] <- del_dmhk[, "M"]
            drtfParams_dparams[el, paste0("h_", el)] <- del_dmhk[, "h"]
            drtfParams_dparams[el, paste0("K_", el)] <- del_dmhk[, "K"]
        }
        drtfParams_dparams["b", "b"] <- 1

        return(drtfParams_dparams)
    } else {
        return(c(
            A = A,
            B = B,
            alpha = alpha,
            beta = beta,
            gamma = gamma,
            tau = tau,
            b = b
        ))
    }
}
