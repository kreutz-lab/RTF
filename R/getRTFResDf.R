#' Generate a data frame with RTF results for defined time points (and doses)
#'
#' @description Generate a data frame with RTF results for defined time 
#' points (and doses).
#' @return Data frame with column for defined time points ('t') and the 
#' predicted quantitative values resulting from calculating RTF with the 
#' specified parameters fir these time points ('y'). If modus = 'doseDependent' 
#' there is an additional column for the dose ('d').
#' @param par Named vector of the parameter values used for RTF (and, in the 
#' case of the dose-dependent RTF, for the Hill equations).
#' @param modus Modus ('singleDose' or 'doseDependent')
#' @param t Vector of time points
#' @param doses Vector of doses. Only relevant if modus = 'doseDependent'.
#' @param scale Boolean, indicates if time-dependent parameters t, tau,
#' alpha, beta, and gamma should be scaled (Default: TRUE).
#' @export getRTFResDf
#' @examples
#' rtfPar <- c(
#'     alpha = 1.00, beta = 1.00, gamma = 1.00, A = 1.05,
#'     B = 3.05, b = -0.28, tau = -1, signSus = 1, signTrans = 1
#' )
#' t <- c(
#'     0, 0.71, 1.42, 2.14, 2.85, 3.57, 4.28, 5, 5.71, 6.42,
#'     7.14, 7.85, 8.57, 9.28, 10
#' )
#' df <- getRTFResDf(par = rtfPar, t = t)
getRTFResDf <- function(par, 
                        modus = 'singleDose', 
                        t, 
                        doses = c(), 
                        scale = TRUE) {
    if (modus == "singleDose") {
        RTFResVec <- getTransientFunctionResult(
            t = t,
            rtfPar = par,
            signSus = par[["signSus"]],
            signTrans = par[["signTrans"]],
            scale = scale, 
            calcGradient = FALSE
        )
        
        RTFResDf <- data.frame(
            t = t,
            y = RTFResVec
        )
    } else if (modus == "doseDependent") {
        geom_line.lst <- list()
        for (i in seq(length(doses))) {
            RTFResVec <- NULL
            dose <- doses[i]
            
            rtfPar <- getHillResults(d = dose, params = par)
            
            RTFResVec <- getTransientFunctionResult(
                rtfPar = rtfPar,
                t = t,
                signSus = par[["signSus"]],
                signTrans = par[["signTrans"]],
                scale = scale, 
                calcGradient = FALSE
            )
            
            geom_line.lst <- append(
                geom_line.lst,
                list(data.frame(
                    t = t,
                    y = RTFResVec,
                    d = dose
                ))
            )
        }
        RTFResDf <- dplyr::bind_rows(geom_line.lst)
    }
    RTFResDf
}
