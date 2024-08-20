#' Saves the function of the fitted RTF as well as a table with the fitted 
#' parameters and the respective bounds to files
#'
#' @description Saves the function of the fitted RTF (to .R file) as well as a 
#' table with the fitted parameters and the respective bounds (to .tsv file). 
#' After the function is generated it becomes sourced.
#' @return Saves the function of the fitted RTF as well as the fitted parameters
#' to files.
#' @param optimObject optimObject, containing the fitted parameters and the 
#' modus ('singleDose' or 'doseDependent').
#' @param functionName (Optional) 
#' String, which specifies how the fitted RTF function in "[functionName].R"  
#' and the table with the fitted parameters and respective bounds 
#' ("[functionName]_fittedParams.tsv") should be called.
#' If not specified, it will be set to "fittedRTF".
#' @export saveFittedRTF
#' @examples
#' \dontrun{
#' data(resSingleDose)
#' saveFittedRTF(optimObject = resSingleDose$finalModel)
#' }

saveFittedRTF <- function(optimObject,
                          functionName = "fittedRTF") {
    
    finalParams <- optimObject$fitted
    functionName <- make.names(functionName)
    
    # Save fitted parameters to .tsv file, together with the lower and upper 
    # bounds.
    sort_and_match <- function(named_vector, order) {
        sorted_vector <- stats::setNames(
            named_vector[match(order, names(named_vector))],
            order
        )
        sorted_vector[is.na(sorted_vector)] <- NA
        return(sorted_vector)
    }
    
    sorted_lb <- sort_and_match(optimObject$lb.vec, names(finalParams))
    sorted_ub <- sort_and_match(optimObject$ub.vec, names(finalParams))
    
    paramDf <- data.frame(fittedParameter = finalParams, 
                          lowerBound = sorted_lb, 
                          upperBound = sorted_ub)
    
    tsvFileName <- paste0(functionName, "_fittedParams.tsv")
    utils::write.table(x = paramDf, file = tsvFileName)
    
    
    # Save fitted RTF to R file, which is sourced subsequently
    rFileName <- paste0(functionName, ".R")
    
    modus <- optimObject$modus
    
    
    if (modus == 'doseDependent') {
        cat(functionName, "<- function(times, doses) {", "\n", 
            file = rFileName)
    } else {
        cat(functionName, "<- function(times) {", "\n", 
            file = rFileName)
    }
    
    cat(paste0("    modus <- \"", modus, "\"\n"), 
        "   finalParams <- ", file = rFileName, append = TRUE)
    utils::capture.output(dput(finalParams), file = rFileName, append = TRUE)
    
    if (modus == 'doseDependent') {
        cat("    df <- getRTFResDf(par = finalParams, modus = modus,", "\n",
            "                      t = times, doses = doses)", "\n",
            file = rFileName, append = TRUE)
    } else {
        cat("    df <- getRTFResDf(par = finalParams, modus = modus, t = times)", 
            "\n",
            file = rFileName, append = TRUE)
    }
    cat("    df", "\n",
        "}", "\n",
        file = rFileName, append = TRUE
        )
    source(rFileName)
}