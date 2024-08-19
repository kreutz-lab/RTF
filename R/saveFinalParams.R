#' Saves fitted parameters to files
#'
#' @description Saves the fitted parameters to an R file which becomes sourced 
#' to make the fitted parameters readily available. Furthermore, the fitted 
#' paratmeters together with the respective parameter bounds become saved to a 
#' .tsv file.
#' @return Saves fitted parameters to files.
#' @param optimObject optimObject, containing the fitted parameters
#' @param saveParamsFileString (Optional, only relevant if saveParams = TRUE) 
#' String, which specifies how the output files with the fitted parameters 
#' should be called ("[saveParamsFileString].tsv" and 
#' "[saveParamsFileString].R"). If not specified it will be set to 
#' "fittedParams".
#' @export saveFinalParams
#' @examples
#' \dontrun{
#' data(resSingleDose)
#' saveFinalParams(optimObject = resSingleDose$finalModel)
#' }

saveFinalParams <- function(optimObject,
                            fileString = "fittedParams") {
    
    finalParams <- optimObject$fitted
    
    rFileName <- paste0(fileString, ".R")
    # Save final parameters to R file, which is sourced subsequently
    cat("finalParams <- ", file = rFileName) 
    utils::capture.output(dput(finalParams), file = rFileName, append = TRUE)
    source(rFileName)
    
    # Save final parameters to tab separated table, together with the lower and 
    # upper bounds.
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
    
    tsvFileName <- paste0(fileString, ".tsv")
    utils::write.table(x = paramDf, file = tsvFileName)
}

