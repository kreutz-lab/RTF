#' Run RTF on multiple time courses
#'
#' @description Runs RTF on multiple columns corresponding to different time 
#' courses.
#' @return List with the RTF result for each time course.
#' @param df Data frame with the first column corresponding to the time points 
#' and all the following columns corresponding to the different time courses.
#' @param modelReduction Boolean indicating if model reduction should be 
#' performed for RTF
#' @param nInitialGuesses Integer indicating number of initial guesses 
#' (in addition to the default initial guess) used both for a signum_TF of -1 
#' and 1 (Default: 50).
#' @export RTFOnMultipleTimeCourses
#' @examples
#' data(strasen)
#' df <- strasen[, 1:3]
#' RTFmodelLst <- RTFOnMultipleTimeCourses(df)

RTFOnMultipleTimeCourses <- function(df,
                                    modelReduction = FALSE,
                                    nInitialGuesses = 50) {
    colNames <- colnames(df[2:ncol(df)])
    RTFmodelLst <- list()
    for (colIdx in 2:ncol(df)) {
        df.tmp <- data.frame(t = df[, 1], y = df[, colIdx])
        res <- RTF(df = df.tmp, nInitialGuesses = nInitialGuesses)
        if (modelReduction) res <- modelReduction(res)
        RTFmodelLst <- append(RTFmodelLst, list(res))
    }
    names(RTFmodelLst) <- colNames
    RTFmodelLst
}