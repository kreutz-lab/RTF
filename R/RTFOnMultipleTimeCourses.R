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
#' (in addition to the default initial guess) used for all four combinations
#' of signumTF_sus and signumTF_trans, which both can be -1 or 1 (Default: 50).
#' @param sameSign Boolean indicating if sign of sustained RTF part
#' (signumTF_sus) and transient RTF part (signumTF_trans) should be equal
#' (Default: TRUE).
#' @export RTFOnMultipleTimeCourses
#' @examples
#' data(strasen)
#' df <- strasen[, 1:3]
#' RTFmodelLst <- RTFOnMultipleTimeCourses(df)
RTFOnMultipleTimeCourses <- function(df,
                                     modelReduction = FALSE,
                                     nInitialGuesses = 50,
                                     sameSign = TRUE) {
    colNames <- colnames(df[2:ncol(df)])
    RTFmodelLst <- list()
    for (colIdx in 2:ncol(df)) {
        df.tmp <- data.frame(t = df[, 1], y = df[, colIdx])
        res <- RTF(
            df = df.tmp, nInitialGuesses = nInitialGuesses,
            sameSign = sameSign
        )
        if (modelReduction) res <- modelReduction(res)
        RTFmodelLst <- append(RTFmodelLst, list(res))
    }
    names(RTFmodelLst) <- colNames
    RTFmodelLst
}
