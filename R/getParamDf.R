#' Generate data frame containing the RTF parameters for multiple time courses.
#'
#' @description Generate data frame containing the RTF parameters for multiple
#' time courses from list of RTF results.
#' @return Data frame containing the RTF parameters for each time course,
#' where the rows correspond to the different time courses and
#' the columns to the RTF parameters.
#' @param RTFmodelLst List with the RTF result for each time course.
#' @export getParamDf
#' @examples
#' data(strasen)
#' df.multipleTimeCourses <- strasen[, 1:3]
#' RTFmodelLst <- RTFOnMultipleTimeCourses(df.multipleTimeCourses)
#' param.df <- getParamDf(RTFmodelLst)
getParamDf <- function(RTFmodelLst) {
    param.lst <- list()
    for (el in RTFmodelLst) {
        param.lst <- append(param.lst, list(el[["finalParams"]]))
    }
    names(param.lst) <- names(RTFmodelLst)
    param.df <- as.data.frame(do.call(cbind, param.lst))
    param.df <- param.df[row.names(param.df) != "sigma", ]

    param.df <- data.frame(t(param.df))
    param.df <- param.df[, apply(param.df, 2, stats::var, na.rm = TRUE) != 0]
    param.df
}
