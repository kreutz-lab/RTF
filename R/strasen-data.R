#' Simulated time course data of cell classes
#'
#' Simulated time course data based on cell class model introduced in
#' Strasen et al. (\href{https://doi.org/10.15252/msb.20177733}{EMBOpress}).
#' Data frame containing the time course data simulated by JWS Online 
#' (https://jjj.bio.vu.nl/models/experiments/strasen2018_fig5a/) for cell lines 
#' 1-6.
#' 
#' The function used to generate the data frame from the .csv files generated 
#' by JWS Online:
#' getMultipleTimeCoursesExampleDf <- function() {
#'   modelName <- "strasen"
#'   df.lst <- list()
#'   for (i in 6:11) {
#'     sub.df <- read.csv(paste0("data_", modelName, i, ".csv"), 
#'                        check.names = FALSE)
#'     colnames(sub.df) <- paste0(colnames(sub.df), "_", i)
#'     df.lst <- append(df.lst, sub.df)
#'   }
#'   df.multipleTimeCourses <- as.data.frame(df.lst)
#'   time <- df.multipleTimeCourses$time_11
#'   df.multipleTimeCourses <- df.multipleTimeCourses[, !grepl("time",
#'                                            colnames(df.multipleTimeCourses))]
#'   df.multipleTimeCourses <- cbind(time, df.multipleTimeCourses)
#' }
#'
#' @docType data
#'
#' @usage data(strasen)
#'
#' @keywords datasets
#'
#' @references Strasen, Jette, et al. "Cell‐specific responses to the cytokine 
#' TGF β are determined by variability in protein levels." 
#' Molecular systems biology 14.1 (2018): e7733.
#' (https://doi.org/10.15252/msb.20177733)
#'
#' @source Simulations to cell classes 1-6 corresponding to Models 6-11 at
#' JWS Online (https://jjj.bio.vu.nl/models/experiments/strasen2018_fig5a/).
#' @examples
#' data(strasen)
"strasen"