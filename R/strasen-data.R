#' Simulated time series data of cell classes
#'
#' Simulated time series data based on cell class model introduced in
#' Strasen et al. (\href{https://doi.org/10.15252/msb.20177733}{EMBOpress}).
#' Data frame containing the time series data simulated by 
#' \href{https://jjj.bio.vu.nl/models/experiments/strasen2018_fig5a/}
#' {JWS Online} for cell lines 1-6.
#' 
#' The function used to generate the data frame from the .csv files generated 
#' by JWS Online:
#' getMultipleTimeSeriesExampleDf <- function() {
#'   modelName <- "strasen"
#'   df.lst <- list()
#'   for (i in 6:11) {
#'     sub.df <- read.csv(paste0("data_", modelName, i, ".csv"), 
#'                        check.names = FALSE)
#'     colnames(sub.df) <- paste0(colnames(sub.df), "_", i)
#'     df.lst <- append(df.lst, sub.df)
#'   }
#'   df.multipleTimeSeries <- as.data.frame(df.lst)
#'   time <- df.multipleTimeSeries$time_11
#'   df.multipleTimeSeries <- df.multipleTimeSeries[, !grepl("time",
#'                                            colnames(df.multipleTimeSeries))]
#'   df.multipleTimeSeries <- cbind(time, df.multipleTimeSeries)
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
#' (\href{https://doi.org/10.15252/msb.20177733}{EMBOpress})
#'
#' @source Simulations to cell classes 1-6 corresponding to Models 6-11 at
#' \href{https://jjj.bio.vu.nl/models/experiments/strasen2018_fig5a/}
#' {JWS Online}
#' @examples
#' data(strasen)
"strasen"