#' Example RTF parameter data frame to simulated time series data of cell 
#' classes
#'
#' This example data frame containing the estimated RTF parameters for the 
#' 'strasenTimeSeries' dataset, which is based on
#' Strasen et al. (\href{https://doi.org/10.15252/msb.20177733}{EMBOpress}).
#' It has been generated as follows:
#' data(strasenTimeSeries)
#' res.lst <- RTFOnMultipleTimeSeries(strasenTimeSeries)
#' strasenParams <- getParamDf(res.lst)
#'
#' @docType data
#'
#' @usage data(strasenParams)
#'
#' @keywords datasets
#'
#' @references Strasen, Jette, et al. "Cell‐specific responses to the cytokine 
#' TGF β are determined by variability in protein levels." 
#' Molecular systems biology 14.1 (2018): e7733.
#' (\href{https://doi.org/10.15252/msb.20177733}{EMBOpress})
#'
#' @examples
#' data(strasenParams)
"strasenParams"