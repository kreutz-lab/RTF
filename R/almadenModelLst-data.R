#' List of RTF results of each simulated time series data of NFkB 
#' pathway components
#'
#' List of the RTF result for each time series of the  NFkB model of 
#' Almaden et al.
#'  (\href{https://doi.org/10.1016/j.celrep.2014.11.024}).
#' It has been generated as follows:
#' data(almaden)
#' params.lst <- getParamsFromMultipleTimeSeries(almaden, 
#'                                               nInitialGuesses = 100)
#' almadenModelLst <- params.lst[["RTFmodelLst"]]
#'
#' @docType data
#'
#' @usage data(almadenModelLst)
#'
#' @keywords datasets
#'
#' @references Almaden, Jonathan V., et al. "A pathway switch directs BAFF 
#' signaling to distinct NFκB transcription factors in maturing and 
#' proliferating B cells." Cell reports 9.6 (2014): 2098-2111.
#' (\href{https://doi.org/10.1016/j.celrep.2014.11.024})
#' 
#' Raue, Andreas, et al. "Data2Dynamics: a modeling environment 
#' tailored to parameter estimation in dynamical systems." Bioinformatics 
#' 31.21 (2015): 3558-3560. 
#' (\href{https://doi.org/10.1093/bioinformatics/btv405})
#'
#' @examples
#' data(almadenModelLst)
"almadenModelLst"