#' Example RTF parameter data frame to simulated time course data of cell 
#' classes
#'
#' This example data frame containing the estimated RTF parameters for the 
#' 'strasen' dataset, which is based on Strasen et al. 
#' (https://doi.org/10.15252/msb.20177733).
#' It has been generated as follows:
#' data(strasen)
#' params.lst <- getParamsFromMultipleTimeCourses(strasen, 
#'                                                nInitialGuesses = 200)
#' strasenParams <- params.lst[["param.df"]]    
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
#' (https://doi.org/10.15252/msb.20177733)
#'
#' @examples
#' data(strasenParams)
"strasenParams"