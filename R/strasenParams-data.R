#' Example RTF parameter data frame to simulated time course data of cell
#' classes
#'
#' This example data frame containing the estimated RTF parameters for the
#' 'strasen' dataset, which is based on the publication
#' \href{https://doi.org/10.15252/msb.20177733}{https://doi.org/10.15252/msb.20177733} 
#' by Strasen et al., which was distributed under the terms of the 
#' Creative Commons Attribution 4.0 License
#' (\href{https://creativecommons.org/licenses/by/4.0/}{https://creativecommons.org/licenses/by/4.0/}).
#' It has been generated as follows:
#' \preformatted{
#' data(strasen)
#' params.lst <- getParamsFromMultipleTimeCourses(strasen,
#'                                                nInitialGuesses = 200,
#'                                                sameSign = FALSE)
#' strasenParams <- params.lst[["param.df"]]
#' }
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
#' (\href{https://doi.org/10.15252/msb.20177733}{https://doi.org/10.15252/msb.20177733})
#'
#' @examples
#' data(strasenParams)
"strasenParams"
