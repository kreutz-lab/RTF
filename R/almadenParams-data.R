#' Example RTF parameter data frame to simulated time courses of NFkB pathway
#' components
#'
#' This example data frame containing the estimated RTF parameters for the
#' 'almaden' dataset, which is modelled using Data2Dynamics and is
#' based on the NFkB model of Almaden et al.
#' (\href{https://doi.org/10.1016/j.celrep.2014.11.024}{https://doi.org/10.1016/j.celrep.2014.11.024}).
#' It has been generated from a rds file returned from:
#' \preformatted{
#' data(almaden)
#' params.lst <- getParamsFromMultipleTimeCourses(almaden,
#'                                                nInitialGuesses = 200,
#'                                                sameSign = FALSE)
#' almadenParams <- params.lst[["param.df"]]
#' }
#'
#' @docType data
#'
#' @usage data(almadenParams)
#'
#' @keywords datasets
#'
#' @references Almaden, Jonathan V., et al. "A pathway switch directs BAFF
#' signaling to distinct NFκB transcription factors in maturing and
#' proliferating B cells." Cell reports 9.6 (2014): 2098-2111.
#' (\href{https://doi.org/10.1016/j.celrep.2014.11.024}{https://doi.org/10.1016/j.celrep.2014.11.024})
#'
#' Raue, Andreas, et al. "Data2Dynamics: a modeling environment
#' tailored to parameter estimation in dynamical systems." Bioinformatics
#' 31.21 (2015): 3558-3560.
#' (\href{https://doi.org/10.1093/bioinformatics/btv405}{https://doi.org/10.1093/bioinformatics/btv405})
#'
#' @examples
#' data(almadenParams)
"almadenParams"
