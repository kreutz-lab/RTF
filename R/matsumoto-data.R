#' Experimental time course data with dose dependencies
#'
#' Time course data of amino acids showing dose dependencies for the 
#' ingestion of branched-chain amino acids, taken from the publication of 
#' Matsumoto et al. 2014. (https://doi.org/10.1186/2193-1801-3-35).
#' 
#' The data frame contains columns corresponding to the measured amino acid 
#' ("measuredAA"), the applied dose ("dose"),  the ingested branch-chain amino 
#' acid ("ingestedBCAA"), the measurement time point ("time"), the measurement 
#' value ("value"), and the standard error of the measurement value ("SE").
#' 
#' @docType data
#'
#' @usage data(matsumoto)
#'
#' @keywords datasets
#'
#' @references Matsumoto, T., Nakamura, K., Matsumoto, H. et al. Bolus 
#' ingestion of individual branched-chain amino acids alters plasma amino acid 
#' profiles in young healthy men. SpringerPlus 3, 35 (2014).
#' (https://doi.org/10.1186/2193-1801-3-35)
#'
#' @examples
#' data(matsumoto)
"matsumoto"