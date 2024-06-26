#' Experimental time course data with dose dependencies
#'
#' Time course data of amino acids showing dose dependencies for the
#' ingestion of branched-chain amino acids, taken from the supplemental
#' file "40064_2013_1453_MOESM1_ESM.doc" of the publication
#' \href{https://doi.org/10.1186/2193-1801-3-35}{https://doi.org/10.1186/2193-1801-3-35}
#' by Matsumoto et al., which was
#' distributed under the terms of the Creative Commons Attribution 2.0
#' International License
#' (\href{https://creativecommons.org/licenses/by/2.0}{https://creativecommons.org/licenses/by/2.0}).
#'
#' The data table of the supplemental material was transformed to a data frame
#' containing columns corresponding to the measured amino acid
#' ("measuredAA"), the applied dose ("dose"), the ingested branch-chain amino
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
#' (\href{https://doi.org/10.1186/2193-1801-3-35}{https://doi.org/10.1186/2193-1801-3-35})
#'
#' @examples
#' data(matsumoto)
"matsumoto"
