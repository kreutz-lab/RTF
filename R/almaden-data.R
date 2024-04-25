#' Simulated time course data of NFkB pathway components
#'
#' Time course data is modelled using Data2Dynamics and is based on the 
#' NFkB model of Almaden et al. (https://doi.org/10.1016/j.celrep.2014.11.024).
#' 
#' The two conditions in the form of a genetic perturbation that prevents 
#' processing of p100 to p52 (condition 2) and wild-type controls (condition 1),
#' are indicated by the last digit of the column names.
#'
#' @docType data
#'
#' @usage data(almaden)
#'
#' @keywords datasets
#'
#' @references Almaden, Jonathan V., et al. "A pathway switch directs BAFF 
#' signaling to distinct NFκB transcription factors in maturing and 
#' proliferating B cells." Cell reports 9.6 (2014): 2098-2111.
#' (https://doi.org/10.1016/j.celrep.2014.11.024)
#' 
#' Raue, Andreas, et al. "Data2Dynamics: a modeling environment 
#' tailored to parameter estimation in dynamical systems." Bioinformatics 
#' 31.21 (2015): 3558-3560. (https://doi.org/10.1093/bioinformatics/btv405)
#'
#' @examples
#' data(almaden)
"almaden"