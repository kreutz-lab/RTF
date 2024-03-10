#' Sort sublists by optimization measure value
#'
#' @description Sorts a list of sublists in ascending order according to
#' parameter 'value'.
#' @return List sorted according to the parameter 'value' in ascending order.
#' @param lst List with sublists, each of which needs to contain
#' parameter 'value', according to which the sublists will be sorted.
#' @export sortListByValue
#' @examples
#' lst <- list(optimRes = list(str = "res1", value = -20),
#'                 optimRes = list(str = "res2", value = -45),
#'                 optimRes = list(str = "res3", value = 8))
#' lst.sorted <- sortListByValue(lst)

sortListByValue <- function(lst) {
  lst2 <- c(unlist(lapply(lst, function(x) x[["value"]])))
  sortIdx <- sort(lst2, index.return = TRUE)$ix
  lst <- lst[sortIdx]
  lst
}
