#' @description Sorts a list of sublists in ascending order according to
#' parameter 'value' in the first element of each sublist.
#' @return List sorted according to the parameter 'value' in the first element
#' of each sublist in ascending order.
#' @param lst List with sublists, of which the first element has to contain
#' parameter 'value', according to which the sublists will be sorted
#' @export sortListByValue
#' @examples
#' res.lst <- list(list(optimRes = list(str = "res1", value = -20),
#'                     gg = ggplot2::ggplot() + ggplot2::ggtitle("res1")),
#'                list(optimRes = list(str = "res2", value = -45),
#'                    gg = ggplot2::ggplot() + ggplot2::ggtitle("res2")),
#'                list(optimRes = list(str = "res3", value = 8),
#'                     gg = ggplot2::ggplot() + ggplot2::ggtitle("res3"))
#'            )
#' res.lst.sorted <- sortListByValue(res.lst)

sortListByValue <- function(lst) {
  lst2 <- unlist(lapply(lst,'[[',1), recursive = FALSE)
  lst2 <- unlist(lst2[names(lst2) == "value"])
  sortIdx <- sort(lst2, index.return = TRUE)$ix
  lst <- lst[sortIdx]
  lst
}
