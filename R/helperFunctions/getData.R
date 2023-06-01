#' Generate data frame for RTF
#'
#' @description Generate data frame containing all columns needed for RTF
#' @return Data frame containing columns named 't' (time) and
#' 'y' (quantitative value). If .csv, .xlsx, or .xls file name is provided for
#' parameter 'file' is provided, this will be used, if no file name is
#' provided data will be simulated.
#' @param file File name of experimental data. Allowed formats:
#' .csv, .xlsx, .xls. file should at least contain two columns, which if not
#' otherwise specified will be named 't' and 'y'. Alternatively, the column
#' corresponding to time and the columns containing quantitative information can
#' be specified using tCol and quantCols, respectively.
#' @param tCol If file is provided, tCol can be used to specify column
#' indicating time
#' @param quantCols If file is provided, quantCols can be used to specify
#' columns containing quantitative information. This can be used, e.g., where
#' the columns in quantCols correspond to replicates at the same time point.
#' @param sdExpCol If file is provided, sdExpCol can be used to explicitly
#' define the column with the standard deviation of the experimental data. If
#' not provided the standard deviation will be estimated during fitting.
#' @export getData
#' @examples
#' data <- getData()
#' # data <- getData(file="ExampleData.xls", tCol="time",
#' # quantCols=c("Replikat1", "Replikat2", "Replikat3"))

getData <- function(file="", tCol="", quantCols=c(), sdExpCol = ""){
  if (file == "") {
    # Simulate data
    nrows <- 20
    signal <- function(x) {x*(x-.9)^2}
    t <- runif(nrows, 0, 1)
    y <- signal(t) + rnorm(length(t), 0, 0.025)
    sdExp <- abs(runif(nrows, 0, 0.002)*y)
    data <- data.frame(t = t, y = y)
  } else {
    if (grepl("\\.csv$", file)){
      data <- read.csv(file)
    } else if (grepl("\\.xls$", file) | grepl("\\.xlsx$", file)){
      data <- readxl::read_excel(file)
    }

    if (nchar(tCol) > 0 & length(quantCols) > 0 & nchar(sdExpCol) > 0){
      data <- data[, c(tCol, quantCols, sdExpCol)]
    } else     if (nchar(tCol) > 0 & length(quantCols) > 0){
      data <- data[, c(tCol, quantCols)]
    }

    ncols <- ncol(data)

    if (nchar(sdExpCol) > 0) {
      # names(data)[names(data) == sdExpCol] <- "sdExp"
      colnames(data) <- c("t", paste0("rep_", 1:(ncols-2)), "sdExp")
    } else {
      colnames(data) <- c("t", paste0("rep_", 1:(ncols-1)))
    }

    repCols <- grepl('rep_', colnames(data))
    nReps <- sum(repCols)
    if (nReps == 1){
      names(data)[names(data) == "rep_1"] <- "y"
    } else {
      if (nchar(sdExpCol) > 0) {
        id.vars <- c("t", "sdExp")
      } else {
        id.vars <- c("t")
      }
      data <- reshape2::melt(data, id.vars = id.vars, value.name = "y")
      data$variable <- NULL
    }

    # else it is assumed that table has the following column order:
    # one time column, quantitative columns
  }
  data <- data[order(data$t, data$y),]
  data
}
