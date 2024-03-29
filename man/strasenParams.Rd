% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strasenParams-data.R
\docType{data}
\name{strasenParams}
\alias{strasenParams}
\title{Example RTF parameter data frame to simulated time series data of cell 
classes}
\format{
An object of class \code{data.frame} with 138 rows and 7 columns.
}
\usage{
data(strasenParams)
}
\description{
This example data frame containing the estimated RTF parameters for the 
'strasenTimeSeries' dataset, which is based on
Strasen et al. (\href{https://doi.org/10.15252/msb.20177733}{EMBOpress}).
It has been generated as follows:
data(strasenTimeSeries)
res.lst <- RTFOnMultipleTimeSeries(strasenTimeSeries)
strasenParams <- getParamDf(res.lst)
}
\examples{
data(strasenParams)
}
\references{
Strasen, Jette, et al. "Cell‐specific responses to the cytokine 
TGF β are determined by variability in protein levels." 
Molecular systems biology 14.1 (2018): e7733.
(\href{https://doi.org/10.15252/msb.20177733}{EMBOpress})
}
\keyword{datasets}
