% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getParamDf.R
\name{getParamDf}
\alias{getParamDf}
\title{Generate data frame containing the RTF parameters for multiple time courses.}
\usage{
getParamDf(RTFmodelLst)
}
\arguments{
\item{RTFmodelLst}{List with the RTF result for each time course.}
}
\value{
Data frame containing the RTF parameters for each time course,
where the rows correspond to the different time courses and
the columns to the RTF parameters.
}
\description{
Generate data frame containing the RTF parameters for multiple
time courses from list of RTF results.
}
\examples{
data(strasen)
df.multipleTimeCourses <- strasen[, 1:3]
RTFmodelLst <- RTFOnMultipleTimeCourses(df.multipleTimeCourses)
param.df <- getParamDf(RTFmodelLst)
}
