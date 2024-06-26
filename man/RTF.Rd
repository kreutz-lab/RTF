% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RTF.R
\name{RTF}
\alias{RTF}
\title{Run RTF}
\usage{
RTF(
  df,
  modus = NULL,
  optimFunction = "logLikelihood",
  nInitialGuesses = 50,
  sameSign = TRUE,
  plotAllPointsWaterfall = FALSE,
  control = list(trace = 1, maxit = 1000, factr = 1e+07),
  resOld = NULL
)
}
\arguments{
\item{df}{Data frame containing columns named 't' (time) and
'y' (quantitative value) for modus = 'singleDose' ('SD'), and columns
't', 'y', and 'd' (dose) for modus = 'doseDependent' ('DD').
Optionally, a column 'sigmaExp' can be provided with the standard error of
the experimental data.}

\item{modus}{String indicating if modus 'singleDose' ('SD') or
'doseDependent' ('DD') should be used.
If no modus is provided default setting are 'doseDependent' if column with
name 'd' is present and else 'singleDose'.}

\item{optimFunction}{String indicating the optimization function which
should be used (Default: 'logLikelihood')}

\item{nInitialGuesses}{Integer indicating number of initial guesses
(in addition to the default initial guess) used for all four combinations
of signSus and signTrans, which both can be -1 or 1 (Default: 50).}

\item{sameSign}{Boolean indicating if sign of sustained RTF part
(signSus) and transient RTF part (signTrans) should be equal
(Default: TRUE).}

\item{plotAllPointsWaterfall}{Boolean indicating if all points should be
plotted in waterfall plot (Default: FALSE).
If FALSE, all values up to the median of those values are plotted.}

\item{control}{List of control arguments passed to the function stats::optim
(Default: list(trace = 1, maxit = 1000, factr = 1e7))}

\item{resOld}{(Optional) Result of a previous RTF run, with which results
of current run will be combined}
}
\value{
List of the final RTF model ('finalModel') and the optimized
parameters ('finalParams').
}
\description{
Run RTF
}
\examples{
# Single-dose RTF
data <- getSimData()
res <- RTF(data, modus = "singleDose")

# Dose-dependent RTF
data <- getSimData(modus = "doseDependent")
res <- RTF(data, modus = "doseDependent")
}
