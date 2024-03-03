library(RTF)

data(almadenTimeSeries)
fileString <- "almaden"

almadenParams <-
  RTF::getParamsFromMultipleTimeSeries(
    almadenTimeSeries,
    fileString = fileString,
    nInitialGuesses = 200
  )

write.csv(almadenParams, "almadenParams.csv", row.names = FALSE)
save(almadenParams, file = "almadenParams.rda", version = 2)

res.lst <- readRDS(file = paste0(fileString, ".RDS"))

RTF::plotRTFForAllTimeSeries(
  res.lst,
  fileString = fileString,
  plotFitsToSingleFile = FALSE,
  plotFitOnly = TRUE,
  height = 7,
  width = 7
)

session <- sessionInfo()
sink(paste0("getAlmadenParams_sessionInfo.txt"))
print(session)
sink()
