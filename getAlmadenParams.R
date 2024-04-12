library(RTF)

data(almaden)
fileString <- "almaden"

almadenParams <-
  RTF::getParamsFromMultipleTimeSeries(
    almaden,
    fileString = fileString,
    nInitialGuesses = 100
  )

write.csv(almadenParams, "almadenParams.csv", row.names = FALSE)
save(almadenParams, file = "almadenParams.rda", version = 2)

almadenModelLst <- readRDS(file = paste0(fileString, ".RDS"))
save(almadenModelLst, file = "almadenModelLst.rda", version = 2)

RTF::plotRTFForAllTimeSeries( # later called plotRTFOnMultipleTimeSeries
  almadenModelLst,
  fileString = fileString,
  plotFitsToSingleFile = FALSE,
  plotFitsToSingleFileExtension = "jpeg",
  plotFitOnly = TRUE,
  height = 7,
  width = 7
)

session <- sessionInfo()
sink(paste0("getAlmadenParams_sessionInfo.txt"))
print(session)
sink()
