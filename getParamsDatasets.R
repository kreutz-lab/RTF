library(RTF)

data(strasen)
fileString <- "strasen"

almadenParams <-
    RTF::getParamsFromMultipleTimeCourses(
        strasen,
        fileString = fileString,
        nInitialGuesses = 100
    )

write.csv(strasenParams, "strasenParams.csv", row.names = FALSE)
save(strasenParams, file = "strasenParams.rda", version = 2)

strasenModelLst <- readRDS(file = paste0(fileString, ".RDS"))
save(strasenModelLst, file = "strasenModelLst.rda", version = 2)

################################################################################

data(almaden)
fileString <- "almaden"

almadenParams <-
  RTF::getParamsFromMultipleTimeCourses(
    almaden,
    fileString = fileString,
    nInitialGuesses = 100
  )

write.csv(almadenParams, "almadenParams.csv", row.names = FALSE)
save(almadenParams, file = "almadenParams.rda", version = 2)

almadenModelLst <- readRDS(file = paste0(fileString, ".RDS"))
save(almadenModelLst, file = "almadenModelLst.rda", version = 2)

RTF::plotRTFOnMultipleTimeCourses( 
  almadenModelLst,
  fileString = fileString,
  plotFitsToSingleFile = FALSE,
  plotFitsToSingleFileExtension = "jpeg",
  plotFitOnly = TRUE,
  height = 7,
  width = 7
)

################################################################################

session <- sessionInfo()
sink(paste0("getParamsDatasets_sessionInfo.txt"))
print(session)
sink()


