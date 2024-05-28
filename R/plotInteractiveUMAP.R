#' Generates dynamic UMAP plot based on the RTF parameters for multiple time
#' courses
#'
#' @description Generates dynamic UMAP plot based on the RTF parameters for
#' multiple time courses, where each point corresponds to a single time course.
#' By hovering over a point the corresponding time-resolved behavior is
#' displayed in an additional smaller subplot.
#' @return Plotly object of the interactive UMAP plot.
#' @param df Data frame with the first column corresponding to the time points
#' and all the following columns corresponding to the different time courses.
#' @param fileString String of the name of the created .rds file with the RTF
#' results.
#' @param conditions Vector specifying the condition for each time course.
#' Needs to have the same length as the number of rows of df.
#' @param nInitialGuesses Integer indicating number of initial guesses
#' (in addition to the default initial guess) used for all four combinations
#' of signSus and signTrans, which both can be -1 or 1 (Default: 50).
#' @param sameSign Boolean indicating if sign of sustained RTF part
#' (signSus) and transient RTF part (signTrans) should be equal
#' (Default: TRUE) (Only relevant if RTFmodelLst or param.df is not provided).
#' @param takeRank Boolean indicating if rank should be used for UMAP instead
#' of absolute value (Default: TRUE).
#' @param scaled Boolean indicating if values used for UMAP should be scaled
#' for the time courses in a cluster (Default: TRUE). Only relevant if
#' takeRank = FALSE.
#' @param dimX Integer from 1 to 3 indicating the UMAP dimension plotted on
#' the x axis (Default: 1).
#' @param dimY Integer from 1 to 3 indicating the UMAP dimension plotted
#' on the y axis (Default: 2).
#' @param seed Seed for UMAP.
#' @param plotLines Boolean indicating if lines should be plotted between
#' species of the same name (Default: TRUE). Can only be plotted if there are at
#' most two time courses per species.
#' @param species Vector with names of molecular species for each time course.
#' Needs to have the same length as the number of rows of df.
#' Needs to be provided in plotLines = TRUE.
#' @param hRatio Float between 0 and 1 indicating where subplot should be placed
#' horizontally in relation to plot width (Default: 0).
#' @param vRatio Float between 0 and 1 indicating where subplot should be placed
#' vertically in relation to plot height (Default: 0).
#' @param RTFmodelLst (Optional) List with the RTF result for each time course.
#' If this has already been calculated using the function
#' getParamsFromMultipleTimeCourses() it can be provided here to save time, if
#' param.df is provided as well.
#' @param param.df (Optional) data frame containing the RTF parameters for
#' each time course, where the rows correspond to the different time courses
#' and the columns to the RTF parameters.
#' If this has already been calculated using the function
#' getParamsFromMultipleTimeCourses() it can be provided here to save time, if
#' RTFmodelLst is provided as well.
#' @export plotInteractiveUMAP
#' @examples
#' timeCourses <- almaden
#' RTFmodelLst <- almadenModelLst
#' param.df <- almadenParams
#' colNames <- colnames(timeCourses[2:ncol(timeCourses)])
#' plt <- plotInteractiveUMAP(
#'     df = timeCourses,
#'     fileString = "almadenInteractiveUMAP",
#'     conditions = gsub(".*_", "", colNames),
#'     species = sub("_[^_]+$", "", colNames),
#'     takeRank = TRUE,
#'     seed = 222,
#'     vRatio = 0.05,
#'     hRatio = 0.35,
#'     RTFmodelLst = RTFmodelLst,
#'     param.df = param.df
#' )
plotInteractiveUMAP <- function(df,
                                fileString = "interactiveUMAP",
                                conditions = c(),
                                nInitialGuesses = 50,
                                sameSign = TRUE,
                                takeRank = TRUE,
                                scaled = TRUE,
                                dimX = 1,
                                dimY = 2,
                                seed = 111,
                                plotLines = TRUE,
                                species = c(),
                                hRatio = 0,
                                vRatio = 0,
                                RTFmodelLst = NULL,
                                param.df = NULL) {
    if (is.null(RTFmodelLst) | is.null(param.df)) {
        params.lst <- getParamsFromMultipleTimeCourses(
            df = df,
            fileString = fileString,
            saveFolderPath = getwd(),
            nInitialGuesses = nInitialGuesses,
            sameSign = sameSign
        )
        param.df <- params.lst[["param.df"]]
        RTFmodelLst <- params.lst[["RTFmodelLst"]]
    }

    param.df.wConditions <- data.frame(param.df, conditions)
    colnames(param.df.wConditions) <- c(colnames(param.df), "conditions")
    gg.umap.conditions <- plotUMAP(
        df = param.df.wConditions,
        groupColName = "conditions",
        takeRank = takeRank,
        scaled = scaled,
        dimX = dimX,
        dimY = dimY,
        seed = seed
    )

    df.umap.conditions <- gg.umap.conditions[["data"]]

    ID <- row.names(df.umap.conditions)
    plt <- plotInteractive2DPlot(
        dim1Vec = df.umap.conditions$UMAP1,
        dim2Vec = df.umap.conditions$UMAP2,
        ID = ID,
        species = species,
        RTFmodelLst = RTFmodelLst,
        metaInfo = conditions,
        plotLines = plotLines,
        hRatio = hRatio,
        vRatio = vRatio
    )
    plt
}
