#' Generates UMAP plot based on the RTF parameters for multiple time series
#'
#' @description Generates UMAP plot based on the RTF parameters for multiple 
#' time series
#' @return ggplot2 object of UMAP plot ('plot')
#' @param df Data frame containing columns corresponding to the values for each
#' RTF parameter as well as a column indicating how the data points in the UMAP
#' plot should be colored (name of this column given by 'groupColName').
#' @param groupColName String with the column name of the groups based on which 
#' the data points of the UMAP plot should be colored.
#' @param takeRank Boolean indicating if rank should be used for UMAP instead 
#' of absolute value
#' @param alpha Alpha value (between 0 and 1) of the data points in the UMAP 
#' plot
#' @param size Size of the data points in the UMAP plot
#' @param ellipse Boolean indicating if ellipse should be drawn
#' @param ellipseLevel The level at which to draw an ellipse (between 0 and 1)
#' @param seed Seed
#' @export getUMAPplot
#' @importFrom dplyr %>%
#' @examples
#' data(strasenParams)
#' metaInfo <- sub("_[^_]+$", "", row.names(strasenParams))
#' metaInfoName <- "species"
#' param.df.wMetaInfo <- data.frame(strasenParams, metaInfo)
#' colnames(param.df.wMetaInfo) <- c(colnames(strasenParams), metaInfoName)
#' gg.umap.metaInfo <- getUMAPplot(df = param.df.wMetaInfo,
#'                                 groupColName = metaInfoName,
#'                                 alpha = 1, size = 1.5)

getUMAPplot <- function(df, groupColName = "", takeRank = TRUE,
                        alpha = 0.3, size = 0.8, 
                        ellipse = TRUE, ellipseLevel = 0.68, seed = 111) {
  # set.seed(142)
  
  tryCatch({
    if (takeRank) {
      umap_fit <- df %>% dplyr::mutate(ID = dplyr::row_number())  %>%
        dplyr::select(-!!groupColName) %>% 
        dplyr::select_if(~ !any(is.na(.))) %>%
        tibble::remove_rownames() %>% tibble::column_to_rownames("ID") %>%
        dplyr::mutate_all(rank) %>%
        umap::umap(n_components = 3, preserve.seed = TRUE, random_state = seed)
    } else {
      umap_fit <- df %>% dplyr::mutate(ID = dplyr::row_number())  %>%
        dplyr::select(-!!groupColName) %>% 
        dplyr::select_if(~ !any(is.na(.))) %>%
        tibble::remove_rownames() %>% tibble::column_to_rownames("ID") %>%
        scale() %>%
        umap::umap(n_components = 3, preserve.seed = TRUE, random_state = seed)
    }

  }
  , error = function(e) {
    stop(paste0(e, " Too few time series to run Low dimensional RTF analysis."))
  })
  
  # umap_fit <- df %>% dplyr::mutate(ID = dplyr::row_number())  %>%
  #   dplyr::select(-!!groupColName) %>% 
  #   dplyr::select_if(~ !any(is.na(.))) %>%
  #   tibble::remove_rownames() %>% tibble::column_to_rownames("ID") %>%
  #   scale() %>%
  #   umap::umap(n_components = 3, preserve.seed = TRUE)
  
  groupVec <- df[[groupColName]]
  umap_df <- umap_fit$layout %>%
    as.data.frame() %>%
    dplyr::rename(UMAP1 = "V1",
                  UMAP2 = "V2",
                  UMAP3 = "V3") %>%
    dplyr::mutate(!!groupColName := !!groupVec)
  row.names(umap_df) <- row.names(df)
  
  gg <- ggplot2::ggplot(umap_df, ggplot2::aes(x = UMAP1, y = UMAP2, 
                                              color = get(groupColName))) +
    ggplot2::geom_point(size = size, alpha = alpha)  + 
    ggplot2::theme_bw() +
    ggplot2::theme(legend.direction = 'horizontal', 
                   legend.position = 'bottom') +
    ggplot2::scale_color_discrete(name = '') +
    ggplot2::guides(colour = ggplot2::guide_legend(
      override.aes = list(alpha = 1, size = 3)))
  
  if (ellipse)
    gg <- gg + ggplot2::stat_ellipse(level =  ellipseLevel)
  
  gg
}
