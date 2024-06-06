#' Plot RTF, optionally together with experimental data points
#'
#' @description Plot RTF, optionally together with experimental data points.
#' In addition, the sustained and transient part of the RTF are plotted.
#' @return ggplot object showing RTF, if withData = TRUE together with
#' experimental data points
#' @param par Vector containing
#' alpha, beta, gamma, A, B, b, tau, signSus, and signTrans
#' (modus 'singleDose') or M_alpha, h_alpha, K_alpha, M_gamma, h_gamma,
#' K_gamma, M_A, h_A, K_A, M_B, h_B, K_B, M_tau, h_tau, K_tau, signSus,
#' and signTrans (modus 'doseDependent').
#' @param withData Boolean indicating if data should be added to fit line
#' @param modus Modus ('singleDose' or 'doseDependent')
#' @param y Experimental outcome for time points (corresponds to y column in
#' experimental data frame)
#' @param t Time points
#' @param d Dose (obligatory for 'doseDependent')
#' @param title Plot title
#' @param pointAlpha Transparency of points (Default: 0.5).
#' @param pointSize Point size (Default: 0.75)
#' @param lineAlpha Transparency of line (Default: 0.5 for modus 'singleDose', 
#' else 1).
#' @param lineWidth Width of line (Default: 1.5 for modus 'singleDose', 
#' else 1).
#' @param color Only relevant if modus = 'singleDose': Color of line and points
#' (Default: "#290AD8")
#' @export plotFit
#' @examples
#' gg <- plotFit(
#'     par = c(
#'         alpha = 1.00, beta = 1.00, gamma = 1.00,
#'         A = 1.05, B = 3.05,
#'         b = -0.28, tau = -1,
#'         signSus = 1,
#'         signTrans = 1
#'     ),
#'     withData = TRUE,
#'     y = c(0.45, 0.96, 1.13, 1.1, 0.9, 0.76, 0.78),
#'     t = c(0, 0.7, 1.2, 1.55, 2.3, 7.45, 10)
#' )
plotFit <- function(par,
                    withData = FALSE,
                    modus = "singleDose",
                    y = NULL,
                    t = NULL,
                    d = NULL,
                    title = "",
                    pointAlpha = 0.5,
                    pointSize = 0.75,
                    lineAlpha = NULL,
                    lineWidth = NULL,
                    color = "#290AD8") {
    
    if (modus == "singleDose") {
        if (is.null(lineAlpha)) lineAlpha <- 0.5
        if (is.null(lineWidth)) lineWidth <- 1.5
    } else {
        if (is.null(lineAlpha)) lineAlpha <- 1
        if (is.null(lineWidth)) lineWidth <- 1
    }
    
    if (is.null(t)) {
        stop("Please provide vector of time points or maximum time point.")
    }

    if (!is.null(d)) {
        doses <- sort(unique(d))
    } else {
        doses <- 1
    }

    if (modus == "singleDose") {
        xi <- seq(min(t), max(t), length.out = 1000)
        RTFResVec <- getTransientFunctionResult(
            t = xi,
            rtfPar = par,
            signSus = par[["signSus"]],
            signTrans = par[["signTrans"]]
        )

        RTFResDf <- data.frame(
            t = xi,
            y = RTFResVec,
            Component = "RTF"
        )

        # Only Signal_sus: B = 0
        # Only Signal_trans: A = 0
        parSus <- parTrans <- par
        parSus[names(parSus) == "B"] <- 0
        parTrans[names(parTrans) == "A"] <- 0

        susOnlyResVec <- getTransientFunctionResult(
            t = xi,
            rtfPar = parSus,
            signSus = parSus[["signSus"]],
            signTrans = parSus[["signTrans"]]
        )

        transOnlyResVec <- getTransientFunctionResult(
            t = xi,
            rtfPar = parTrans,
            signSus = parTrans[["signSus"]],
            signTrans = parTrans[["signTrans"]]
        )

        geom_line.df <- data.frame(
            t = xi,
            Sustained = susOnlyResVec,
            Transient = transOnlyResVec,
            RTF = RTFResVec
        )
    } else if (modus == "doseDependent") {
        geom_line.lst <- list()
        for (i in seq(length(doses))) {
            RTFResVec <- NULL
            dose <- doses[i]
            xi <- seq(min(t[d == dose]), max(t[d == dose]), length.out = 1000)

            rtfPar <- getHillResults(d = dose, params = par)

            RTFResVec <- getTransientFunctionResult(
                rtfPar = rtfPar,
                t = xi,
                signSus = par[["signSus"]],
                signTrans = par[["signTrans"]],
                scale = TRUE, # TODO TRUE or FALSE?
                calcGradient = FALSE
            )

            geom_line.lst <- append(
                geom_line.lst,
                list(data.frame(
                    t = xi,
                    y = RTFResVec,
                    d = dose
                ))
            )
        }
        geom_line.df <- dplyr::bind_rows(geom_line.lst)
    }

    if (length(doses) > 1) {
        geom_line.df <- geom_line.df[order(geom_line.df$d), ]
        gg <- ggplot2::ggplot(
            data = geom_line.df,
            ggplot2::aes(
                x = t,
                y = y,
                color = factor(d)
            )
        ) +
            ggplot2::theme_bw() +
            ggplot2::geom_line() +
            ggplot2::theme(
                legend.position = "bottom",
                legend.title = ggplot2::element_blank()
            ) +
            ggplot2::scale_colour_viridis_d(direction = -1)
        if (withData) {
            gg <- gg + ggplot2::geom_point(
                data = data.frame(t = t, y = y),
                ggplot2::aes(
                    x = t,
                    y = y,
                    color = factor(d)
                ),
                alpha = pointAlpha,
                size = pointSize
            )
        }
    } else {
        if (((par[["signSus"]] == 1) & (par[["signTrans"]] == 1)) |
            ((par[["signSus"]] == -1) & (par[["signTrans"]] == -1))) {
            if ((par[["signTrans"]] == 1)) {
                limit <- min(geom_line.df$RTF)
            } else if (par[["signTrans"]] == -1) {
                limit <- max(geom_line.df$RTF)
            }

            gg <- ggplot2::ggplot(geom_line.df, ggplot2::aes(x = t, y = RTF)) +
                ggplot2::theme_bw() +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = Sustained, ymax = RTF),
                    fill = "#7F7F7F", alpha = .5
                ) +
                ggplot2::geom_ribbon(
                    ggplot2::aes(
                        ymin = limit,
                        ymax = Sustained
                    ),
                    fill = "#262626", alpha = .5
                ) +
                ggplot2::geom_line(
                    ggplot2::aes(y = RTF),
                    size = lineWidth, alpha = lineAlpha,
                    color = color
                ) +
                ggplot2::ylab("y")
        } else {
            limit <- geom_line.df$RTF[1]

            if ((par[["signSus"]] == 1) &
                (par[["signTrans"]] == -1)) {
                ribbonTransYmin <-
                    abs(geom_line.df$Transient - min(geom_line.df$Transient)) -
                    max(abs(geom_line.df$Transient -
                        min(geom_line.df$Transient))) + limit
                ribbonTransYmax <- limit
                ribbonSusYmin <- limit
                ribbonSusYmax <-
                    abs(geom_line.df$Sustained -
                        min(geom_line.df$Sustained)) + limit
            } else if ((par[["signSus"]] == -1) &
                (par[["signTrans"]] == 1)) {
                ribbonTransYmin <- limit
                ribbonTransYmax <-
                    abs(geom_line.df$Transient -
                        min(geom_line.df$Transient)) + limit
                ribbonSusYmin <- abs(geom_line.df$Sustained -
                    min(geom_line.df$Sustained)) -
                    max(abs(geom_line.df$Sustained -
                        min(geom_line.df$Sustained))) + limit
                ribbonSusYmax <- limit
            }

            gg <- ggplot2::ggplot(geom_line.df, ggplot2::aes(x = t, y = RTF)) +
                ggplot2::theme_bw() +
                ggplot2::geom_ribbon(
                    ggplot2::aes(
                        ymin = ribbonTransYmin,
                        ymax = ribbonTransYmax
                    ),
                    fill = "#7F7F7F", alpha = .5
                ) +
                ggplot2::geom_ribbon(
                    ggplot2::aes(
                        ymin = ribbonSusYmin,
                        ymax = ribbonSusYmax
                    ),
                    fill = "#262626", alpha = .5
                ) +
                ggplot2::geom_line(
                    ggplot2::aes(y = RTF),
                    size = lineWidth, alpha = lineAlpha,
                    color = color
                ) +
                ggplot2::ylab("y")
        }

        if (withData) {
            gg <- gg + ggplot2::geom_point(
                data = data.frame(t = t, y = y),
                ggplot2::aes(x = t, y = y),
                alpha = pointAlpha,
                size = pointSize,
                colour = color
            )
        }
    }

    if (!is.null(d)) {
        gg <- gg + ggplot2::labs(color = "Dose")
    }
    if (nchar(title) > 0) {
        gg <- gg + ggplot2::ggtitle(title)
    }

    gg
}
