#' Log-likelihood optimization function
#'
#' @description Log-likelihood optimization function
#' @return Single optimization value if calcGradient = FALSE or named vector
#' with optimization value for each parameter if calcGradient = TRUE
#' @param par Initial values for the parameters to be optimized over.
#' @param data Data frame containing columns named 't' (time), 'y' (quantitative
#' value) and optionally 'sigmaExp' (standard error of the experimental data)
#' @param optimObject optimObject
#' @param calcGradient Boolean indicating if gradient should be calculated
#' (Default: FALSE)
#' @export objFunct
#' @examples
#' # modus "singleDose"
#' optimObject.singleDose <- list(
#'     data = structure(
#'         list(
#'             t = c(
#'                 0, 0.7, 1.4, 2.1, 2.8, 3.5,
#'                 4.2, 4.9, 5.6, 6.3, 7, 7.7, 8.4, 9.1, 9.8, 10.5, 11.2, 11.9,
#'                 12.6, 13.3, 14, 14.7, 15.4, 16.1, 16.8
#'             ),
#'             y = c(
#'                 2.01377848917595,
#'                 2.00723150384742, 1.95383023987706, 2.02401934780125,
#'                 1.99101642077785,
#'                 1.99438185391122, 1.96782895298547, 1.99232892596512,
#'                 2.04023699145328,
#'                 2.00987713004573, 2.04907131050778, 2.10220682031083,
#'                 2.0667888587053,
#'                 2.13146981736463, 2.13083497439212, 2.14307404336409,
#'                 2.16454894647236,
#'                 2.18975601505842, 2.21300572106394, 2.24987752491394,
#'                 2.18137275078123,
#'                 2.22381313982203, 2.28992651825763, 2.18074803633825,
#'                 2.29384492954725
#'             )
#'         ),
#'         class = "data.frame",
#'         row.names = c(NA, -25L)
#'     ),
#'     initialGuess.vec =
#'         c(
#'             alpha = 0.0412393049421161, beta = 0.0412393049421161,
#'             gamma = 0.0412393049421161, A = 0.612026441406334,
#'             B = 0.612026441406334,
#'             b = 2.12383758471215, tau = -1.68, sigma = 0.0525708346158025
#'         ),
#'     lb.vec = c(
#'         alpha = 0.000595238095238095,
#'         beta = 0.000595238095238095,
#'         A = 0, B = 0, b = 1.95383023987706,
#'         tau = -3.36, sigma = 0.000340014689670185
#'     ),
#'     ub.vec = c(
#'         alpha = 2.85714285714286, beta = 2.85714285714286,
#'         A = 0.680029379340371, B = 0.680029379340371,
#'         b = 2.29384492954725,
#'         tau = 15.12, sigma = 0.104801654541935
#'     ),
#'     fixed = c(
#'         alpha = NA, beta = NA,
#'         gamma = 0.301029995663981, A = NA, B = NA, b = NA, tau = NA,
#'         sigma = NA, signSus = 1, signTrans = 1
#'     ),
#'     takeLog10 = c(
#'         alpha = TRUE, beta = TRUE, gamma = TRUE,
#'         A = FALSE, B = FALSE, b = TRUE, tau = FALSE, sigma = FALSE
#'     ),
#'     positive.par.names = c("alpha", "beta", "gamma", "b"),
#'     modus = "singleDose",
#'     optimFunction = "logLikelihood",
#'     control =
#'         list(
#'             trace = 1,
#'             maxit = 1000, factr = 1e+07,
#'             ndeps = c(
#'                 alpha = -3.22530928172586, beta = -3.22530928172586,
#'                 A = 6.80029379340371e-06, B = 6.80029379340371e-06,
#'                 b = 0.001,
#'                 tau = 0.0001848, sigma = 1.04461639852265e-06
#'             )
#'         ),
#'     fitted = list()
#' )
#'
#' vec.singleDose <- c(
#'     alpha = -1.38468866303807, beta = -1.38468866303807,
#'     A = 0.612026441406334,
#'     B = 0.612026441406334,
#'     b = 0.327121302070328,
#'     tau = -1.68, sigma = 0.0525708346158025
#' )
#' optimRes.singleDose <- stats::optim(
#'     par = vec.singleDose,
#'     fn = objFunct,
#'     gr = objFunctGradient,
#'     method = "L-BFGS-B",
#'     lower = optimObject.singleDose$lb.vec,
#'     upper = optimObject.singleDose$ub.vec,
#'     data = optimObject.singleDose$data,
#'     optimObject = optimObject.singleDose,
#'     calcGradient = FALSE,
#'     control = optimObject.singleDose$control
#' )
#'
#' # modus "doseDependent"
#' optimObject.doseDependent <- list(
#'     data = structure(
#'         list(
#'             t = c(
#'                 2, 0, 0.7, 1.4, 2.1, 2.8, 3.5,
#'                 4.2, 4.9, 5.6, 6.3, 7, 7.7, 8.4, 9.1, 9.8, 10.5, 11.2, 11.9,
#'                 12.6, 13.3, 14, 14.7, 15.4, 16.1, 16.8, 0, 0.7, 1.4, 2.1, 2.8,
#'                 3.5, 4.2, 4.9, 5.6, 6.3, 7, 7.7, 8.4, 9.1, 9.8, 10.5, 11.2, 11.9,
#'                 12.6, 13.3, 14, 14.7, 15.4, 16.1, 16.8, 0, 0.7, 1.4, 2.1, 2.8,
#'                 3.5, 4.2, 4.9, 5.6, 6.3, 7, 7.7, 8.4, 9.1, 9.8, 10.5, 11.2, 11.9,
#'                 12.6, 13.3, 14, 14.7, 15.4, 16.1, 16.8, 0, 0.7, 1.4, 2.1, 2.8,
#'                 3.5, 4.2, 4.9, 5.6, 6.3, 7, 7.7, 8.4, 9.1, 9.8, 10.5, 11.2, 11.9,
#'                 12.6, 13.3, 14, 14.7, 15.4, 16.1, 16.8
#'             ),
#'             y = c(
#'                 NA, 0.402257345034451, 0.266217153220169, 0.435413325854042,
#'                 0.0819913122734036, 0.242347666824238,
#'                 0.788600976947511, 0.75944552925416, 1.46078677965929,
#'                 1.39153951008744,
#'                 1.51773562782859, 1.71229574761147, 1.6484714427425,
#'                 1.6553008770675,
#'                 1.37614191263719, 1.50683842708258, 1.3835565818699,
#'                 1.25948900535055,
#'                 1.38331576248608, 1.10168010379371, 1.63354417953745,
#'                 1.11337376122112,
#'                 1.3432654500857, 1.31385410736916, 1.08535708321926,
#'                 0.939366767699398,
#'                 0.281266092859909, 0.235958987509578, 0.257174537967354,
#'                 0.257596160404742,
#'                 0.131323053153022, 0.814756625861746, 1.9556070593933,
#'                 2.34247161610908,
#'                 3.36590991892609, 2.98445002514665, 2.57180198673554,
#'                 2.46852511812208,
#'                 2.30205457051659, 2.12177688203478, 1.64885390789293,
#'                 1.5978422949694,
#'                 1.70791612189577, 1.42672466964502, 1.5336879562476,
#'                 1.57265870149065,
#'                 1.7913428871832, 1.30207047395272, 1.40254888235878,
#'                 1.63684127583152,
#'                 1.09392943683496, 0.593498832888432, 0.322300052001033,
#'                 0.430630403141288,
#'                 0.464519906538464, 0.553104972239775, 1.570973047572,
#'                 3.24710288041198,
#'                 5.49279598610643, 5.6986449301012, 5.56280654814025,
#'                 5.18854187944511,
#'                 4.09185207359011, 3.88221905558804, 3.25005581993081,
#'                 2.7954127845873,
#'                 2.67291956708053, 1.88653553289802, 2.40490173199245,
#'                 1.58028258230218,
#'                 1.56059505945802, 1.67374824964235, 1.4208334056463,
#'                 1.1855684679459,
#'                 1.25759861094918, 1.66528871691006, 0.188903687333398,
#'                 0.272268776198665,
#'                 0.523848892262106, 0.310772181112352, 0.756979081604201,
#'                 2.55895030102469,
#'                 6.47875189759571, 9.719659469997, 10.7624834451922,
#'                 9.58905210614822,
#'                 8.67208238293666, 7.18197490196731, 5.9909255047158,
#'                 4.58072158292929,
#'                 4.28477836249586, 3.70354139529079, 3.08076948026664,
#'                 2.59717263148153,
#'                 2.4134991372254, 2.02419765422541, 2.017731297372,
#'                 1.73727816295749,
#'                 1.81547221646639, 1.81534104024578, 1.40861895347188
#'             ),
#'             d = c(
#'                 2,
#'                 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'                 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
#'                 4, 4, 4, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
#'                 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 9, 9,
#'                 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
#'             )
#'         ),
#'         row.names = c(NA, -101L), class = "data.frame"
#'     ),
#'     initialGuess.vec =
#'         c(
#'             M_alpha = 0.109108945117996, h_alpha = 3.16227766016838,
#'             K_alpha = 4.24264068711928,
#'             M_beta = 0.109108945117996, h_beta = 3.16227766016838,
#'             K_beta = 4.24264068711928,
#'             M_gamma = 0.109108945117996,
#'             h_gamma = 3.16227766016838, K_gamma = 4.24264068711928,
#'             M_A = 96.1244291962694,
#'             h_A = 5.5, K_A = 45.1, M_B = 96.1244291962694, h_B = 5.5, K_B = 45.1,
#'             M_tau = -1.68, h_tau = 5.5, K_tau = 45.1, b = 5.42223737873282,
#'             sigma = 1.07250964165806
#'         ),
#'     lb.vec = c(
#'         M_alpha = 0.000595238095238095,
#'         h_alpha = 1, K_alpha = 0.2,
#'         M_beta = 0.000595238095238095,
#'         h_beta = 1, K_beta = 0.2,
#'         h_gamma = 1, K_gamma = 0.2, M_A = 0,
#'         h_A = 1, K_A = 0.2, M_B = 0, h_B = 1, K_B = 0.2, M_tau = -3.36,
#'         h_tau = 1, K_tau = 0.2, b = 0.0819913122734036,
#'         sigma = 0.0106804921329188
#'     ),
#'     ub.vec = c(
#'         M_alpha = 20.0000000000001, h_alpha = 10, K_alpha = 90,
#'         M_beta = 20.0000000000001, h_beta = 10, K_beta = 90,
#'         h_gamma = 10, K_gamma = 90, M_A = 106.804921329188, h_A = 10,
#'         K_A = 90, M_B = 106.804921329188, h_B = 10, K_B = 90,
#'         M_tau = 15.12,
#'         h_tau = 10, K_tau = 90, b = 10.7624834451922,
#'         sigma = 2.1343387911832
#'     ),
#'     fixed = c(
#'         M_alpha = NA, h_alpha = NA, K_alpha = NA,
#'         M_beta = NA, h_beta = NA, K_beta = NA,
#'         M_gamma = -0.301029995663981,
#'         h_gamma = NA, K_gamma = NA, M_A = NA, h_A = NA, K_A = NA,
#'         M_B = NA,
#'         h_B = NA, K_B = NA, M_tau = NA, h_tau = NA, K_tau = NA, b = NA,
#'         sigma = NA, signSus = 1, signTrans = 1
#'     ),
#'     takeLog10 = c(
#'         M_alpha = TRUE, h_alpha = TRUE, K_alpha = TRUE,
#'         M_beta = TRUE, h_beta = TRUE, K_beta = TRUE,
#'         M_gamma = TRUE, h_gamma = TRUE, K_gamma = TRUE,
#'         M_A = FALSE, h_A = TRUE, K_A = TRUE, M_B = FALSE, h_B = TRUE,
#'         K_B = TRUE, M_tau = FALSE, h_tau = TRUE, K_tau = TRUE,
#'         b = TRUE,
#'         sigma = FALSE
#'     ),
#'     positive.par.names = c(
#'         "M_alpha", "h_alpha", "K_alpha",
#'         "M_beta", "h_beta", "K_beta",
#'         "M_gamma", "h_gamma", "K_gamma",
#'         "h_A", "K_A", "h_B", "K_B", "h_tau",
#'         "K_tau", "b"
#'     ),
#'     modus = "doseDependent", optimFunction = "logLikelihood",
#'     control = list(trace = 1, maxit = 1000, factr = 1e+07),
#'     fitted = list()
#' )
#' vec.doseDependent <- c(
#'     M_alpha = -0.96213964303094, h_alpha = 0.5,
#'     K_alpha = 0.627636252551653,
#'     M_beta = -0.96213964303094, h_beta = 0.5,
#'     K_beta = 0.627636252551653,
#'     h_gamma = 0.5, K_gamma = 0.627636252551653, M_A = 96.1244291962694,
#'     h_A = 0.740362689494244, K_A = 1.65417654187796,
#'     M_B = 96.1244291962694,
#'     h_B = 0.740362689494244, K_B = 1.65417654187796, M_tau = -1.68,
#'     h_tau = 0.740362689494244, K_tau = 1.65417654187796,
#'     b = 0.734178526526858,
#'     sigma = 1.07250964165806
#' )
#'
#' optimRes.doseDependent <- stats::optim(
#'     par = vec.doseDependent,
#'     fn = objFunct,
#'     gr = objFunctGradient,
#'     method = "L-BFGS-B",
#'     lower = optimObject.doseDependent$lb.vec,
#'     upper = optimObject.doseDependent$ub.vec,
#'     data = optimObject.doseDependent$data,
#'     optimObject = optimObject.doseDependent,
#'     calcGradient = FALSE,
#'     control = optimObject.doseDependent$control
#' )
objFunct <- function(par, data, optimObject, calcGradient = FALSE) {
    retval <- NULL
    parOrder <- names(par)

    data <- data[stats::complete.cases(data), ]

    fixed <- optimObject[["fixed"]]
    signSus <- fixed[["signSus"]]
    signTrans <- fixed[["signTrans"]]
    if ("sigmaExp" %in% colnames(data)) {
        sigma <- data$sigmaExp
    } else {
        if (!is.na(fixed[["sigma"]])) {
            sigma <- fixed[["sigma"]]
        } else {
            sigma <- par[["sigma"]]
        }
    }

    allParamNames <- setdiff(names(fixed), c("signSus", "signTrans"))
    fixed <- fixed[allParamNames]

    if ("d" %in% colnames(data)) {
        d <- data$d
    } else {
        d <- rep(1, nrow(data))
    }

    parAfterFix <- par
    # Fixed parameters will overwrite the values in par
    for (v in 1:length(fixed)) {
        if (!is.na(fixed[[v]])) {
            parAfterFix[names(fixed)[v]] <- fixed[[v]]
        }
    }

    if (calcGradient) {
        dparAfterFix_dpar <-
            matrix(0, nrow = length(parAfterFix), ncol = length(parOrder))
        rownames(dparAfterFix_dpar) <- names(parAfterFix)
        colnames(dparAfterFix_dpar) <- parOrder
        for (name in parOrder) dparAfterFix_dpar[name, name] <- 1

        dpar_dpar <- applyLog10ForTakeLog10(
            parAfterFix, optimObject[["takeLog10"]],
            reverse = TRUE, calcGradient = TRUE
        )
    }

    parAfterFix <- applyLog10ForTakeLog10(
        parAfterFix, optimObject[["takeLog10"]],
        reverse = TRUE, calcGradient = FALSE
    )

    ds <- unique(d)
    res <- array(NA, dim = length(d))
    sigmaRes <- array(NA, dim = length(d))
    hillHasNaNs <- FALSE

    if (calcGradient) {
        dres_dpar <- matrix(nrow = length(d), ncol = length(parAfterFix))
        colnames(dres_dpar) <- names(parAfterFix)

        dsigmaRes_dpar <-
            matrix(0, nrow = length(d), ncol = length(parAfterFix))
        colnames(dsigmaRes_dpar) <- names(parAfterFix)

        dretval_dres <- matrix(nrow = length(d), ncol = 1)
    }

    for (id in 1:length(ds)) { # loop over all doses
        ind <- which(d == ds[id]) # indices where dose matches

        if (optimObject$modus == "doseDependent") {
            hillF <- getHillResults(d = ds[id], params = parAfterFix)
            if (sum(is.nan(hillF)) > 0) {
                hillHasNaNs <- TRUE
            }

            rtfPar <- hillF

            if (calcGradient) {
                # length(rtfPara) x length(par),  length(par) something like 15
                dhillF_dpar <- getHillResults(
                    d = ds[id],
                    params = parAfterFix,
                    calcGradient = TRUE
                )
                drtfPar_dpar <- dhillF_dpar # drtfPar_dhillF %*% dhillF_dpar
            }
        } else {
            rtfPar <- parAfterFix

            if (calcGradient) {
                drtfPar_dpar <- matrix(0,
                    nrow = length(rtfPar),
                    ncol = length(rtfPar)
                )
                # length(rtfPara) x length(par), length(par) something like 7
                diag(drtfPar_dpar) <- 1
            }
        }

        yRtf <- getTransientFunctionResult(
            rtfPar = rtfPar,
            t = data$t[ind],
            signSus = signSus,
            signTrans = signTrans,
            scale = TRUE,
            calcGradient = FALSE
        )

        if (calcGradient) {
            dyRtf_drtfPar <- getTransientFunctionResult(
                rtfPar = rtfPar,
                t = data$t[ind],
                signSus = signSus,
                signTrans = signTrans,
                scale = TRUE,
                calcGradient = TRUE
            )

            dyRtf_drtfPar <- cbind(dyRtf_drtfPar,
                sigma = rep(0, nrow(dyRtf_drtfPar))
            )
            dyRtf_drtfPar <- dyRtf_drtfPar[, names(rtfPar)]

            # set derivates of fixed parameters to zero
            # length(data$y) x length(par)
            dyRtf_dpar <- dyRtf_drtfPar %*% drtfPar_dpar
            dres_dpar[ind, ] <- -dyRtf_dpar
        }

        res[ind] <- data$y[ind] - yRtf

        if (("sigmaExp" %in% colnames(data))) {
            sigmaRes[ind] <- sigma[ind]
        } else {
            sigmaRes[ind] <- sigma
            if (calcGradient) {
                dsigmaRes_dpar[ind, names(parAfterFix) == "sigma"] <- 1
            }
        }
    }

    retval <-
        sum(-2 * (-0.5 * (res / sigmaRes)^2) - log((sigmaRes * (2 * pi)^(0.5))))

    if (calcGradient) {
        if (("sigmaExp" %in% colnames(data))) {
            dretval_dsigmaRes <- matrix(0, nrow = 1, ncol = length(sigmaRes))
            dretval_dres <- (2 * res) / sigmaRes^2
        } else {
            dretval_dsigmaRes <- -(2 * res^2) / sigmaRes^3 - 1 / sigmaRes

            dretval_dres <- (2 * res) / sigmaRes^2
        }

        dretval_dpar <-
            dretval_dres %*% dres_dpar + dretval_dsigmaRes %*% dsigmaRes_dpar
        # Because of log
        dretval_dpar <- dretval_dpar %*% dpar_dpar
        # Because of fixing params
        dretval_dpar <- dretval_dpar %*% dparAfterFix_dpar

        dretval_dpar <- dretval_dpar[1, ]
    }

    if (calcGradient) {
        dretval_dpar
    } else {
        if (is.infinite(retval)) retval <- 10^20
        retval
    }
}
