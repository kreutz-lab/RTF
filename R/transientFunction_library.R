library(ggplot2)
library(reshape2)
library(readxl)

#################################################################################
# HELPER FUNCTIONS

getNamedVectorsFromDf <- function(df) {
  lb.vec <- df$lb.vec
  ub.vec <- df$ub.vec
  initialGuess.vec <- df$initialGuess.vec
  names(lb.vec) <- names(ub.vec) <- names(initialGuess.vec) <- row.names(df)
  list(lb.vec=lb.vec, ub.vec=ub.vec, initialGuess.vec=initialGuess.vec)
}

sortListByValue <- function(res.lst) {
  res.lst2 <- unlist(lapply(res.lst,'[[',1), recursive = FALSE)
  res.lst2 <- unlist(res.lst2[names(res.lst2) == "value"])
  sortIdx <- sort(res.lst2, index.return = TRUE)$ix
  res.lst <- res.lst[sortIdx]
  res.lst
}

getPositiveParNames <- function(lb.vec, ub.vec, initialGuess.vec) {
  boundsDefault.df <- data.frame(
    lb.vec, ub.vec, initialGuess.vec)
  par.names <- row.names(boundsDefault.df)
  positive.par.names <- par.names[which(apply(boundsDefault.df, 1, function(x) !any(x<=0)))]
  positive.par.names
}

takeLog10OfBounds <- function(optimObject) {
  boundsDefault.df <- data.frame(lb.vec = optimObject$lb.vec,
                                 ub.vec = optimObject$ub.vec,
                                 initialGuess.vec = optimObject$initialGuess.vec)
  boundsDefault.df[optimObject$positive.par.names,] <- log10(boundsDefault.df[optimObject$positive.par.names,])
  vecs <- getNamedVectorsFromDf(boundsDefault.df)
  optimObject$lb.vec <- vecs$lb.vec
  optimObject$ub.vec <- vecs$ub.vec
  optimObject$initialGuess.vec <- vecs$initialGuess.vec
  optimObject
}

getInitialGuessVec <- function(initialGuess.vec,
                               lb.vec,
                               ub.vec,
                               nInitialGuesses) {
  # For 50 different random initial guesses between bounds
  initialGuess.vec.lst <- list()
  initialGuess.vec.lst[[length(initialGuess.vec.lst) + 1]] <- initialGuess.vec

  for (i in 1:nInitialGuesses){
    randomPortions <- runif(length(initialGuess.vec))
    randomInitialGuess.vec <- (randomPortions*lb.vec) + ((1-randomPortions)*ub.vec)
    initialGuess.vec.lst[[length(initialGuess.vec.lst) + 1]] <- randomInitialGuess.vec
  }

  initialGuess.vec.lst
}
################################################################################
# DATA FUNCTIONS

getData <- function(file="", tCol="", quantCols=c(), sdExpCol = ""){
  if (file == "") {
    # Simulate data
    nrows <- 20
    signal <- function(x) {x*(x-.9)^2}
    t <- runif(nrows, 0, 1)
    y <- signal(t) + rnorm(length(t), 0, 0.025)
    sdExp <- abs(runif(nrows, 0, 0.002)*y)
    data <- data.frame(t = t, y = y)
  } else {
    if (grepl("\\.csv$", file)){
      data <- read.csv(file)
    } else if (grepl("\\.xls$", file) | grepl("\\.xlsx$", file)){
      data <- readxl::read_excel(file)
    }

    if (nchar(tCol)>0 & length(quantCols) > 0){
      data <- data[, c(tCol, quantCols)]
    }

    ncols <- ncol(data)

    if (nchar(sdExpCol) > 0) {
      # names(data)[names(data) == sdExpCol] <- "sdExp"
      colnames(data) <- c("t", paste0("rep_", 1:(ncols-2)), "sdExp")
    } else {
      colnames(data) <- c("t", paste0("rep_", 1:(ncols-1)))
    }

    repCols <- grepl('rep_', colnames(data))
    nReps <- sum(repCols)
    if (nReps == 1){
      names(data)[names(data) == "rep_1"] <- "y"
    } else {
      if (nchar(sdExpCol) > 0) {
        id.vars <- c("t", "sdExp")
      } else {
        id.vars <- c("t")
      }
      data <- reshape2::melt(data, id.vars = id.vars, value.name = "y")
      data$variable <- NULL
    }

    # else it is assumed that table has the following column order:
    # one time column, quantitative columns
  }
  data <- data[order(data$t, data$y),]
  data
}

scaleTimeCol <- function(data, scaleTime = TRUE) {
  t <- data$t
  y <- data$y

  if (scaleTime){
    t_range <- max(t)-min(t)
    t_prime <- 10*t/t_range
  } else {
    t_prime <- data$t
  }
  data$t_prime <- t_prime
  data
}

################################################################################
# PLOT FUNCTIONS

getSignalSusPlusOffset <- function(
    tau_1, A_sus, p_0, T_shift, signum_TF, t_prime) {
  nonLinTransformation <- log10(10^t_prime+10^T_shift)-log10(1+10^T_shift)
  Signal_sus <- A_sus * (1-exp(- nonLinTransformation /tau_1))
  signum_TF * Signal_sus + p_0
}

getSignalTransPlusOffset <- function(
    tau_1, tau_2, A_trans, p_0, T_shift, signum_TF, t_prime) {
  nonLinTransformation <- log10(10^t_prime+10^T_shift)-log10(1+10^T_shift)
  Signal_trans <- A_trans * (1-exp(- nonLinTransformation /tau_1))* exp(- nonLinTransformation /tau_2)
  signum_TF * Signal_trans + p_0
}

getNonLinTransformationPlusOffset <- function(T_shift, p_0, t_prime) {
  nonLinTransformation <- log10(10^t_prime+10^T_shift)-log10(1+10^T_shift)
  nonLinTransformation + p_0
}

getTransientFunctionExampleData <- function(
    tau_1, tau_2, A_sus, A_trans, p_0, T_shift, signum_TF, t_prime) {

  nonLinTransformation <- log10(10^t_prime+10^T_shift)-log10(1+10^T_shift)

  Signal_sus <- A_sus * (1-exp(- nonLinTransformation /tau_1))  # Warum tau_2? Taucht in Paperformel nicht auf
  Signal_trans <- A_trans * (1-exp(- nonLinTransformation /tau_1))* exp(- nonLinTransformation /tau_2)
  transientFunctionRes <- signum_TF * Signal_sus + signum_TF * Signal_trans + p_0

  transientFunctionRes
}

getExampleDf <- function(){
  t <- seq(0, 10, 0.7)
  y <- getTransientFunctionExampleData(tau_1=0.4, tau_2=2, A_sus=0.25, A_trans=0.5, p_0=0.3,
                                       T_shift=2.5, signum_TF=1, t_prime=t)
  data.frame(t = t, y = y + rnorm(length(t), 0, 0.03))
}

plotTvsTprime <- function(t, t_prime) {
  ggplot(data, aes(x = t, y = t_prime)) +
    geom_point(size = 1, alpha = 0.5) +
    theme_bw()
}

# t_prime vs. nonLinearTransformation
plotLinearTransformation <- function(p_0, T_shift, y, t_prime) {
  xi <- seq(0, max(t_prime), length.out = 1000)

  ggplot(data.frame(t_prime = t_prime, y = y), aes(x=t_prime, y=y)) +
    geom_point(alpha=0.5) +
    ggtitle("NonLinTransformation + p_0") +
    geom_line(data=data.frame(x = xi,
                              y = getNonLinTransformationPlusOffset(
                                t_prime = xi,
                                p_0 = p_0,
                                T_shift = T_shift)), aes(x=x,y=y)) +
    theme_bw()
}

plotSignalSus <- function(tau_1, A_sus, p_0, T_shift, signum_TF, y, t_prime) {
  xi <- seq(0, max(t_prime), length.out = 1000)

  ggplot(data.frame(t_prime = t_prime, y = y), aes(x=t_prime, y=y)) +
    geom_point(alpha=0.5) +
    ggtitle("SignalSus + p_0") +
    geom_line(data=data.frame(x = xi,
                              y = getSignalSusPlusOffset(
                                t_prime = xi,
                                tau_1 = tau_1,
                                A_sus = A_sus,
                                p_0 = p_0,
                                T_shift = T_shift,
                                signum_TF = signum_TF)), aes(x=x,y=y)) +
    theme_bw()
}

plotSignalTrans <- function(
    tau_1, tau_2, A_trans, p_0, T_shift, signum_TF, y, t_prime) {
  xi <- seq(0, max(t_prime), length.out = 1000)

  ggplot(data.frame(t_prime = t_prime, y = y), aes(x=t_prime, y=y)) +
    geom_point(alpha=0.5) +
    ggtitle("SignalTrans + p_0") +
    geom_line(data=data.frame(x = xi,
                              y = getSignalTransPlusOffset(
                                t_prime = xi,
                                tau_1 = tau_1,
                                tau_2 = tau_2,
                                A_trans = A_trans,
                                p_0 = p_0,
                                T_shift = T_shift,
                                signum_TF = signum_TF)), aes(x=x,y=y)) +
    theme_bw()
}

plotFit <- function(
    tau_1, tau_2, A_sus, A_trans, p_0, T_shift, signum_TF, y, t_prime) {
  xi <- seq(0, max(t_prime), length.out = 1000)

  ggplot(data.frame(t_prime = t_prime, y = y), aes(x=t_prime, y=y)) +
    geom_point(alpha=0.5) +
    ggtitle("RTF =  SignalSus + SignalTrans + p_0") +
    geom_line(data=data.frame(x = xi,
                              y = getTransientFunctionExampleData(
                                t_prime = xi,
                                tau_1 = tau_1,
                                tau_2 = tau_2,
                                A_sus = A_sus,
                                A_trans = A_trans,
                                p_0 = p_0,
                                T_shift = T_shift,
                                signum_TF = signum_TF)), aes(x=x,y=y)) +
    theme_bw()
}

plotWaterfallPlot <- function(optimResTmpLstValuesAll, idxCurrentFit = NULL) {
  if (!is.null(idxCurrentFit)) {
    #colVals <- rep("black", nrow(optimResTmpLstValuesAll))
    # colVals[idxCurrentFit] <- "red"
    df <- optimResTmpLstValuesAll
    df$col <- "#000000" # "black"
      df[df$idx == idxCurrentFit, ]$col <- "#FF0000" #  "red"
  } else {
    df <- data.frame(value = sort(optimResTmpLstValuesAll))
    df$idx <- as.numeric(row.names(df))
    df[df == 10^10] <- NA
    df$col <- "#000000" # "black"
  }

  gg <- ggplot(data = df, aes(x = idx, y = value, color = col)) +
    geom_point() +
    # geom_line() +
    labs(x='Index') +
    scale_colour_manual(values=c("#000000",  "#FF0000")) +
    theme_bw() +
    theme(legend.position="none")

  gg
}

plotParameterDistribution <- function(optimResTmpLstParsAll.df.long) {
  ggplot(optimResTmpLstParsAll.df.long, aes(x=value)) +
    geom_histogram(alpha=0.6) +
    theme_bw() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    facet_wrap(~variable, scales = "free")
}

# pars is vector containing T_shift, tau_1, tau_2, A_sus, A_trans, p_0
# data is data frame containing columns t, t_prime, y
plotModelComponents <- function(pars, data, signum_TF, title = "") {
  # T_shift <- pars[["T_shift"]]
  # tau_1 <- pars[["tau_1"]]
  # tau_2 <- pars[["tau_2"]]
  # A_sus <- pars[["A_sus"]]
  # A_trans <- pars[["A_trans"]]
  # p_0 <- pars[["p_0"]]

  for (v in 1:length(pars)) assign(names(pars)[v], pars[[v]])
  for (v in 1:ncol(data)) assign(names(data)[v], data[,v])

  gg1 <- plotTvsTprime(t = t, t_prime = t_prime)
  gg2 <- plotLinearTransformation(p_0 = p_0, T_shift = T_shift,
                                  y = y, t_prime = t_prime)
  gg3 <- plotSignalSus(tau_1 = tau_1, A_sus = A_sus, p_0 = p_0,
                       T_shift = T_shift, signum_TF = signum_TF,
                       y = y, t_prime = t_prime)
  gg4 <- plotSignalTrans(tau_1 = tau_1, tau_2 = tau_2,
                         A_trans = A_trans,
                         p_0 = p_0, T_shift = T_shift, signum_TF = signum_TF,
                         y = y, t_prime = t_prime)
  gg5 <- plotFit(tau_1 = tau_1, tau_2 = tau_2, A_sus = A_sus,
                 A_trans = A_trans, p_0 = p_0, T_shift = T_shift,
                 signum_TF = signum_TF,
                 y = y, t_prime = t_prime)

  library(patchwork)
  patchwork <- gg1 + gg2 + gg3 + gg4 + gg5 + plot_layout(ncol = 2)
  patchwork + plot_annotation(
    title = title
  )
}

################################################################################

initializeOptimObject <- function(data, modus, takeLog10=TRUE) {
  for (v in 1:ncol(data)) assign(names(data)[v], data[,v])

  # Define Lower and Upper bounds, and Default initial guess
  lb.vec <- c(tau_1=min(diff(unique(t_prime)))/2, # minimal sampling interval
              tau_2=min(diff(unique(t_prime)))/2, # minimal sampling interval
              A_sus=0,
              A_trans=0,
              p_0=min(y),
              T_shift=-(max(t_prime)-min(t_prime))/5,
              sigma = max(1e-8,diff(range(y))/(10^4)))

  ub.vec <- c(tau_1=2*(max(t_prime)-min(t_prime)),
              tau_2=2*(max(t_prime)-min(t_prime)),
              A_sus=2*(max(y)-min(y)),
              A_trans=2*(max(y)-min(y)),
              p_0=max(y),
              T_shift=(max(t_prime)-min(t_prime))/2,
              sigma = sd(y, na.rm =TRUE))

  initialGuess.vec <- c(tau_1=0.5*lb.vec[["tau_1"]] + 0.5*ub.vec[["tau_1"]],
                        tau_2=0.5*lb.vec[["tau_2"]] + 0.5*ub.vec[["tau_2"]],
                        A_sus=0.1*lb.vec[["A_sus"]] + 0.9*ub.vec[["A_sus"]],
                        A_trans=0.1*lb.vec[["A_trans"]] + 0.9*ub.vec[["A_trans"]],
                        p_0=0.5*lb.vec[["p_0"]] + 0.5*ub.vec[["p_0"]],
                        T_shift=-(max(t_prime)-min(t_prime))/10,
                        sigma = max(lb.vec["sigma"], 0.1*diff(range(y))))

  if ("sdExp" %in% colnames(data)){
    lb.vec <- lb.vec[names(lb.vec) != "sigma"]
    ub.vec <- ub.vec[names(ub.vec) != "sigma"]
    initialGuess.vec <- initialGuess.vec[names(initialGuess.vec) != "sigma"]
  }

  optimObject.orig <- list(data = data,
                           initialGuess.vec = initialGuess.vec,
                           lb.vec = lb.vec,
                           ub.vec = ub.vec,
                           fixed = c(signum_TF = NA,
                                     tau_1 = NA,
                                     tau_2 = NA,
                                     A_sus = NA,
                                     A_trans = NA,
                                     p_0 = NA,
                                     T_shift = NA,
                                     sigma = NA),
                           takeLog10 = takeLog10,
                           positive.par.names = NULL,
                           modus = modus,
                           fitted = list()
  )

  if (takeLog10){
    positive.par.names <- getPositiveParNames(
      lb.vec = optimObject.orig$lb.vec,
      ub.vec = optimObject.orig$ub.vec,
      initialGuess.vec = optimObject.orig$initialGuess.vec)
    positive.par.names <- setdiff(positive.par.names, "sigma")
    optimObject.orig$positive.par.names <- positive.par.names
    optimObject.orig <- takeLog10OfBounds(optimObject.orig)
  }
  optimObject.orig
}

getTransientFunctionResult <- function(par, data, fixed, t_prime=NULL,
                                       modus = "RetardedTransientDynamics") {

  for (v in 1:length(par)) assign(names(par)[v], par[[v]])

  # Fixed parameters will overwrite the values in par
  for (v in 1:length(fixed)) {
    if (!is.na(fixed[[v]])) assign(names(fixed)[v], fixed[[v]])
  }

  if (is.null(t_prime)) t_prime <- data$t_prime
  nonLinTransformation <- log10(10^t_prime+10^T_shift)-log10(1+10^T_shift)

  if (modus == "ImmediateResponseFunction"){
    Signal_sus <- A_sus * (1-exp(- t_prime/tau_1)) # Warum tau_2? Taucht in Paperformel nicht auf
    Signal_trans <- A_trans * (1-exp(- t_prime/tau_1))*exp(- t_prime/tau_2)
    transientFunctionRes <-  signum_TF * Signal_sus + signum_TF * Signal_trans + p_0
  } else if (modus == "RetardedTransientDynamics"){
    Signal_sus <- A_sus * (1-exp(- nonLinTransformation /tau_1))  # Warum tau_2? Taucht in Paperformel nicht auf
    Signal_trans <- A_trans * (1-exp(- nonLinTransformation /tau_1))* exp(- nonLinTransformation /tau_2)
    transientFunctionRes <- signum_TF * Signal_sus + signum_TF * Signal_trans + p_0
  }
  transientFunctionRes
}

# Chi square
objFunct <- function(par, data, optimObject) {
  # par <- optimObject$initialGuess.vec

  if (optimObject$takeLog10) par[optimObject$positive.par.names] <- 10^par[optimObject$positive.par.names]

  res <- data$y - getTransientFunctionResult(par = par[names(par) != "sigma"],
                                             data = data,
                                             fixed = optimObject$fixed)

  if (("sdExp" %in% colnames(data))){

    sdVec <- data$sdExp
    loglik <- sum(log(dnorm(res, 0, sdVec)))
  } else {
    loglik <- sum(log(dnorm(res, 0, par["sigma"])))
  }

  retval <- -2 * loglik

  # if(is.infinite(retval)){
  if (retval == Inf){
    print(par)
    retval <- 10^10
    warning(paste0("objective function is infinite."))
  } else if (retval == -Inf){
    retval <- -10^10
  }
  return(retval)
}


runOptimization <- function(initialGuess.vec.lst, optimObject, objFunct) {

  currentBestRes <- NULL
  currentBestResValue <- NULL
  res.lst <- list()
  optimObject.tmp <- optimObject

  pars.tmp <- c()
  # Remove each fixedParam from vec, optimObject$lb.vec, and optimObject$ub.vec
  for (el in c("tau_1", "tau_2", "A_sus", "A_trans", "p_0", "T_shift", "sigma")){
    if (!is.na(optimObject.tmp$fixed[[el]])){
      nam <- names(pars.tmp)
      pars.tmp <- c(pars.tmp, optimObject.tmp$fixed[[el]])
      names(pars.tmp) <- c(nam, el)
      optimObject.tmp$lb.vec <- optimObject.tmp$lb.vec[-which(names(optimObject.tmp$lb.vec) == el)]
      optimObject.tmp$ub.vec <- optimObject.tmp$ub.vec[-which(names(optimObject.tmp$ub.vec) == el)]

      # remove from each sublist in initialGuess.vec.lst
      initialGuess.vec.lst <- lapply(initialGuess.vec.lst, function(x) x[-which(names(x) == el)])
    }
  }

  for (vec in initialGuess.vec.lst){
    print(vec)

    optimResTmp <- stats::optim(par = vec,
                                fn = objFunct,
                                method = "L-BFGS-B",
                                lower = optimObject.tmp$lb.vec,
                                upper = optimObject.tmp$ub.vec,
                                data = optimObject.tmp$data,
                                optimObject = optimObject.tmp,
                                #positive.par.names = optimObject$positive.par.names,
                                control = list(trace = 2, maxit = 1000, factr=1.0e-20))
    if (optimObject$takeLog10) {
      optimResTmp$par[optimObject$positive.par.names] <- 10^optimResTmp$par[optimObject$positive.par.names]
    }

    optimResTmp$par <- pars <- c(pars.tmp, optimResTmp$par)
    value <- optimResTmp$value

    title <- paste0("OptimValue: ", round(value, 2),
                    "; signum_TF: ", optimObject$fixed[["signum_TF"]], ", ",
                    paste(names(pars), round(pars, 4), sep = ": ", collapse = ", "))

    gg <- plotModelComponents(pars = pars,
                              data = optimObject$data,
                              signum_TF = optimObject$fixed[["signum_TF"]], title = title)

    lst <- list(append(list(optimResTmp), list(gg)))
    names(lst[[1]]) <- c("optimRes", "gg")
    res.lst <- append(res.lst, lst)

    if (is.null(currentBestResValue)){
      currentBestResValue <- value
      optimRes <- optimResTmp
    }

    if (value < currentBestResValue) {
      currentBestResValue <- value
      optimRes <- optimResTmp
    }
  }

  res.lst <- sortListByValue(res.lst)

  list(res.lst = res.lst, bestOptimRes = optimRes)
}

getInitialGuessResults <- function(optimObject, objFunct, nInitialGuesses, plot = TRUE) {
  initialGuess.vec.lst <- getInitialGuessVec(
    initialGuess.vec = optimObject$initialGuess.vec,
    lb.vec = optimObject$lb.vec,
    ub.vec = optimObject$ub.vec,
    nInitialGuesses = nInitialGuesses)

  initialGuessResults <- runOptimization(initialGuess.vec.lst, optimObject, objFunct)

  res.lst <- initialGuessResults[["res.lst"]]
  bestOptimRes <- initialGuessResults[["bestOptimRes"]]

  gg.final <- gg.waterfall <- gg.paramDistr <- NA

  if (plot){
    res.lst.optimRes <- lapply(res.lst, function(x) {
      tmp <- unlist(x[grep("optimRes", names(x))], recursive = FALSE)
      names(tmp) <- sub("optimRes.", "", names(tmp))
      tmp
    })

    optimResTmpLstParsAll <- lapply(res.lst.optimRes, function(x) {
      tmp <- unlist(x[grep("par",names(x))])
      names(tmp) <- sub("par.", "", names(tmp))
      tmp
    })

    optimResTmpLstValuesAll <- unlist(
      lapply(res.lst.optimRes, function(x) unlist(x[grep("value",names(x))])))

    gg.waterfall <- plotWaterfallPlot(optimResTmpLstValuesAll)

    optimResTmpLstParsAll.df <- data.frame(do.call(rbind, optimResTmpLstParsAll))
    optimResTmpLstParsAll.df.long <- reshape2::melt(optimResTmpLstParsAll.df)
    gg.paramDistr <- plotParameterDistribution(optimResTmpLstParsAll.df.long)

    title <- paste0("OptimValue: ", round(bestOptimRes$value, 2),
                    "; signum_TF: ", optimObject$fixed[["signum_TF"]], ", ",
                    paste(names(bestOptimRes$par),
                          round(bestOptimRes$par, 4), sep = ": ", collapse = ", "))
    gg.final <- plotModelComponents(
      pars = bestOptimRes$par,
      data = optimObject$data,
      signum_TF = optimObject$fixed[["signum_TF"]], title = title)
  }

  final <- list(optimRes = bestOptimRes,
                gg = gg.final,
                gg.waterfall = gg.waterfall,
                gg.paramDistr = gg.paramDistr)

  res.lst.wFinal <- list(res.lst = res.lst, final = final)
  res.lst.wFinal
}


fittingHelper <- function(optimObject, plot = TRUE, titlePrefix = "") {
  nInitialGuesses <- 50
  res.lst.wFinal <- getInitialGuessResults(optimObject, objFunct, nInitialGuesses, plot = plot)
  final <- res.lst.wFinal$final

  bestFit.plot <- NA
  if (plot) {
    library(patchwork)

    waterfallPlotData <- final$gg.waterfall$data

    res.lst <- res.lst.wFinal$res.lst
    res.lst.gg <- lapply(res.lst, function(x) x[["gg"]])

    pdf(file = paste0(titlePrefix, "allFits.pdf"), width = 12, height = 10)
    for (i in seq(length(res.lst.gg))) {
      #print(i)
      gg <- res.lst.gg[[i]]
      library(patchwork)
      gg.wWaterfall <-  gg + plotWaterfallPlot(waterfallPlotData, i)
      print(gg.wWaterfall)
      #gg
    }
    dev.off()

    bestFit.plot <- final$gg + final$gg.waterfall +
      final$gg.paramDistr + plot_layout(ncol = 2)
    ggsave(filename = paste0(titlePrefix, "bestFit.pdf"),
           bestFit.plot, width = 12, height = 13)
  }

  list(res.pars = final$optimRes$par,
       value = final$optimRes$value,
       bestFit.plot = bestFit.plot)
}

getFittingResults_fixedSignumTF <- function(
    optimObject, signum_TF, plot, titlePrefix = "") {
  optimObject$fixed[["signum_TF"]] <- signum_TF
  optim.res <- fittingHelper(optimObject, plot = plot, titlePrefix = titlePrefix)
  res.pars <- optim.res$res.pars
  value <- optim.res$value
  optimObject$fitted <- c(res.pars, signum_TF = signum_TF)
  optimObject$value <- value
  optimObject$bestFit.plot <- optim.res$bestFit.plot
  optimObject
}

getFittingResult <- function(optimObject, plot = TRUE, titlePrefixPrefix = "") {
  for(pname in names(optimObject$initialGuess.vec)){
    # for(pname in names(optimObject$fixed)){ # was replaced because signum_TF
    # is fixed but is ot listed in lb.vec, ub.vec, and initialGuess.vec
    if (!is.na(optimObject$fixed[pname]))
      optimObject$initialGuess.vec[pname] <- optimObject$lb.vec[pname] <- optimObject$ub.vec[pname] <- optimObject$fixed[pname]
  }
  optimObject.orig <- optimObject
  optim.res.plus1 <- getFittingResults_fixedSignumTF(
    optimObject, signum_TF = 1, plot = plot,
    titlePrefix = paste0(titlePrefixPrefix, "signum_TFPlus1_"))
  optim.res.minus1 <- getFittingResults_fixedSignumTF(
    optimObject.orig, signum_TF = -1, plot = plot,
    titlePrefix = paste0(titlePrefixPrefix, "signum_TFMinus1_"))


  list(plus1 = optim.res.plus1, minus1 = optim.res.minus1)
  # if (optim.res.plus1$value < optim.res.minus1$value){
  #   res <- optim.res.plus1
  # } else {
  #   res <- optim.res.minus1
  # }
  # res
}

selectPlusOrMinus <- function(res) {
  if (res$plus1$value < res$minus1$value){
    res <- res[["plus1"]]
  } else {
    res <- res[["minus1"]]
  }
  res
}

selectSmallerModelIfDiffIsSmall <- function(res, res.smallerModel) {
  # allg: chi2cdf(m2LLworseSmaller-m2LLdetterLarger, df=WievieleGefixt_NpLarge-NpSmall, x=0.95)
  difference <-  res.smallerModel$value - res$value
  df <- sum(is.na(res[["fixed"]])) - sum(is.na(res.smallerModel[["fixed"]])) # number of fitted in large model - number of fitted in small model
  if (pchisq(difference, df = df, lower.tail = FALSE) >= 0.05) res <- res.smallerModel
  res
}

runRTF <- function(data, modus = "RetardedTransientDynamics", plot = TRUE) {

  data <- scaleTimeCol(data=data)
  optimObject.orig <- initializeOptimObject(data, modus = modus)
  res.all.plusMinus <- getFittingResult(optimObject.orig, plot = plot, titlePrefixPrefix = "fullModel_")
  res <- selectPlusOrMinus(res.all.plusMinus)

  #  MODEL REDUCTION
  # 1. Testing whether there is time retardation,
  # i.e., if T_shift parameter is significantly different from the lower bound.
  #  If not significant, T_shift is set to the lower bound which is
  # T_shift = −2 by default.
  optimObjectTmp <- optimObject.orig
  optimObjectTmp$positive.par.names <- setdiff(optimObjectTmp$positive.par.names, "T_shift")  # because lb.vec[["T_shift"]] corresponds to -2
  optimObjectTmp$fixed[["T_shift"]] <- optimObject.orig$lb.vec[["T_shift"]]
  res.T_shiftLB.plusMinus <- getFittingResult(optimObjectTmp, plot = plot, titlePrefixPrefix = "TshiftFixed_")
  res.T_shiftLB <- selectPlusOrMinus(res.T_shiftLB.plusMinus)
  res <- selectSmallerModelIfDiffIsSmall(res, res.T_shiftLB)

  # 2. Testing whether the model is in agreement with a constant.
  # If not significant, we set A_sus = A_trans = 0.
  optimObjectTmp2 <- res
  optimObjectTmp2$positive.par.names <- setdiff(optimObjectTmp2$positive.par.names, c("A_sus", "A_trans"))  # because lb.vec[["T_shift"]] corresponds to -2
  optimObjectTmp2$fixed[["A_sus"]] <- optimObjectTmp2$fixed[["A_trans"]] <- 0
  res.constant.plusMinus <- getFittingResult(optimObjectTmp2, plot = plot, titlePrefixPrefix = "Constant_")
  res.constant <- selectPlusOrMinus(res.constant.plusMinus)
  res <- selectSmallerModelIfDiffIsSmall(res, res.constant)

  # 3. Testing whether the offset p0 is significantly different from zero.
  # If not significant, we set p_0 = 0.
  optimObjectTmp3 <- res
  optimObjectTmp3$positive.par.names <- setdiff(optimObjectTmp3$positive.par.names, "p_0")  # because lb.vec[["T_shift"]] corresponds to -2
  optimObjectTmp3$A_sus <- optimObjectTmp3$p_0  <- 0
  res.p_0Zero.plusMinus <- getFittingResult(optimObjectTmp3, plot = plot, titlePrefixPrefix = "p0Zero_")
  res.p_0Zero <- selectPlusOrMinus(res.p_0Zero.plusMinus)
  res <- selectSmallerModelIfDiffIsSmall(res, res.p_0Zero)

  finalModel <- res
  finalParams <- res$fitted
  finalPlot <- res$bestFit.plot

  if (plot) {
    ggsave(filename = "finalModel.pdf", finalPlot, width = 12, height = 13)
  }

  print("The parameters of the best fit are:")
  print(paste(names(finalParams), round(finalParams, 4), sep = ": ", collapse = ", "))

  return(list(finalModel = res,
              finalParams = res$fitted,
              finalPlot = finalPlot,
              intermediateResults = list(fullModel = res.all.plusMinus,
                                         TshiftFixed = res.T_shiftLB.plusMinus,
                                         Constant = res.constant.plusMinus,
                                         p0Zero = res.p_0Zero.plusMinus)))

}

