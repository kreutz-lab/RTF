#' @description Plot unscaled time plotted against scaled time.
#' @return ggplot object showing unscaled time plotted against scaled time
#' @param t Vector of unscaled time points
#' @param t_prime Vector of time points scaled such that each time points lies
#' in time interval [0,10]
#' @export plotTvsTprime
#' @examples
#' t <- c(0.4, 4.5, 5.8, 7.9, 10.02, 15.6, 21.0, 44.8)
#' t <- t-min(t)
#' t_range <- max(t)-min(t)
#' t_prime <- 10*t/t_range
#' gg <- plotTvsTprime(t, t_prime)

plotTvsTprime <- function(t, t_prime) {
  ggplot(data, aes(x = t, y = t_prime)) +
    geom_point(size = 1, alpha = 0.5) +
    theme_bw()
}
