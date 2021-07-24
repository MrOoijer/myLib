#' @title pretty_density
#' @description makes a pretty density plot with mean and CI
#' @param data a vector with the data
#' @param mid is NA, the mean of the data, otherwise a given value such as median
#' @param kleur the line color in the palet
#' @param cdf if true, take the cumulative distribution
#' @param ... other values for pretty_plot
#' @return NA
#' @examples
#' # pretty_density(rnorm(1000, 0,1), mid=0)
pretty_density <- function(data
                           , xlab = "value"
                           , ylab = ""
                           , cdf = FALSE 
                           , main = NA
                           , kleur = 1
                           , ccloc = 0
                           , add = FALSE
                           , mid = NA
                           , quants = c(2.5, 97.5)
                           , bw = "nrd0"
                           , ...){
  dens=density(data, bw=bw)
  if (cdf) dens$y <- cumsum(dens$y)/tail(cumsum(dens$y), 1)
  if (is.na(mid)) mid=mean(data)
  if (is.na(main)) main= ifelse(cdf, "Cumulative plot with 95% confidence interval", "Density plot with 95% confidence interval")
  p=round(c(quantile(data, prob=quants/100), mid),2)
  if (!add) pretty_plot(data.frame(dens$x, dens$y), main=main, xlab=xlab, ylab=ylab, kleur=kleur, ccloc=ccloc, xat=p, ...) else
    pretty_plot(kleur=kleur, add=TRUE, data.frame(dens$x, dens$y), ... )
  pretty_abline(kleur=kleur, lty=2, v=p)
}
