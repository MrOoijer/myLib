#' @title preRS
#' @description calculate predictive R-squared aka leave one out Cross Validation
#' @param this.lm a linear model object
#' @return the preRS statistic - please note this only works for a linear model
#' @examples
#' # preRS(lm)
#' ## 0.78
#' # hc4.ts <- rd.hc4()
#' # this.lm <- lm(hc4.ts~index(hc4.ts))
#' # print(summary(this.lm))
#' # print(preRS(this.lm))

preRS <- function(this_lm){
  if (! "lm" %in% class(this_lm)) {
    # other models: approximate, not exact !
    a <- resid(this_lm)
    b <- fitted(this_lm)
    this_lm <- lm((a + b) ~ b)
  }
  this.anova <- anova(this_lm)
  tss <- sum(this.anova$"Sum Sq")
  pr <- residuals(this_lm)/(1 - lm.influence(this_lm)$hat)
  PRESS <- sum(pr^2)
  return(1-PRESS/tss)
}


# test
# read.hc4()
# this.lm<-lm(hc4.ts~index(hc4.ts))
# print(summary(this.lm))
# print(preRS(this.lm))