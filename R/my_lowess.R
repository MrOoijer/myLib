
#' @title my_lowess
#' @description high frequency lowess filtering
#' @param timeS a time series
#' @param span the number of periods to span
#' @return the filtered time series
#' @examples
#' # hc4<- my_lowess(hc4.ts, span= 13)


my_lowess <- function(timeS, span=5){
  f<- span/length(timeS)
  m1 <- as.numeric(timeS)-lowess(index(timeS), timeS, f)$y
  return(timeS-m1)
}
