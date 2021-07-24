
#' @title my_ksmooth
#' @description high frequency ksmooth filtering
#' @param timeS a time series
#' @param span the number of periods to span (should be odd)
#' @param kernel box or normal
#' @return the filtered time series
#' @examples
#' # hc4<- my_ksmooth(hc4.ts, span= 13)

my_ksmooth <- function(timeS, span=5, kernel="box"){
  m1 <- as.numeric(timeS)-
    ksmooth(index(timeS), as.numeric(timeS), kernel=kernel, bandwidth=span/12)$y
  return(timeS-m1)
}
