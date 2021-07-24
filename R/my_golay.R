#' @title my_golay
#' @description high frequency golay filtering
#' @param model a time series
#' @param span the number of periods to span (should be odd)
#' @param p the degree of the Golay polynomial
#' @return the filtered time series
#' @examples
#' # hc4<- my_golay(hc4.ts, 13, p=5 )


my_golay<-function(model, span, p=3){
  # from library signal
  library(signal)
  # model is the time series
  if (span <= 0) return(model)
  span<- -1 + span +(span%%2)
  m1<-as.numeric(model)-sgolayfilt(model, p=p, n = max(span,5))
  return(model-m1)
}
