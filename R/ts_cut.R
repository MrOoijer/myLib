#' @title ts_cut
#' @description cut first time series to the size of the second
#' @param ser1 is the series to be cut
#' @param ser2 is the series from which the boundaries for the cut are used
#' @return the truncated time series
#' @examples
#' # co2<- ts_cut(co2.ts, rss.ts)

ts_cut<-function(ser1, ser2){
  return(window(ser1, start=start(ser2), end=end(ser2)))
}
