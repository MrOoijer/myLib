#' @title binned
#' @description map vector to bins
#' @param nbin the number of bins or an increasing vector giving the bin boundaries 
#' @param type c("height", "width") either the same number of elements in a bin or equal widths for the bins
#' @return the vector mapped to bin numbers 1 to nbin  
#' @examples
#' # a<- binned(x, 10) # 10 equal width bins
#' # a<- binned(x, 0:10) # ten bins for numbers between 0-10
#' #                     # other numbers that do not fit are in the first or last bin
binned <- function(x, nbin=0, type = "width") {
  if(length(nbin) > 1){
    return( findInterval(x, nbin, all.inside = TRUE) )
  }
  
  if(length(unique(x)) < nbin) return(x)
  if(nbin <= 0) stop("Nbin should be > 0")
  type <- match.arg(type, c("height", "width"))
  if (type == "width") {
    bins = seq (min(x), max(x), length.out = nbin + 1)
    x.binned <- findInterval(x, bins, all.inside = TRUE)
  }
  else if (type == "height") {
    bins = quantile(x, seq(0, 1, length.out = nbin + 1))
    x.binned <- findInterval(x, bins, all.inside = TRUE)
  }
  else
    stop("wrong type")
  return(x.binned)
}
