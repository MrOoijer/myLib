#' @title pretty_cc
#' @description called from pretty_plot, do not use otherwise

pretty_cc <- function(ccloc){
  loc= 0
  if(ccloc <= 0) return
  if(ccloc > 4) return
  if (ccloc %in% c(1,3)){
    loc= ccloc
    ln = -1.1
    a = 0.02
  } else if (ccloc %in% c(2,4)) {
    loc= ccloc-1
    ln = -0.9
    a = 0.98
  } else {return(0)}
  if(loc ==0) print(ccloc) 
  yr<-substring(format(Sys.time()), 1, 4)
  mtext(paste("CC Jan van Rongen", yr)
        , col=cpalet$as
        , side=loc, line= ln, adj=a, cex=0.55)
}
