#' @title fake_likert
#' @description Create a sample for a likert scale score with given mean and st.dev.
#'     This uses a primitive type of simulated annealing for the search. This was written
#'     to test the existence of fraudulent numbers in a manuscript by Diederik Stapel.
#' @param N the number of observations
#' @param base the lowest value of the score (usually 0 or 1)
#' @param M the highest score
#' @param d the number of scores from base to M that are averaged (usually 1)
#' @param m the mean to approach
#' @param s the standard deviation to approach
#' @param eps the tolerance for |m-s|
#' @param rep.count the maximum number of try's
#' @param T the temperature (set to higher values below sqrt(rep.count) to find rare solutions)
#' @param test set to TRUE to print some test information from the fucntion
#' @return a vector with N integers on the likert scale
#' @examples
#' # fake_likert(N=16, M=7, base=1, m=4.6, s=3.6) # Cannot exist. Should fail

fake_likert<-function(N=16, M=5, m=3, d=1, s=2, rep.count=250000, eps=0.005, base=0, T=4, test=FALSE){
  
  objective <- function(result, m, s){
    p1<-abs(sd(result)-s)
    p2<-abs(mean(result)-m)
    return(max(p1,p2))
  }
  result= sapply(1:N, function(i) mean(sample(base:M, d, replace=TRUE)))
  cur_obj<-objective(result, m, s)
  step=1/d
  for (i in 1:rep.count){
    a=sample(1:N, 1)
    if (result[a] > base){
      r=runif(1,0,1)
      r = r < exp(-sqrt(i)/(N*T))
      result[a]=result[a]-step
      t_obj = objective(result, m, s)
      if( cur_obj > t_obj | r) cur_obj=t_obj else result[a]=result[a]+step 
    }
    if (result[a]<M){
      r=runif(1,0,1)
      r = r < exp(-sqrt(i)/(N*T))
      result[a]=result[a]+step
      t_obj = objective(result, m, s)
      if( cur_obj > t_obj | r) cur_obj=t_obj else result[a]=result[a]-step 
    }
    a=sample(1:N, 2)
    b=a[2]; a=a[1]
    if (result[a]<M & result[b]>base){
      result[a]=result[a]+step
      result[b]=result[b]-step
      t_obj = objective(result, m, s)
      if( cur_obj > t_obj) cur_obj=t_obj else {
        result[a]=result[a]-step
        result[b]=result[b]+step
      } 
      
    }
    if(cur_obj < eps) break  
  }
  if (test) {
    print(i)
    print(cur_obj)
  }
  return(result)
}

# # test - 1a
# 
# N=16; M=7; base=1
# for (i in 1:16){
#   b= sample(1:M, N, replace=TRUE)
#   m=mean(b); s=sd(b)
#   print(c(m,s))
#   a<-fake_likert(N=N, M=M, base=base, m=m, s=s, test=TRUE)
#   if (abs(mean(a)* sd(a) -m*s) >0.001) print(b) else print("Okay")
# }
# # test - 1b
# N=100; M=5; base=0
# for (i in 1:16){
#   b= sample(1:M, N, replace=TRUE)
#   m=mean(b); s=sd(b)
#   print(c(m,s))
#   a<-fake_likert(N=N, M=M, base=base, m=m, s=s, test=TRUE)
#   if (abs(mean(a)* sd(a) -m*s) >0.001) print(b) else print("Okay")
# }
# 
# 
# # test -2 should fail
# 
# m=4.6; s=3.3; N=16
# a<-fake_likert(N=N, M=M, base=base, m=m, s=s, T=100, eps=0.02)
# cat(paste(c(N, M, base, m, s, "->", a, mean(a), sd(a)), sep=" ")); cat("\n")
# if(round(mean(a), 1) != m) print("m fails")
# if(round(sd(a), 1) != s) print("s fails")

#### test for forster

# for (N in 12:15){
#   print(a<-fake_likert(M=9, N=N, d=2, m=7.32, s=1.30, eps=0.005, base=1, test=TRUE))
#   print(mean(a))
#   print(sd(a))
# }