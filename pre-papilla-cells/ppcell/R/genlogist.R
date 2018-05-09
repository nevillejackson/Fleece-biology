genlogist <-
function(lowasymp,upasymp,rate,nu,yinflect,c,t)
# genlogist()  -  generalised logistic function
#                 or Richard's curve, used for growth modelling
#         lowasymp - lower asymptote
#         upasymp  - upper asymptote
#         rate     - growth rate
#         nu       - >0  affects near which asymptote max growth rate occurs
#         yinflect - related to Y(0) 
#         c        - typically = 1
#         t        - vector of time values at which to evaluate function
{
  y <- rep(0,length(t))
  for(i in 1:length(t)){
    y[i] <- lowasymp + (upasymp - lowasymp)/
          ((c + yinflect * exp(-(rate * t[i])))^(1/nu))
  }
  return(y)
}
