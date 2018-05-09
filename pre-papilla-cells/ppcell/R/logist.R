logist <-
function(nu,tau,mu,sigma,rho,x)
# logist()  -  calculat y given x  for logistic model
#           mu - x location
#           nu - y location
#           sigma - x scale
#           tau - y scale
#           rho > 0
#  
{
  y <- nu + tau * ( 1 + exp((x-mu)/sigma)) ^(-rho)
  return(y)
}
