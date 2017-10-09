stretch <-
function(lambda,ampl,stalen)
# stretch() - intrinsic radius of curvature, fibre length, and Lf/Ls ratio 
#             from wavelength (lambda) 
#		   amplitude (ampl)
#                  stalen (staple length)
#             for a stretched helix
{
  as <- ampl
  bs <- lambda/(2 * pi)
  ac <- sqrt(as^2 + bs^2)
  t <- stalen/lambda
  lf <- 2 * pi * t * sqrt(as^2 + bs^2)
  lftols <- ac/bs
  retobj <- list(radius=ac,t=t,lf=lf,lftols=lftols)
  return(retobj)
}
