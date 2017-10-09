stretcht <-
function(lambda,ampl,stalen,t)
# stretcht() - intrinsic radius of curvature, fibre length, and Lf/Ls ratio 
#             from wavelength (lambda) 
#		   amplitude (ampl)
#                  stalen (staple length)
#		   t = crimps per staple supplied by call
#             for a stretched helix
{
  as <- ampl
  bs <- lambda/(2 * pi)
  ac <- sqrt(as^2 + bs^2)
  lf <- 2 * pi * t * sqrt(as^2 + bs^2)
  lftols <- lf/stalen
  retobj <- list(radius=ac,t=t,lf=lf,lftols=lftols)
  return(retobj)
}
