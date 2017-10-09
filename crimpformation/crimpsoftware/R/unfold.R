unfold <-
function(lambda,ampl,twistlen,twistrad,stalen){
# unfold() - radius of curvature, angle between unfoldings,
#	     bundle length, fibre length, and Lf/Ls ratio
#                 from wavelength (lambda) 
#		       amplitude (ampl)
#		       twistlen (length of twisted region)
#		       twistrad (radius of curvature of twist)
#		       stalen (staple length)
#                 for unfolded helix
  a <- iradius(lambda,ampl)
  if(a <= 0) {
    stop(" Radius not positive in unfold()(:\n")
  }
  if(a == ampl){
    theta <- c(0,0)
    theta[2] <- 180
    theta[1] <- pi
  }
  else if( ampl < a) {
    theta <- unfanlt180(a,lambda)
  }
  else if( ampl > a) {
    theta <- unfangt180(a,lambda)
  }
    t <- stalen/lambda
    lb <- 2 * a * t * theta[1]
    lbtols <- lb/stalen
    lf <- (2 * a * theta[1] - 2 * twistlen +
                          2 * sqrt((twistlen)^2 + (pi * twistrad)^2)) * t
    lftols <- lf/stalen

  retobj <- list(radius=a,unfang=theta,t=t,lb=lb,lbtols=lbtols,lf=lf,lftols=lftols)
  return(retobj)
}
