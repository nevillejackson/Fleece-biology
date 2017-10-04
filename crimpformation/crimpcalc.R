function(lambda,ampl){
# unfold() - radius of curvature, angle between unfoldings and ratio
#                 from wavelength and amplitude
#                 for unfolded helix
  a <- iradius(lambda,ampl)
  if(a[1] > 0) posa <- 1
  else if(a[2] > 0) posa <- 2
  else stop(" No radius positive in radunfang()(:\n")
  if(a[posa] == ampl){
    theta <- c(0,0)
    theta[2] <- 180
    theta[1] <- pi
    ratio <- theta[1]/(2 * sin((2*pi - theta[1])/2))
  }
  else if( ampl < a[posa]) {
    theta <- unfanlt180(a[posa],lambda)
    ratio <- theta[1]/(2 * sin(theta[1]/2))
  }
  else if( ampl > a[posa]) {
    theta <- unfangt180(a[posa],lambda)
    ratio <- theta[1]/(2 * sin((2*pi - theta[1])/2))
  }
  retobj <- list(radius=a[posa],unfang=theta,ratio=ratio)
  return(retobj)
}
function(lambda,ampl)
# stretch() - intrinsic radius of curvature and ratio 
#             from wavelength and amplitude
#             for a stretched helix
{
  as <- ampl
  bs <- lambda/(2 * pi)
  ac <- sqrt(as^2 + bs^2)
  ratio <- ac/bs
  retobj <- list(radius=ac, ratio=ratio)
  return(retobj)
}
function(x){
#  degtorad()  -  degrees to radians
  xrad <- 2 * pi * x /360
  return(xrad)
}
function(a,lambda){
#  unfangt180()  - unfolding angle in radians and degrees for theta>180
  theta <- c(0,0)
  theta[1] <- 2*(pi - asin(lambda/(4 * a)))
  theta[2] <- theta[1] * 360 / (2 * pi)
  return(theta)
}
function(a,lambda){
#  unfanlt180()  - unfolding angle in radians and degrees for theta < 180
  theta <- c(0,0)
  theta[1] <- 2 * asin(lambda/(4 * a))
  theta[2] <- theta[1] * 360 / (2 * pi)
  return(theta)
}
function(lambda,h){
# iradius() - intrinsic radius from wavelength and amplitude
  disc <- 4 * h^2 + 60*(lambda^2 + h^2)
  a <- c(0,0)
  a[1] <- (-2*h + sqrt(disc))/30
  a[2] <- (-2*h - sqrt(disc))/30
  return(a)
}
 [1] 100 120 140 160 180 200 220 240 260 280 300
