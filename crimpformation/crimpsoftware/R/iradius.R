iradius <-
function(lambda,h){
# iradius() - intrinsic radius of fibre curvature
#             from wavelength (lambda) and amplitude (h)
#             for unfolded helix crimp only
  a <- (lambda^2 + 16*h^2)/(32 * h)
  return(a)
}
