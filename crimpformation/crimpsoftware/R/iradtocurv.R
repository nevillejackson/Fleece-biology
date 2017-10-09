iradtocurv <-
function(irad)
# iradtocurv() - intrinsic radius of fibre curvature (irad) to curvature
#                curvature in radians per mm and degrees per mm
{
  curv <- rep(0,2)
  curv[1] <- 1/irad
  curv[2] <- curv[1] * 180 / pi
  return(curv)
}
