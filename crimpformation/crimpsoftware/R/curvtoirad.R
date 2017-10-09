curvtoirad <-
function(curv)
# curvtoirad() - curvature (curv) in degrees per mm, to intrinsic radius of
#                fibre curvature in mm
{
  curvrad <- curv * pi / 180
  irad <- 1/curvrad
  return(irad)
}
