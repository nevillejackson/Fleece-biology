unfanlt180 <-
function(a,lambda){
#  unfanlt180()  - unfolding angle in radians and degrees for theta < 180
  theta <- c(0,0)
  theta[1] <- 2 * asin(lambda/(4 * a))
  theta[2] <- theta[1] * 360 / (2 * pi)
  return(theta)
}
