fregion <-
function(base.param)
# fregion()  -  find the parameter combinations which lead to results in
#               the feasable region for density and S/P
#               at given fibre diameters (Dp,DSo, DSd) 
{
  nlevels <- 5
  p <- make.levels(base.param,nlevels)

  l <- 1:nlevels
  levlist <- list(l,l,l,l,l,l,l)
  levarray <- expand.grid(levlist)

  psetarray <- matrix(0,nrow(levarray),7)
  adens <- rep(0,nrow(levarray))
  for(iset in 1:nrow(levarray)){
    pset <- rep(0,7)
    for(i in 1:7) {
      pset[i] <- p[levarray[iset,i],i]
    }
    psetarray[iset,] <- pset
    paramset <- as.plist(pset,base.param) # turn  vector into a list for folli
    adens[iset] <- folli(paramset)$adultdens
  }
  outlist <- list(p=p, apset=psetarray, adens=adens)
  return(outlist)
}
