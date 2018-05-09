adultdensity <-
function(adultsurfarea,follno,pfollno,sofollno,sdfollno)
# adultdensity()  -  convert follicle nos to densities given surfarea
{
  last <- length(follno)
  samm2 <- adultsurfarea * 100  # sa in mm^2 instead of cm^2
  dens <- follno[last]/samm2
  pdens <- pfollno[last]/samm2
  sodens <- sofollno[last]/samm2
  sddens <- sdfollno[last]/samm2
  sop <- (sofollno[last] + sdfollno[last])/pfollno[last]
  outlist <- list(adultdens=dens,adultpdens=pdens,adultsodens=sodens,adultsddens=sddens,adultsopratio=sop)
  return(outlist)
}
