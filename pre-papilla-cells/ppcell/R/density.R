density <-
function(surfarea,time,follno,pfollno,sofollno,sdfollno)
# density()  -  convert follicle nos to densities given surfarea
{
  samm2 <- surfarea * 100  # sa in mm^2 instead of cm^2
  dens <- follno/samm2
  pdens <- pfollno/samm2
  sodens <- sofollno/samm2
  sddens <- sdfollno/samm2
  sop <- rep(NA,length(follno))
  for (i in 1:length(pfollno)) {
    if(pfollno[i] > 0) {
      sop[i] <- (sofollno[i] + sdfollno[i])/pfollno[i]
    }
    else {
      sop[i] <- NA
    }
  }
  outlist <- list(dens=dens,pdens=pdens,sodens=sodens,sddens=sddens,sopratio=sop)
  return(outlist)
}
