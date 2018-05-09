varytwopar <-
function(param,which=c("pstarttime","sostarttime"),values=seq(63,65,.1))
#varytwopar()  -  vary pstarttime and set sostarttime at + 22
{
  savedens <- rep(0,length(values))
  savesop <- rep(0,length(values))
  for(i in 1:length(values)){
    tmp.param <- param
    tmp.param[which[1]] <- values[i]
    tmp.param[which[2]] <- values[i] + 22
    tmp.calc <- folli(tmp.param)
    savedens[i] <- tmp.calc$adultdens
    savesop[i] <- tmp.calc$adultsopratio
  }
  outlist <- list(which=which,values=values,dens=savedens,sop=savesop)
  return(outlist)
}
