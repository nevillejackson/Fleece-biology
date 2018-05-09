varyonepar <-
function(param,which="cellbirthprob",values=seq(.0740,.0769,.0001))
#varyonepar()  -  vary one parameter and hold other 19 at base value
{
  savedens <- rep(0,length(values))
  savesop <- rep(0,length(values))
  for(i in 1:length(values)){
    tmp.param <- param
    tmp.param[which] <- values[i]
    tmp.calc <- folli(tmp.param)
    savedens[i] <- tmp.calc$adultdens
    savesop[i] <- tmp.calc$adultsopratio
  }
  outlist <- list(which=which,values=values,dens=savedens,sop=savesop)
  return(outlist)
}
