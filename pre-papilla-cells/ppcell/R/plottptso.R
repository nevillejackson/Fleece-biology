plottptso <-
function(param,which = c("pstarttime","sostarttime"),values = seq(63,65,0.1),other="cellbirthprob",ovalues=c(.070,.073,.076,.0764),out="dens",myxlab=which)
# plottptso()  -  plot dens or sop for Tp and Tso varying
#                   -  other specifies parameter for multiple lines on one graph
#                   -  ovalues specifies values for multiple lines
{
  maxparam <- param
  maxparam[other] <- max(ovalues)
  maxtmp <- varytwopar(maxparam,which,values)
  minparam <- param
  minparam[other] <- min(ovalues)
  mintmp <- varytwopar(minparam,which,values)
  rangetmp <- maxtmp
  rangetmp$dens[1] <- min(mintmp$dens)
  rangetmp$sop[1] <- min(mintmp$sop)

  for(i in 1:length(ovalues)){
    tmpparam <- param
    tmpparam[other] <- ovalues[i]
    tmp <- varytwopar(tmpparam,which,values)
    if(out == "dens"){
      if(i == 1) {
        plot(maxtmp$values,rangetmp$dens,xlab=myxlab,ylab="Adult density ( no per mm^2)",type="n")
      }
      points(tmp$values,tmp$dens,pch=i,col=i+1)
    }
    else {
      if (i == 1) {
        plot(maxtmp$values,rangetmp$sop,xlab=myxlab,ylab="Adult S/P ratio",type="n")
      }
      points(tmp$values,tmp$sop,pch=i,col=i+1)
    }
  }
}
