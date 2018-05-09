plotonepar <-
function(param,which,values,out="dens")
# plotonepar()  -  plot dens or sop for one parameter varying
{
   tmp <- varyonepar(param,which,values)
   if(out == "dens"){
     plot(tmp$values,tmp$dens,xlab=which,ylab="Adult density ( no per mm^2)")
   }
   else {
     plot(tmp$values,tmp$sop,xlab=which,ylab="Adult S/P ratio")
   }
}
