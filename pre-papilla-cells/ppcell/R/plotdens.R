plotdens <-
function(x,ymax=300)
# plotdens()  -  plot follicle density against time
{
  par(lwd=2,lty=1)
  ydum <- x$dens
  ydum[length(x$dens)] <- ymax
  plot(x$time,ydum,xlab="Days",ylab="Follicles per mm^2",type="n")
  points(x$time,x$pdens,type="p",col=5)
  points(x$time,x$sodens,type="p",col=2)
  points(x$time,x$sddens,type="p",col=3)
  points(x$time,x$dens,type="l",col=4)
  legend(mean(x$time),max(ydum)*0.9,legend=c("P","So","Sd","All"),col=c(5,2,3,4),lty=1)
}
