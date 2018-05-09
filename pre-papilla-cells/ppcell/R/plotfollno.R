plotfollno <-
function(x,ymax=1.e+08)
# plotfollno()  -  plot follicle nos against time
{
  par(lwd=2,lty=1)
  ydum <- x$follno
  ydum[length(x$follno)] <- ymax
  plot(x$time,ydum,xlab="Days",ylab="Number of follicles",type="n")
  points(x$time,x$pfollno,type="p",col=5)
  points(x$time,x$sofollno,type="p",col=2)
  points(x$time,x$sdfollno,type="p",col=3)
  points(x$time,x$follno,type="l",col=4)
  legend(mean(x$time),max(ydum)*0.9,legend=c("P","So","Sd","All"),col=c(5,2,3,4),lty=1)
}
