labplot2 <-
function(x,y,labels,main="",xlab="",ylab="")
#  labplot2()   -   scatterplot with points a text label
{
  plot(x,y,type="n",main=main,xlab=xlab,ylab=ylab)
  text(x,y,label=as.integer(labels),col=c(1,2,3))
# legend(min(x),max(y),legend=c("30mm","50mm","70mm"),pch="123")
  r <- cor(x,y)
  rtext <- paste("R = ",signif(r,3))
  text(mean(x),max(y)-.05,rtext,cex=1.1) 
}
