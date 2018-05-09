make.levels <-
function(base.param,nlevels)
# make.levels()  -  make 'nlevels' values for each parameter
{
  p <- matrix(0,nlevels,7)
  tmp <- base.param$psiteno
  p[,1] <- c(tmp-40000, tmp-20000, tmp, tmp+20000, tmp+40000)
  tmp <- base.param$sopratio
  p[,2] <- c(tmp-.8, tmp-.4, tmp, tmp+.4, tmp+.8)
  tmp <- base.param$follinitrate
  p[,3] <- c(tmp-100, tmp-50, tmp, tmp+50, tmp+100)
  tmp <- base.param$pstarttime
  p[,4] <- c(tmp-3, tmp-2, tmp, tmp+2, tmp+3)
  tmp <- base.param$sostarttime
  p[,5] <- c(tmp-4, tmp-2, tmp, tmp+2, tmp+4)
  tmp <- base.param$zcellno
  p[,6] <- c(tmp-2e+07, tmp-1e+07, tmp, tmp+1e+07, tmp+2e+07)
  tmp <- base.param$cellbirthprob
  p[,7] <- c(tmp-.008, tmp-.004, tmp, tmp+.004, tmp+.008)
 
  return(p)
}
