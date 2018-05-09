folli <-
function(param)
# folli()  -  model for pre-papilla cell population growth
#             param$surfareaconst = 9 ) gives result in cm^2 for surfarea
#             param$growth$upasymp  = 40000 g ) gives result in cm^2 for surfarea
{
  ztime <- param$ztime
  maxtime <- param$maxtime
  timeincrement <- param$timeincrement
  len <- (maxtime - ztime) %/% timeincrement + 1
  pfollno <- rep(0,len)
  sofollno <- rep(0,len)
  sdfollno <- rep(0,len)
  cellno <- rep(0,len)
  cellno[1] <- param$zcellno
  follno <- rep(0,len)
  diffcellno <- rep(0,len)
  pinterval <- 0
  sointerval <- 0
  sdinterval <- 0
  sdendtime <- 0
  psiteno <- param$psiteno
  sositeno <- psiteno * param$sopratio
  pstarttime <- param$pstarttime
  pendtime <- pstarttime
  sostarttime <- param$sostarttime
  soendtime <- sostarttime
  follinitrate <- param$follinitrate
  pavecellno <- param$pavecellno
  soavecellno <- param$soavecellno
  sdavecellno <- param$sdavecellno
  sdstarttime <- NULL
  sdendtime <- sdstarttime
  avecellwt <-  1.e-10  # cell wt in gm ( 1 x 10^-10)
#  dN/dt = dW/dt / avecellwt
# mitoticrate <- param$growth$rate / avecellwt # no cells per unit time
  cellbirthprob <- param$cellbirthprob
#
  atime <- rep(0,len)
  aweight <- rep(0,len)
  asurfarea <- rep(0,len)
# growth parameters
  A <- param$growth$lowasymp
  K <- param$growth$upasymp
  B <- param$growth$rate
  nu <- param$growth$maxpos
  Q <- param$growth$wtatmax
  C <- param$growth$c
# loop over growth steps
  time <- ztime - timeincrement
  while ( time < maxtime) {
    time <- time + timeincrement
    interval <- time - ztime
    index <- interval + 1
#   cellbirthprob <- mitoticrate / time
#   cellbirthprob <- mitoticrate 
#   growth curve
#   weight <- param$growthint * exp(param$growthrate*log(time)) 
#   weight <- genlogist(0,45000,.03,1.1,1000,1, time)
    weight <- genlogist(A,K,B,nu,Q,C, time)
    surfarea <- param$surfareaconst * exp((2.0/3.0)*log(weight))
    atime[index] <- time
    aweight[index] <- weight
    asurfarea[index] <- surfarea
#   check if all cells differentiated
    if (cellno[index] <= 0) {
#     out of founder cells
      if(index > 1) {
        pfollno[index] <- pfollno[index-1]
        sofollno[index] <- sofollno[index-1]
        sdfollno[index] <- sdfollno[index-1]
        diffcellno[index] <- diffcellno[index-1]
        follno[index] <- follno[index-1]
        cellno[index] <- 0
      }
      else {
        pfollno[1] <- 0
        sofollno[1] <- 0
        sdfollno[1] <- 0
        diffcellno[1] <- 0
        follno[1] <- 0
        cellno[1] <- 0
      }
    }
#   count differentiated ppcell nos
    else {
#     still have some cells to differentiate
      if(time < pstarttime) {
#       before primary period 
        pinterval <- pinterval + timeincrement
        ptime <- time - pstarttime
        pfollno[index] <- 0
        sofollno[index] <- 0
        sdfollno[index] <- 0
        diffcellno[index] <- 0
        follno[index] <- 0
        pendtime <- time

      }
      else if ((time >= pstarttime) && (pfollno[index-1] < psiteno)) {
#       primary period
        pinterval <- pinterval + timeincrement
        ptime <- time - pstarttime
        pfollno[index] <- follinitrate * surfarea * ptime
        sofollno[index] <- 0
        sdfollno[index] <- 0
        diffcellno[index] <- pfollno[index] * pavecellno
        follno[index] <- pfollno[index]
        pendtime <- time
      }
      else if (time < sostarttime) {
#       between primary and secondary periods if no overlap
        pfollno[index] <- pfollno[index - 1]
        sofollno[index] <- 0
        sdfollno[index] <- 0
        diffcellno[index] <- pfollno[index] * pavecellno
        follno[index] <- pfollno[index]
      }
      if ((time >= sostarttime) && (sofollno[index-1] < sositeno)) {
#       So period
        sointerval <- sointerval + timeincrement
        sotime <- time - sostarttime
        pfollno[index] <- pfollno[index-1]
        sofollno[index] <- follinitrate * surfarea * sotime
        sdfollno[index] <- 0
        diffcellno[index] <- pfollno[index] * pavecellno +
                             sofollno[index] * soavecellno
        follno[index] <- pfollno[index] + sofollno[index]
        soendtime <- time
      }
      else if ((time >= sostarttime) && (sofollno[index-1] >= sositeno)){
#       Sd period
        sdinterval <- sdinterval + timeincrement
        sdstarttime <- sostarttime + sointerval
        sdtime <- time - sdstarttime
        pfollno[index] <- pfollno[index-1]
        sofollno[index] <- sofollno[index-1]
        sdfollno[index] <- follinitrate * surfarea * sdtime
        diffcellno[index] <- pfollno[index] * pavecellno +
                             sofollno[index] * soavecellno +
                             sdfollno[index] * sdavecellno
        follno[index] <- pfollno[index] + sofollno[index] + sdfollno[index]
        sdendtime <- time
      }
#     calculate undifferentiated cell nos for start of next time interval
        if (index == 1) {
          cellno[index+1] <- cellno[index]  +
                       cellbirthprob * cellno[index] * timeincrement -
                       (diffcellno[index] - 0)
        }
        else {
          cellno[index+1] <- cellno[index]  +
                       cellbirthprob * cellno[index] * timeincrement -
                       (diffcellno[index] - diffcellno[index-1])
        }
    } # end else - not out of founder cells
  } # end if (time < maxtime)
#
# convert follicle numbers to densities
  denslist <- density(asurfarea,atime,follno,pfollno,sofollno,sdfollno)

# calculate adult density
  adultsurfarea <- param$surfareaconst * exp((2.0/3.0) * log(param$growth$upasymp))
  adultdenslist <- adultdensity(adultsurfarea,follno,pfollno,sofollno,sdfollno)

  outlist <- list(time=atime,weight=aweight,surfarea=asurfarea,
                  pfollno=pfollno,sofollno=sofollno,sdfollno=sdfollno,
                  diffcellno=diffcellno,follno=follno,cellno=cellno,
                  dens=denslist$dens,pdens=denslist$pdens,
                  sodens=denslist$sodens,sddens=denslist$sddens,
                  sopratio=denslist$sopratio,
                  adultsurfarea=adultsurfarea,
                  adultdens=adultdenslist$adultdens,
                  adultpdens=adultdenslist$adultpdens,
                  adultsodens=adultdenslist$adultsodens,
                  adultsddens=adultdenslist$adultsddens,
                  adultsopratio=adultdenslist$adultsopratio,
                  pendtime=pendtime,soendtime=soendtime,
                  sdendtime=sdendtime, pinterval=pinterval,
                  sointerval=sointerval, sdinterval=sdinterval,
                  param=param)
  return(outlist)
}
