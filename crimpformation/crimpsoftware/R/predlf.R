predlf <-
function(lambda,ampl,type,stalen,twistlen=0.5,twistrad=0.10)
# predlf()  - predict fibre length, and Lf/Ls ratio for all crimp types
#             for a matrix of values of wavelength (lambda) - nsheep x nmeas
#             and matrix of amplitudes (ampl) - nsheep x nmeas
#             crimp type (type) must be specified for each sheep
#             staple length (stalen) must be specified in mm for each sheep
#	      twistlen is a constant multiplier of amplitude 
#             twistrad is a constant - twisted bundle radius in mm
{
  outlist <- vector("list",20)
  nsheep <- nrow(lambda)
  nmeas <- ncol(lambda)
  radius <- matrix(0,nsheep,nmeas)
  unfangrad <- matrix(0,nsheep,nmeas)
  unfangdeg <- matrix(0,nsheep,nmeas)
  lf <- matrix(0,nsheep,nmeas)
  lftols <- matrix(0,nsheep,nmeas)
  lb <- matrix(0,nsheep,nmeas)
  lbtols <- matrix(0,nsheep,nmeas)

# do all individual measurement predictions
  for(i in 1:nsheep){
    if(type[i] == "unfolded") {
      for(j in 1:nmeas){
        tmp <- unfold(lambda[i,j],ampl[i,j],ampl[i,j]*twistlen,twistrad,stalen[i])
        radius[i,j] <- tmp$radius
        unfangrad[i,j] <- tmp$unfang[1]
        unfangdeg[i,j] <- tmp$unfang[2]
        lf[i,j] <- tmp$lf
        lftols[i,j] <- tmp$lftols
        lb[i,j] <- tmp$lb
        lbtols[i,j] <- tmp$lbtols
      }  #  end j
    }
    else if (type[i] == "stretched") {
      for(j in 1:nmeas){
        tmp <- stretch(lambda[i,j],ampl[i,j],stalen[i])
        radius[i,j] <- tmp$radius
        unfangrad[i,j] <- NA
        unfangdeg[i,j] <- NA
        lf[i,j] <- tmp$lf
        lftols[i,j] <- tmp$lftols
        lb[i,j] <- NA
        lbtols[i,j] <- NA
      }  # end j
    }
    else if (type[i] == "unaligned") {
      for(j in 1:nmeas){
        tmp <- stretch(lambda[i,j],ampl[i,j],stalen[i])
        radius[i,j] <- tmp$radius
        unfangrad[i,j] <- NA
        unfangdeg[i,j] <- NA
        lf[i,j] <- tmp$lf
        lftols[i,j] <- tmp$lftols
        lb[i,j] <- NA
        lbtols[i,j] <- NA
      }  # end j
    }
    else {
      stop("Invalid crimp type \n")
    }
  }  # end i

# average the individual measurement predictions
  for(i in 1:nsheep){
    meanradius <- apply(radius,1,mean)
    meanunfangrad <- apply(unfangrad,1,mean)
    meanunfangdeg <- apply(unfangdeg,1,mean)
    meanlf <- apply(lf,1,mean)
    meanlftols <- apply(lftols,1,mean)
    meanlb <- apply(lb,1,mean)
    meanlbtols <- apply(lbtols,1,mean)
  }  # end i

# setup outlist
   outlist <- list(Radius=radius,Unfangrad=unfangrad,Unfangdeg=unfangdeg,Lf=lf,LftoLs=lftols,Lb=lb,LbtoLs=lbtols,MeanRadius=meanradius,MeanUnfangrad=meanunfangrad,MeanUnfangdeg=meanunfangdeg,MeanLf=meanlf,MeanLftoLs=meanlftols,MeanLb=meanlb,MeanLbtoLs=meanlbtols,Lambda=lambda,Ampl=ampl,CrimpType=type,Stalen=stalen,Twistlen=twistlen,Twistrad=twistrad)
  return(outlist)
}
