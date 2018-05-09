as.plist <-
function(v,base.param)
# as.plist()  -  turn a vector v into a param list for folli
{
  plist <- base.param
  plist$psiteno <- v[1]
  plist$sopratio <- v[2]
  plist$follinitrate <- v[3]
  plist$pstarttime <- v[4]
  plist$sostarttime <- v[5]
  plist$zcellno <- v[6]
  plist$cellbirthprob <- v[7]
  return(plist)
}
