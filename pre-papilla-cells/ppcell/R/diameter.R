diameter <-
function(dpcellno)
# diameter()  -  diameter in microns from dpcellno
{
  diam <- (dpcellno + 48.281)/5.678
  return(diam)
}
