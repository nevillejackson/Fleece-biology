\name{plotonepar}
\alias{plotonepar}
\alias{plotoneparplus}
\alias{plotthreepar}
\alias{plottptso}
\title{
Plot various graphs showing effect of one parameter on adult density, at set levels of a second parameter.
}
\description{
Plot adult density against one parameter with separate lines ( point symbols) for levels of a second parameter, which is usually the cell birth probability.
}
\usage{
plotonepar(param, which, values, out = "dens")
plotoneparplus(param,which,values,other="cellbirthprob",
      ovalues=c(.070,.073,.076,.0764),out="dens",myxlab=which)
plotthreepar(param,which = c("pavecellno","soavecellno","sdavecellno"),
      values = seq( 30,70,1),other="cellbirthprob",
      ovalues=c(.070,.073,.076,.0764),out="dens",myxlab=which)
plottptso(param,which = c("pstarttime","sostarttime"),
      values = seq(63,65,0.1),other="cellbirthprob",
      ovalues=c(.070,.073,.076,.0764),out="dens",myxlab=which)
} 
\arguments{
  \item{param}{
List item of parameters as described for function \code{folli()}
}
  \item{which}{
Name of parameter which varies. For some functions this is a vector of parameter names.
}
  \item{values}{
Vector of values for parameter which varies
}
  \item{other}{
Name of parameter which is plotted as separate lines
}
  \item{ovalues}{
Vector of values for \code{other} argument
}
  \item{out}{
What to put on Y axis. Default is \code{"dens"} which means a density plot. Other option is \code{"sop"} which means plot S/P ratio instead of density.
}
  \item{myxlab}{
Label for X axis
}
}
\value{
Nothing is returned. These functions are for their plot side effect.
}
\author{
Neville Jackson
}
\examples{
}
\keyword{ misc1 }
