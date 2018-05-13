\name{genlogist}
\alias{genlogist}
\title{
Generalised logistic function or Richard's curve.
}
\description{
Calculates value of the generalised logistic function at time \code{t}, given 5 parameters. 
}
\usage{
genlogist(lowasymp, upasymp, rate, nu, yinflect, c, t)
}
\arguments{
  \item{lowasymp}{
Lower asymptote ( normally zero)
}
  \item{upasymp}{
Upper asymptote ( mature weight) in gm
}
  \item{rate}{
growth rate
}
  \item{nu}{
Controls near which asymptote the maximum growth rate occurs (nu > 0)
}
  \item{yinflect}{
Related to Y(0) here~~
}
  \item{c}{
typically = 1 here~~
}
  \item{t}{
vector of tim values at which to evaluate function
}
}
\details{
The Richards curve is a sigmoid growth curve. 
}
\value{
Array of weights, one for each time in vector \code{t}
}
\author{
Neville Jackson
}

\examples{
}
\keyword{ misc }
