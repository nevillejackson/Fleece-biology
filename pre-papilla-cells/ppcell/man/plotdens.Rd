\name{plotdens}
\alias{plotdens}
\alias{plotfollno}
\title{
Plots follicle density ( follicle number) against time
}
\description{
Plots follicle densities for P, So, Sd and all follicles in no per mm^2 against time from zero days to maxtime. Alternative routine plots follicle numbers per head.
}
\usage{
plotdens(x, ymax = 300)
plotfollno(x, ymax=1.e+08)
}
\arguments{
  \item{x}{
An object produced by function \code{folli()}
}
  \item{ymax}{
Maximum Y value for plot control
}
}
\value{
Returns NULL. Function is used for its side effect which is a plot object.
}
\author{
Neville Jackson
}
\examples{
}
\keyword{ misc }
