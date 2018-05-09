\name{density}
\alias{density}
\title{
Converts follicle numbers to densities given surfacearea of  sheep.
}
\description{
Surface area is specified as a vector of areas ( in $cm^{2}$). Follicle numbers are vectors of counts per head for P, So, Sd, and all follicles.  Densities are
vectors of counts per $mm^{2}$. So this routine calculartes the whole array of d
ensities from day zero to adulthood.
}
\usage{
density(surfarea, time, follno, pfollno, sofollno, sdfollno)
}
\arguments{
  \item{surfarea}{
Vector of surface areas in $cm^{2}$
}
  \item{time}{
Vector of times.
}
  \item{follno}{
Vector of follicle numbers from output of \code{folli()} routine.
}
  \item{pfollno}{
Vector of primary follicle numbers from output of \code{folli()} routine.
}
  \item{sofollno}{
Vector of So follicle numbers from output of \code{folli()} routine.
}
  \item{sdfollno}{
Vector of Sd follicle numbers from output of \code{folli()} routine.
}
}
\details{
Assumes growth curve calculation is carried on  by routine \code{folli()} until 
the last points are effectively on the asymptotic adult body weight.
}
\value{
List item containing:
\item{dens}{Vector of follicle densities at all times}
\item{pdens}{Vector of primary follicle densities at all times}
\item{sodens}{Vector of So follicle densities at all times}
\item{sddens}{Vector of Sd follicle densities at all times}
\item{sopratio}{Vector of S/P ratios at all times}
}
\author{
Neville Jackson
}
\note{
Converting per head follicle  numbers to densities depends on assumptions about 
the relationship of body weight to surface area.
}
\seealso{
The function \code{density()}
}

\examples{
}
\keyword{ misc }
