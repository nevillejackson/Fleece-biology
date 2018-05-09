\name{adultdensity}
\alias{adultdensity}
\title{
Converts follicle numbers to densities given surfacearea of adult sheep.
}
\description{
Adult surfacearea is specified in $cm^{2}$ , but densities for P, So, Sd, and all follicles are in number per $mm^{2}$. Just the final densities for the adult animal are calculated.
}
\usage{
adultdensity(adultsurfarea, follno, pfollno, sofollno, sdfollno)
}
\arguments{
  \item{adultsurfarea}{
Adult surface area in $cm^{2}$
}
  \item{follno}{
Vector of follicle numbers from output of \code{folli()} routine. Only the last element is used.
}
  \item{pfollno}{
Vector of primary follicle numbers from output of \code{folli()} routine. Only the last element is used.
}
  \item{sofollno}{
Vector of So follicle numbers from output of \code{folli()} routine. Only the last element is used.
}
  \item{sdfollno}{
Vector of Sd follicle numbers from output of \code{folli()} routine. Only the last element is used.
}
}
\details{
Assumes growth curve calculation is carried on  by routine \code{folli()} until the last points are effectively on the asymptotic adult body weight. 
}
\value{
List item containing:
\item{adultdens}{Adult density of all folllicles as no per $mm^{2}$}
\item{adultpdens}{Adult primary density as no per $mm^{2}$}
\item{adultsodens}{Adult So density  as no per $mm^{2}$}
\item{adultsddens}{Adult Sd density  as no per $mm^{2}$}
\item{adultsopratio}{Adult S/P ratio }
}
\author{
Neville Jackson
}
\note{
Converting per head follicle  numbers to densities depends on assumptions about the relationship of body weight to surface area.
}


\seealso{
The function \code{density()}
}
\examples{

}
\keyword{ misc }
