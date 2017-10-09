\name{stretcht}
\alias{stretcht}
\title{
Calculate intrinsic radius of curvature, fibre length and Lf/Ls ratio for staples with stretched helix crimp type
}
\description{
Calculate intrinsic radius of curvature, fibre length and Lf/Ls ratio, from wavelength, amplitude, no of crimps per sstaple, and staple length, for staples with stretched helix crimp type
}
\usage{
stretcht(lambda, ampl, stalen, t)
}
\arguments{
  \item{lambda}{
wavelength of crimps in mm
}
  \item{ampl}{
amplitude of crimp waves in mm
}
  \item{stalen}{
staple length in mm
}
  \item{t}{
number of crimps per staple
}
}
\details{
Assumes fibre is a stretched circular helix.
}
\value{
\item{radius}{Intrinsic radius of helix before stretching in mm}
\item{t}{Number of crimps per staple}
\item{lf}{Fibre length in mm}
\item{lftols}{Fibre length to staple length ratio}
}
\references{
Jackson and Watts(2016) "Staple crimp formation in the fleece of Merino sheep" \
url{https://githib.com/nevillejackson/Fleece-biology/tree/master/crimpformation/
crimp.pdf}
}
\author{
Neville Jackson
}
\note{
}

\seealso{
Function stretch()
}
\examples{
}
\keyword{ misc }
