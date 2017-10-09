\name{unfoldt}
\alias{unfoldt}
\title{
Calculate intrinsic radius of curvature, fibre length and Lf/Ls ratio for staple
s with unfolded helix crimp type
}
\description{
Calculate intrinsic radius of curvature, fibre length and Lf/Ls ratio, from wave
length, amplitude, staple length and number of crimps per staple, for staples with unfolded helix crimp type
}
\usage{
unfoldt(lambda, ampl, twistlen, twistrad, stalen, t)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda}{
wavelength of crimps in mm
}
  \item{ampl}{
amplitude of crimp waves in mm
}
  \item{twistlen}{
A constant which is multiplied by amplitude to estimated length of the twisted p
art of a fibre bundle.
}
  \item{twistrad}{
A constant equal to the radius of a twisted fibre bundle in mm.
}
  \item{stalen}{
staple length in mm
}
  \item{t}{
number of crimps per staple
}
}
\details{
Assumes fibre is an unfolded circular helix.
}
\value{
\item{radius}{intrinsic radius of curvature}
\item{unfang[1]}{angle between unfoldings in radians}
\item{unfang[2]}{angle between unfoldings in degrees}
\item{t}{number of crimps per staple}
\item{lb}{fibre bundle length}
\item{lbtols}{ratio of fibre bundle length to staple length}
\item{lf}{fibre length}
\item{lftols}{ratio of fibre length to staple length}
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
Function unfold()
}
\examples{
}
\keyword{ misc }
