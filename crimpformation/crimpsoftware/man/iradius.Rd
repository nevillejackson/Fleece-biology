\name{iradius}
\alias{iradius}
\title{
Intrinsic radius of fibre curvature
}
\description{
Computes intrinsic radius of fibre curvature from wavelength and amplitude of crimp for unfolded helix crimp type only
}
\usage{
iradius(lambda, h)
}
\arguments{
  \item{lambda}{
wavelength of fibre crimp in mm
}
  \item{h}{
amplitude of fibre crimp in mm
}
}
\details{
Because crimp is a 3D phenomenon wavelength and amplitude viewed in 2D can be used to calculate intrinsic radius of curvature of a short snippet of fibre viewed in its plane of maximum curvature. Defined in this way, intrinsic radius of curvature is constant even though it appears to vary when the whole fibre is viewed in 2D.
}
\value{
Vector containing radius in mm
}
\references{
}
\author{
Neville Jackson
}
\note{
}


\seealso{
Functions unfold(() and unfoldt()
}
\examples{
}
\keyword{ misc }
