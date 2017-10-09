\name{unfanlt180}
\alias{unfanlt180}
\title{
Calculate angle between unfoldings for a wool with unfolded helix crimp type
}
\description{
Calculate angle between unfoldings for a wool with unfolded helix crimp type, from intrinsic radius and wavelength.
}
\usage{
unfanlt180(a, lambda)
}
\arguments{
  \item{a}{
Vector of intrinsic radius values
}
  \item{lambda}{
Vector of wavelengths
}
}
\details{
This function applies when the angle between unfoldings in less than 180 degrees.
}
\value{
\item{theta[1]}{angle between unfoldings in radians}
\item{theta[2]}{angle between unfoldings in degrees}
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
Function unfangt180()
}
\examples{
}
\keyword{ misc }
