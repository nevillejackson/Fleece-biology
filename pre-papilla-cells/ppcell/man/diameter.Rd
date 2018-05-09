\name{diameter}
\alias{diameter}
\title{
Calculate average fibre diameter from average dermal papilla cell number
}
\description{
Uses an empirical regression relationship to calculate average fibre diameter from average dermal papilla cell count.
}
\usage{
diameter(dpcellno)
}
\arguments{
  \item{dpcellno}{
Average dermal papilla cell count (number per follicle)
}
}
\details{
The regression relationship is
\eqn{
D = (C + 48.281)/5.678
}
}
\value{
Average fibre diameter (microns)
}
\author{
Neville Jackson
}
\examples{
}
\keyword{ misc }
