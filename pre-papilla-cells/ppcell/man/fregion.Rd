\name{fregion}
\alias{fregion}
\alias{as.plist}
\alias{make.levels}
\title{
Generate results from routine \code{folli()} for parameter combinations.
}
\description{
To search for feasable regions of the parameter space need to generate results from routine \code{folli()} for a range of combinations of levels of several paramaters. The routine \code{fregion()} facilitates this. 
}
\usage{
fregion(base.param)
as.plist(v,base.param)
make.levels(base.param, nlevels)
}
\arguments{
  \item{base.param}{
The base parameters dataset as found in file \code{base.param.rda}
}
  \item{v}{
A vector of parameters to be turned into  a list item like \code{base.param}
}
  \item{nlevels}{
Number of levels to use for each parameter varied
}
}
\details{
Routine \code{fregion()} calls \code{as.plist()} and \code{make.levels()}. Some editing of these R files is needed to vary the parameters studied and the levels set for each. The procedure is not totally automated.
}
\value{
A list containing the following:
\item{p}{the levels set for each parameter varied}
\item{apset}{array of parameter levels for all combinations}
\item{adens}{ array of calculated adult follicle densities for all combinations}
}
\author{
Neville Jackson
}

\seealso{
Function \code{folli()}
}
\examples{
}
\keyword{ misc }
