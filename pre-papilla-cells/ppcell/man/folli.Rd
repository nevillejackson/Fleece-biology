\name{folli}
\alias{folli}
\title{
Model pre-papilla cell population growth and follicle formation
}
\description{
Models the dynamics of pre-papilla cell population numbers and the initiation of primary follicles, secondary original follicles  at sites in the d ermis. Allows secondary derived follicles to initate as branches from other foll icles. 
}
\usage{
folli(param)
}

\arguments{
  \item{param} list object containing containing values set for 20 parameters  as follows:
\describe{
\item{growth}{list object containing growth curve parameters as follows:}
  \describe{
  \item{lowasymp}{lower asymptote of growth curve (usually zero)}
  \item{upasymp}{upper asymptote of growth curve. This is adult weight in $gm$.}
  \item{rate}{logarithmic growth rate parameter}
  \item{maxpos}{controls age at maximum growth rate}
  \item{wtatmax}{controls weight at maximum growth rate}
  \item{c}{controls upper asymptote (usually 1).}
  }
\item{surfareaconst}{constant converting weight in gm to surface area in $cm^{2}$}
\item{ztime}{zero time for calculation. this is the day number at which initial pre-papilla cell number is specified}
\item{maxtime}{ending day number for calculations. should correspond to adult sheep}
\item{timeincrement}{time step size used in calculation. typically 1 day}
\item{psiteno}{number of primary follicle sites per head}
\item{sopratio}{number of So follicle sites per P follicle site}
\item{follinitrate}{follicle initiation rate, no per timeincrement per $cm^{2}$}
\item{pstarttime}{day no for start of P follicle initiation}
\item{sostarttime}{day no for start of So follicle initiation}

}
}

\details{
The function calculates cell numbers and follicle numbers at a range of times from \code{ztime} up to \code{maxtime} in steps of \code{timeincrement}.
}

\value{
A list object is returned containing the following items
\item{time}{time index vector in days}
\item{weight}{weight vector containing foetal and lamb weights at all times}
\item{surfarea}{surface area vector corresponding to weights}
\item{pfollno}{primary follicle number vector giving counts per head at all times}
\item{sofollno}{So follicle number vector giving counts per head at all times}
\item{sdfollno}{Sd follicle number vector giving counts per head at all times}
\item{diffcellno}{Vector of numbers of differentiated cells at all times}
\item{follno}{Vector of follicle numbers at all times}
\item{cellno}{Vector of numbers of undifferentiated cells at all times}
\item{dens}{follicle density vector, no per $mm^{2}$ at all times}
\item{pdens}{primary follicle density vector, no per $mm^{2}$ at all times}
\item{sodens}{So follicle density vector, no per $mm^{2}$ at all times}
\item{sddens}{Sd follicle density vector, no per $mm^{2}$ at all times}
\item{sopratio}{Vector of S/P ratios at all times}
\item{adultsurfarea}{Adult surface area $cm^{2}$}
\item{adultdens}{Adult density, no per $mm^{2}$}
\item{adultpdens}{Adult primary density, no per $mm^{2}$}
\item{adultsodens}{Adult So density, no per $mm^{2}$}
\item{adultsddens}{Adult Sd density, no per $mm^{2}$}
\item{adultsopratio}{Adult S/P ratio}
\item{pendtime}{Day no for last day of primary initiation}
\item{soendtime}{Day no for last day of So initiation}
\item{sdendtime}{Day no for last day of Sd initiation}
\item{pinterval}{Length of primary initiation period , days}
\item{sointerval}{Length of So initiation period , days}
\item{sdinterval}{Length of Sd initiation period , days}
\item{param}{List of parameters provided in call}
}
\references{
The document ppcell.pdf has a bibliography of literature references
}
\author{
Neville Jackson
}
\note{
\code{folli()(} will only calculate for one set of parameters at a time. See \code{fregion()} for an example of how to make multiple calls varying some parameters}
}


\seealso{
The functions \code{fregion()}, \code{density()}, \code{adultdensity()}, and \code{genlogist()}. 
The data \code{base.param}
}
\examples{
library(ppcell)
base.out <- folli(base.param)
}

\keyword{ misc }
