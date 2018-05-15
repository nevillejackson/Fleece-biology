\name{base.param}
\alias{base.param}
\alias{fine.param}
\docType{data}
\title{
Base parameter dataset
}
\description{
A list item containing values for each of the 20 parameters, as required for function \code{folli()}. This is just a starting set of parameter values which will produce a feasable result with function \code{folli()}.
}
\usage{
data(base.param)
data(fine.param)
}
\format{
List of 15
\describe{
 \item{$ growth}{List of 6
  \describe{
    \item{$ lowasymp}{ num 0}
    \item{$ upasymp}{ num 45000}
    \item{$ rate}{ num 0.028}
    \item{$ maxpos}{ num 1}
    \item{$ wtatmax}{ num 1000 }
    \item{$ c}{ num 1}
  }
 }
 \item{$ surfareaconst}{ num 9}
 \item{$ ztime}{ num 60}
 \item{$ maxtime}{ num 900}
 \item{$ timeincrement}{ num 1}
 \item{$ psiteno}{ num 2e+06}
 \item{$ sopratio}{ num 3}
 \item{$ follinitrate}{ num 450}
 \item{$ pstarttime}{ num 64}
 \item{$ sostarttime}{ num 86}
 \item{$ zcellno}{num 1e+08}
 \item{$ pavecellno}{ num 65}
 \item{$ soavecellno}{ num 59}
 \item{$ sdavecellno}{ num 53}
 \item{$ cellbirthprob}{ num 0.0764}
}
}

\details{
The alternative set of parameters \code{fine.param} is the same except that the numbers of papilla cells per follicle are les, resulting in smaller follicles and finer fibre diameters.
}

\examples{
data(base.param)
folli(base.param)
}
\keyword{datasets}
