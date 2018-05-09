\name{genlogist}
\alias{genlogist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
genlogist(lowasymp, upasymp, rate, nu, yinflect, c, t)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lowasymp}{
%%     ~~Describe \code{lowasymp} here~~
}
  \item{upasymp}{
%%     ~~Describe \code{upasymp} here~~
}
  \item{rate}{
%%     ~~Describe \code{rate} here~~
}
  \item{nu}{
%%     ~~Describe \code{nu} here~~
}
  \item{yinflect}{
%%     ~~Describe \code{yinflect} here~~
}
  \item{c}{
%%     ~~Describe \code{c} here~~
}
  \item{t}{
%%     ~~Describe \code{t} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (lowasymp, upasymp, rate, nu, yinflect, c, t) 
{
    y <- rep(0, length(t))
    for (i in 1:length(t)) {
        y[i] <- lowasymp + (upasymp - lowasymp)/((c + yinflect * 
            exp(-(rate * t[i])))^(1/nu))
    }
    return(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
