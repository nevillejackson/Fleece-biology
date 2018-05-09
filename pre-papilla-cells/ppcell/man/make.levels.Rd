\name{make.levels}
\alias{make.levels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make.levels(base.param, nlevels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base.param}{
%%     ~~Describe \code{base.param} here~~
}
  \item{nlevels}{
%%     ~~Describe \code{nlevels} here~~
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
function (base.param, nlevels) 
{
    p <- matrix(0, nlevels, 7)
    tmp <- base.param$psiteno
    p[, 1] <- c(tmp - 40000, tmp - 20000, tmp, tmp + 20000, tmp + 
        40000)
    tmp <- base.param$sopratio
    p[, 2] <- c(tmp - 0.8, tmp - 0.4, tmp, tmp + 0.4, tmp + 0.8)
    tmp <- base.param$follinitrate
    p[, 3] <- c(tmp - 100, tmp - 50, tmp, tmp + 50, tmp + 100)
    tmp <- base.param$pstarttime
    p[, 4] <- c(tmp - 3, tmp - 2, tmp, tmp + 2, tmp + 3)
    tmp <- base.param$sostarttime
    p[, 5] <- c(tmp - 4, tmp - 2, tmp, tmp + 2, tmp + 4)
    tmp <- base.param$zcellno
    p[, 6] <- c(tmp - 2e+07, tmp - 1e+07, tmp, tmp + 1e+07, tmp + 
        2e+07)
    tmp <- base.param$cellbirthprob
    p[, 7] <- c(tmp - 0.008, tmp - 0.004, tmp, tmp + 0.004, tmp + 
        0.008)
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
