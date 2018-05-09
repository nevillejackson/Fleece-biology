\name{plottptso}
\alias{plottptso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plottptso(param, which = c("pstarttime", "sostarttime"), values = seq(63, 65, 0.1), other = "cellbirthprob", ovalues = c(0.07, 0.073, 0.076, 0.0764), out = "dens", myxlab = which)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{
%%     ~~Describe \code{param} here~~
}
  \item{which}{
%%     ~~Describe \code{which} here~~
}
  \item{values}{
%%     ~~Describe \code{values} here~~
}
  \item{other}{
%%     ~~Describe \code{other} here~~
}
  \item{ovalues}{
%%     ~~Describe \code{ovalues} here~~
}
  \item{out}{
%%     ~~Describe \code{out} here~~
}
  \item{myxlab}{
%%     ~~Describe \code{myxlab} here~~
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
function (param, which = c("pstarttime", "sostarttime"), values = seq(63, 
    65, 0.1), other = "cellbirthprob", ovalues = c(0.07, 0.073, 
    0.076, 0.0764), out = "dens", myxlab = which) 
{
    maxparam <- param
    maxparam[other] <- max(ovalues)
    maxtmp <- varytwopar(maxparam, which, values)
    minparam <- param
    minparam[other] <- min(ovalues)
    mintmp <- varytwopar(minparam, which, values)
    rangetmp <- maxtmp
    rangetmp$dens[1] <- min(mintmp$dens)
    rangetmp$sop[1] <- min(mintmp$sop)
    for (i in 1:length(ovalues)) {
        tmpparam <- param
        tmpparam[other] <- ovalues[i]
        tmp <- varytwopar(tmpparam, which, values)
        if (out == "dens") {
            if (i == 1) {
                plot(maxtmp$values, rangetmp$dens, xlab = myxlab, 
                  ylab = "Adult density ( no per mm^2)", type = "n")
            }
            points(tmp$values, tmp$dens, pch = i, col = i + 1)
        }
        else {
            if (i == 1) {
                plot(maxtmp$values, rangetmp$sop, xlab = myxlab, 
                  ylab = "Adult S/P ratio", type = "n")
            }
            points(tmp$values, tmp$sop, pch = i, col = i + 1)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
