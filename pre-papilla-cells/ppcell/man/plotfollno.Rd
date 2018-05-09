\name{plotfollno}
\alias{plotfollno}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotfollno(x, ymax = 1e+08)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{ymax}{
%%     ~~Describe \code{ymax} here~~
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
function (x, ymax = 1e+08) 
{
    par(lwd = 2, lty = 1)
    ydum <- x$follno
    ydum[length(x$follno)] <- ymax
    plot(x$time, ydum, xlab = "Days", ylab = "Number of follicles", 
        type = "n")
    points(x$time, x$pfollno, type = "p", col = 5)
    points(x$time, x$sofollno, type = "p", col = 2)
    points(x$time, x$sdfollno, type = "p", col = 3)
    points(x$time, x$follno, type = "l", col = 4)
    legend(mean(x$time), max(ydum) * 0.9, legend = c("P", "So", 
        "Sd", "All"), col = c(5, 2, 3, 4), lty = 1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
