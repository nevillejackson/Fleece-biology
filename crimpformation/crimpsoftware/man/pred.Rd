\name{pred}
\alias{pred}
\title{
Predict fibre length and fibre length/staple length ratio
}
\description{
Predicts fibre length in mm and fibre length/staple length ratio, given crimp type, wavelength and amplitude, and staple length in mm.
}
\usage{
pred(lambda, ampl, type, stalen, crimpfreq = NULL, twistlen = 0.5, twistrad = 0.1)
}
\arguments{
  \item{lambda}{
Matrix of wavelengths in mm. Matrix has one row per sheep and one column per measurement and thus allows multiple measures of wavelength along the staple or fibre.
}
  \item{ampl}{
Matrix of amplitudes in mm. Matrix has one row per sheep and one column per measurement, ans for wavelength.
}
  \item{type}{
Character vector of crimp types , one per sheep. Possible values of crimp type are "unfolded", "stretched", and "unaligned"
}
  \item{stalen}{
Vector of staple lengths in mm, one per sheep.
}
  \item{crimpfreq}{
Vector of staple crimp frequencies in crimps per cm, one per sheep
}
  \item{twistlen}{
A constant which is multiplied by amplitude to estimated length of the twisted part of a fibre bundle. Applies only to "unfolded" crimp type.
}
  \item{twistrad}{
A constant equal to the radius of a twisted fibre bundle in mm. Applies only to "unfolded" crimp type
}
}
\details{
The function does predictions for all individual measurements of wavelengtth and amplitude, then pools them to derive a prediction for each sheep.

The calculation of no of crimps per staple is done from either crimp frequency or wavelength measurements and staple length and is done before calling  functions unfoldt() and stretcht().  If one needs crimps per staple to be calculated from crimp frequency and staple length one should supply a non-NULL value for the crimp frequency argument. If the crimp frequency argument is NULL the calculation of no of crimps per staple is done from wavelength measurements.
}
\value{
\item{Radius}{predicted radius for each measurement}
\item{Unfangrad}{predicted angle between unfoldings for each measurement in radians. "Unfolded" crimp type omly}
\item{Unfangdeg}{predicted angle between unfoldings for each measurement in degrees. "Unfolded" crimp type omly}
\item{Lf}{fibre length prediction in mm for each measurement}
\item{LftoLs}{predicted fibre length to staple length ratio for each measurement}
\item{Lb}{fibre bundle length prediction in mm for each measurement. "Unfolded" crimp type only}
\item{LbtoLs}{predicted fibre bundle length to staple length ratio for each measurement. "Unfolded" crimp type only}
\item{MeanRadius}{predicted radius in mm for each sheep}
\item{MeanUnfangrad}{predicted angle between unfoldings for each sheep in radians. "Unfolded" crimp type only}
\item{MeanUnfangdeg}{predicted angle between unfoldings for each sheep in degrees. "Unfolded" crimp type only}
\item{MeanLf}{fibre length prediction in mm for each sheep}
\item{MeanLftoLs}{predicted fibre length to staple length ratio for each sheep}
\item{MeanLb}{fibre bundle lengtth prediction in mm for each sheep. "Unfolded" crimp type only}
\item{MeanLbtoLs}{predicted fibre bundle length to staple length ratio for each sheep. "Unfolded" crimp type only}
\item{Lambda}{wavelength argument}
\item{Ampl}{amplitude argument}
\item{CrimpType}{crimp type argument}
\item{Stalen}{staple length argument}
\item{Twistlen}{twistlen argument}
\item{Twistrad}{twistrad argument}
}
\references{
Jackson and Watts(2016) "Staple crimp formation in the fleece of Merino sheep" \url{https://githib.com/nevillejackson/Fleece-biology/tree/master/crimpformation/crimp.pdf}
}
\author{
Neville Jackson
}
\note{
}

\seealso{
Function predlf()
}
\examples{
}
\keyword{ misc }
